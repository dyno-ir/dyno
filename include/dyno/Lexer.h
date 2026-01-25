#pragma once

#include "dyno/IDImpl.h"
#include "dyno/Opcode.h"
#include "support/Lexer.h"
#include "support/VectorLUT.h"

namespace dyno {
class DynoLexer;

template <typename SymbolT> class SymbolResolver {
  VectorLUT<SymbolT> lut;

public:
  void registerOpcodes(DynoLexer &lexer, DialectID dialectID);
  void registerTypes(DynoLexer &lexer, TyID dialectID);

  std::optional<SymbolT> resolve(unsigned ident) {
    auto &val = lut.find(ident);
    if (!val)
      return std::nullopt;
    return *val;
  }
};

class DynoLexer : public Lexer<false, false, true, true> {
public:
  constexpr static std::array<const char *, 0> Keywords;
  constexpr static auto Operators = std::to_array(
      {".", ":", ",", "[", "]", "?", "#", "(", ")", "{", "}", ";"});
  enum OperatorEnum {
    _op_start = Lexer::TOK_OPS_START - 1,
    op_dot,
    op_colon,
    op_comma,
    op_abropen,
    op_abrclose,
    op_qmark,
    op_hash,
    op_rbropen,
    op_rbrclose,
    op_cbropen,
    op_cbrclose,
    op_semicolon
  };

  DialectInfos &infos;

  VectorLUT<uint8_t> dialectLUT;

  // these are used for unqualified names. Conflicting names are not resolvable
  // without explicit qualification ()
  SymbolResolver<DialectOpcode> genericOpcResolver;
  SymbolResolver<DialectType> genericTypeResolver;

  // these are used with qualified names.
  Interfaces<NUM_DIALECTS, SymbolResolver<DialectType>,
             SymbolResolver<DialectOpcode>>
      dialectResolvers;

  void registerDialects() {
    for (auto [dialectIdx, dialect] : Range{infos.dialectInfoArr}.enumerate()) {
      if (!dialect)
        continue;
      auto id = DialectID{DialectID::num_t(dialectIdx)};
      dialectLUT.insert(GetIdentIdx(dialect->name), dialectIdx);
      genericOpcResolver.registerOpcodes(*this, id);
      genericTypeResolver.registerTypes(*this, id);
      auto &specificOpc =
          dialectResolvers.getVal<SymbolResolver<DialectOpcode>>(dialectIdx);
      specificOpc.registerOpcodes(*this, id);
      auto &specificTy =
          dialectResolvers.getVal<SymbolResolver<DialectType>>(dialectIdx);
      specificTy.registerTypes(*this, id);
    }
  }

public:
  DynoLexer(DialectInfos &infos, ArrayRef<char> src, std::string &&fileName)
      : Lexer(src, std::move(fileName), Operators, Keywords), infos(infos) {
    registerDialects();
    this->config.numericParseType = Lexer::Config::NumericParseType::DYNO;
  }

  std::optional<DialectID> tryPopDialect() {
    auto ident = Peek();
    if (ident.type != Token::IDENTIFIER)
      return std::nullopt;
    if (auto &entry = dialectLUT.find(ident.ident.idx)) {
      Pop();
      return DialectID{entry.value()};
    }
    return std::nullopt;
  }

  template <typename T> std::optional<T> tryPopQualified() {
    auto state = getState();
    if (auto dialect = tryPopDialect()) {
      if (auto tok = tryPopEnsure(op_dot); !tok) {
        restoreState(state);
        return std::nullopt;
      }

      auto ident = tryPeekEnsure(Token::IDENTIFIER);
      if (!ident) {
        restoreState(state);
        return std::nullopt;
      }
      auto &resolver = dialectResolvers.getVal<SymbolResolver<T>>(dialect->num);
      auto entry = resolver.resolve(ident->ident.idx);
      if (!entry) {
        restoreState(state);
        return std::nullopt;
      }
      Pop();
      return *entry;
    }
    return std::nullopt;
  }

  std::optional<DialectOpcode> tryPopOpcode() {
    if (auto opc = tryPopQualified<DialectOpcode>())
      return *opc;
    auto ident = Peek();
    if (ident.type != Token::IDENTIFIER)
      return std::nullopt;
    auto entry = genericOpcResolver.resolve(ident.ident.idx);
    if (!entry)
      return std::nullopt;
    Pop();
    return *entry;
  }

  std::optional<DialectType> tryPopType() {
    if (auto opc = tryPopQualified<DialectType>())
      return *opc;
    auto ident = Peek();
    if (ident.type != Token::IDENTIFIER)
      return std::nullopt;
    auto entry = genericTypeResolver.resolve(ident.ident.idx);
    if (!entry)
      return std::nullopt;
    Pop();
    return *entry;
  }

  std::expected<DialectOpcode, ParseError> popOpcode() {
    if (auto opc = tryPopOpcode())
      return *opc;
    return std::unexpected{makeErrorOnPeekToken("invalid opcode")};
  }

  std::expected<DialectType, ParseError> popType() {
    if (auto type = tryPopType())
      return *type;
    return std::unexpected{makeErrorOnPeekToken("invalid type")};
  }

  std::optional<DialectOpcode> peekOpcode() {
    auto state = getState();
    auto opc = popOpcode();
    if (opc) {
      restoreState(state);
      return *opc;
    }
    return std::nullopt;
  }

  std::optional<DialectType> peekType() {
    auto state = getState();
    auto type = popType();
    if (type) {
      restoreState(state);
      return *type;
    }
    return std::nullopt;
  }
};

template <typename T>
inline void SymbolResolver<T>::registerOpcodes(DynoLexer &lexer,
                                               DialectID dialectID) {
  auto dialectIdx = dialectID.num;
  auto opcodes = lexer.infos.opcodeInfoArr[dialectIdx];
  for (auto [opcodeIdx, opcode] : Range{opcodes}.enumerate()) {
    auto identIdx = lexer.GetIdentIdx(opcode.name);
    auto &entry = lut.find(identIdx);
    if (entry) {
      entry = DialectOpcode{DialectID::invalid(), OpcodeID::invalid() - 1};
    } else {
      entry = DialectOpcode{DialectID::num_t(dialectIdx),
                            OpcodeID::num_t(opcodeIdx)};
    }
  }
}

template <typename T>
inline void SymbolResolver<T>::registerTypes(DynoLexer &lexer, TyID dialectID) {
  auto dialectIdx = dialectID.num;
  auto types = lexer.infos.typeInfoArr[dialectIdx];
  for (auto [typeIdx, type] : Range{types}.enumerate()) {
    TyID tyId{TyID::num_t(typeIdx | (type.isDefUse ? TY_DEF_USE_START : 0))};

    auto identIdx = lexer.GetIdentIdx(type.name);
    auto &entry = lut.find(identIdx);
    if (entry) {
      entry = DialectType{DialectID::invalid(), TyID::invalid() - 1};
    } else {
      entry = DialectType{DialectID::num_t(dialectIdx), tyId};
    }
  }
}
}; // namespace dyno
