#pragma once

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

class DynoLexer : public Lexer<false, false, false, true, true> {
public:
  constexpr static std::array<const char *, 0> Keywords;
  constexpr static auto Operators =
      std::to_array({".", "%", ":", ",", "[", "]", "?", "#"});
  enum OperatorEnum {
    _op_start = Lexer::TOK_OPS_START - 1,
    op_dot,
    op_percent,
    op_colon,
    op_comma,
    op_abropen,
    op_abrclose,
    op_qmark,
    op_hash
  };

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
    for (auto [dialectIdx, dialect] : Range{HWPrinter::dialectIs}.enumerate()) {
      auto id = DialectID{DialectID::num_t(dialectIdx)};
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
  DynoLexer(ArrayRef<char> src, std::string &&fileName)
      : Lexer(src, std::move(fileName), Operators, Keywords) {
    registerDialects();
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
    if (auto dialect = tryPopDialect()) {
      popEnsure(op_dot);
      auto ident = peekEnsure(Token::IDENTIFIER);
      auto &resolver = dialectResolvers.getVal<SymbolResolver<T>>(dialect->num);
      auto entry = resolver.resolve(ident.ident.idx);
      if (!entry)
        printErrorOnPeekToken("undefined opcode/type");
      Pop();
      return *entry;
    }
    return std::nullopt;
  }

  DialectOpcode popOpcode() {
    if (auto opc = tryPopQualified<DialectOpcode>())
      return *opc;
    auto ident = peekEnsure(Token::IDENTIFIER);
    auto entry = genericOpcResolver.resolve(ident.ident.idx);
    if (!entry)
      printErrorOnPeekToken("invalid opcode");
    Pop();
    return *entry;
  }

  DialectType popType() {
    if (auto ty = tryPopQualified<DialectType>())
      return *ty;
    auto ident = peekEnsure(Token::IDENTIFIER);
    auto entry = genericTypeResolver.resolve(ident.ident.idx);
    if (!entry)
      printErrorOnPeekToken("invalid type");
    Pop();
    return *entry;
  }
};

template <typename T>
inline void SymbolResolver<T>::registerOpcodes(DynoLexer &lexer,
                                               DialectID dialectID) {
  auto dialectIdx = dialectID.num;
  auto opcodes = HWPrinter::opcodeInfoArrays[dialectIdx];
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
  auto types = HWPrinter::typeInfoArrays[dialectIdx];
  for (auto [typeIdx, type] : Range{types}.enumerate()) {
    auto identIdx = lexer.GetIdentIdx(type.name);
    auto &entry = lut.find(identIdx);
    if (entry) {
      entry = DialectType{DialectID::invalid(), TyID::invalid() - 1};
    } else {
      entry = DialectType{DialectID::num_t(dialectIdx), TyID::num_t(typeIdx)};
    }
  }
}

class Parser {
  TempBindVal<DynoLexer> lexer;
  VectorLUT<FatDynObjRef<>> identMap;

  struct ParseOperand {
    FatDynObjRef<> ref;
    bool isDef;
  };

  ParseOperand parseConstantOperand() {
    assert(lexer->Pop().type == DynoLexer::op_hash);
    auto litTok = lexer->peekEnsure(Token::NUMERIC_LITERAL);
    auto ptr = litTok.numericLit.value.begin();
    auto res = BigInt::parseDyno(ptr, litTok.numericLit.value.end());
    if (!res || ptr != litTok.numericLit.value.end())
      lexer->printErrorOnPeekToken("ill-formed numeric literal");
    lexer->Pop();
    return ParseOperand{ctx.getConstants().findOrInsert(*res), false};
  }

  ParseOperand parseObjectOperand() {
    auto ident = lexer->Pop();
    assert(ident.type == Token::IDENTIFIER);
    bool isDef = false;

    if (lexer->popIf(DynoLexer::op_colon)) {
      isDef = !lexer->popIf(DynoLexer::op_qmark);
    }
  }

  ParseOperand parseOperand() {
    auto tok = lexer->Peek();
    if (tok.type == DynoLexer::op_hash)
      return parseConstantOperand();

    if (tok.type == Token::IDENTIFIER)
      return parseObjectOperand();

    lexer->printErrorOnPeekToken(
        "invalid operand (expected constant or identifier)");
  }

  void parseInstr(BlockRef insertBlock) {
    auto opc = lexer->popOpcode();

    while (lexer->peekIs(DynoLexer::op_hash, DynoLexer::op_percent)) {
    }
  }
};

}; // namespace dyno
