#pragma once
#include "dyno/DialectInfo.h"
#include "dyno/IDImpl.h"
#include "dyno/Instr.h"
#include "dyno/Interface.h"
#include "dyno/Obj.h"
#include "hw/DebugInfo.h"
#include "hw/HWPrinter.h"
#include "hw/passes/ParseLiberty.h"
#include "support/ErrorRecovery.h"
#include "support/Lexer.h"
#include "support/SmallVec.h"
#include "support/TempBind.h"
#include "support/Tokenizer.h"
#include "support/Utility.h"
#include "support/VectorLUT.h"
#include <charconv>
#include <string>

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
  constexpr static auto Operators =
      std::to_array({".", ":", ",", "[", "]", "?", "#", "(", ")", "{", "}"});
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
    op_cbrclose
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

template <typename Derived> class Parser {
protected:
  TempBindVal<DynoLexer> lexer;
  VectorLUT<FatDynObjRef<>> identMap;

  using obj_parse_fn = FatDynObjRef<> (Parser::*)(DialectType type,
                                                  ArrayRef<char> name);
  Interfaces<NUM_DIALECTS, obj_parse_fn> interfaces;
  SourceLocInfo<Instr> *sourceLocInfo = nullptr;

  struct ParseOperand {
    FatDynObjRef<> ref;
    bool isDef;
  };

private:
  auto &getInstrs() { return static_cast<Derived *>(this)->getInstrs(); }
  auto &getConstants() { return static_cast<Derived *>(this)->getConstants(); }
  auto &getCFG() { return static_cast<Derived *>(this)->getCFG(); }

protected:
  FatDynObjRef<> parseObject(ArrayRef<char> name) {
    auto type = lexer->popType();
    auto fn = interfaces.template getVal<obj_parse_fn>(type.getDialectID());

    auto ref = (this->*fn)(type, std::string_view{name});
    if (!ref)
      lexer->printErrorOnPeekToken("invalid object");
    return ref;
  }

  ParseOperand parseConstantOperand() {
    lexer->popEnsure(DynoLexer::op_hash);
    auto litTok = lexer->popEnsure(Token::BIG_INT_LITERAL);
    return ParseOperand{getConstants().findOrInsert(*litTok.bigIntLit.value),
                        false};
  }

  ParseOperand parseObjectOperand() {
    auto ident = lexer->popEnsure(Token::IDENTIFIER);

    bool isDef = false;
    FatDynObjRef<> obj = nullref;

    auto ref = identMap.find(ident.ident.idx);
    auto identStr = lexer->GetIdent(ident.ident.idx);
    if (identStr[0] != '%')
      report_fatal_error("identifiers need to start with %");
    identStr = identStr.substr(1);

    if (lexer->popIf(DynoLexer::op_colon)) {
      isDef = !lexer->popIf(DynoLexer::op_qmark);
      obj = parseObject(ArrayRef{identStr});
      identMap.insert(ident.ident.idx, FatDynObjRef{obj});
    } else {
      if (!ref)
        lexer->printErrorOnPeekToken("undefined value");
      obj = *ref;
    }

    return ParseOperand{obj, isDef};
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

  void parseBlockContents(BlockRef block) {
    lexer->popEnsure(DynoLexer::op_cbropen);
    while (!lexer->popIf(DynoLexer::op_cbrclose)) {
      auto instr = parseInstr();
      block.end().insertPrev(instr);
    }
  }

  void parseSourceLoc(InstrRef instr) {
    Token tok = lexer->popEnsure(Token::STRING_LITERAL);
    if (!sourceLocInfo)
      return;
    auto pos = tok.strLit.value.find_first_of(':');
    auto file = tok.strLit.value.substr(0, pos);
    auto lines = tok.strLit.value.substr(pos);
    auto linesSplit = Tokenizer{lines, ".-:"};
    SmallVec<uint32_t, 4> lineNums;
    lineNums.push_back_range(
        Range{linesSplit}.transform([](size_t, std::string_view view) {
          uint32_t val;
          auto res = std::from_chars(view.begin(), view.end(), val);
          if (res.ec != std::errc())
            report_fatal_error("expected line number");
          return val;
        }));
    uint32_t startLine, startCol, endLine, endCol;

    // ignore actual syntax, just assume
    // 1: line
    // 2: line + col
    // 3: line, start col, end col
    // 4: all
    auto sz = lineNums.size();
    lineNums.resize(4);
    switch (sz) {
    case 1:
      lineNums[2] = lineNums[0];
      lineNums[1] = 0;
      lineNums[2] = 0;
      break;
    case 2:
      lineNums[2] = lineNums[0];
      lineNums[3] = lineNums[1];
      break;
    case 3:
      // make end col num
      lineNums[3] = lineNums[2];
      // all on one line
      lineNums[2] = lineNums[0];
      break;
    case 4:
      break;
    default:
      report_fatal_error("invalid line numbers");
    }

    sourceLocInfo->addSrcLoc(instr, file, lineNums[0], lineNums[1], lineNums[2],
                          lineNums[3]);
  }

  InstrRef parseInstr() {
    auto opc = lexer->popOpcode();

    SmallVec<FatDynObjRef<>, 16> operands;
    uint numDefs = 0;

    SmallVec<BlockRef, 4> defBlocks;

    while (lexer->peekIs(DynoLexer::op_hash, Token::IDENTIFIER)) {
      auto op = parseOperand();
      if (op.isDef) {
        if (numDefs != operands.size())
          lexer->printErrorOnPeekToken("invalid def operand position (all def "
                                       "operands must be leading)");
        numDefs++;

        if (auto asBlock = op.ref.template dyn_as<BlockRef>())
          defBlocks.emplace_back(asBlock);
      }
      operands.emplace_back(op.ref);

      if (!lexer->popIf(DynoLexer::op_comma))
        break;
    }

    for (auto block : defBlocks)
      parseBlockContents(block);

    auto instr = getInstrs().create(operands.size(), opc);
    auto ib = InstrBuilder{instr};

    ib.addRefs(Range{operands.begin(), operands.begin() + numDefs});
    ib.other();
    ib.addRefs(Range{operands.begin() + numDefs, operands.end()});

    if (lexer->popIf(DynoLexer::op_abropen)) {
      while (lexer->peekIs(Token::STRING_LITERAL)) {
        parseSourceLoc(instr);
        if (!lexer->popIf(DynoLexer::op_comma))
          break;
      }
      lexer->popEnsure(DynoLexer::op_abrclose);
    }

    return instr;
  }

public:
  void parse(ArrayRef<char> src, std::string fileName) {
    auto val = lexer.emplace(src, fileName);

    while (!lexer->peekIs(Token::NONE)) {
      parseInstr();
    }
  }

  Parser() { interfaces.registerVal(DIALECT_CORE, &Parser::parseCore); }

  FatDynObjRef<> parseCore(DialectType type, ArrayRef<char> name) {
    assert(type.dialect == DIALECT_CORE);
    if (type == CORE_BLOCK) {
      auto block = getCFG().blocks.create(getCFG());
      return block;
    }

    return nullref;
  }
};

}; // namespace dyno
