#pragma once
#include "dyno/CFG.h"
#include "dyno/DialectInfo.h"
#include "dyno/IDImpl.h"
#include "dyno/Instr.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Interface.h"
#include "dyno/Obj.h"
#include "hw/DebugInfo.h"
#include "support/ErrorRecovery.h"
#include "support/Lexer.h"
#include "support/SmallVec.h"
#include "support/TempBind.h"
#include "support/TemplateUtil.h"
#include "support/Tokenizer.h"
#include "support/Utility.h"
#include "support/VectorLUT.h"
#include <charconv>
#include <expected>
#include <string>

#define FWD_ERR(x)                                                             \
  do {                                                                         \
    if (auto res = x; !res)                                                    \
      return std::unexpected{res.error()};                                     \
  } while (false)

#define UNWRAP_INNER(out, in, tmp)                                             \
  auto tmp = (in);                                                             \
  if (!tmp)                                                                    \
    return std::unexpected{tmp.error()};                                       \
  auto &out = *tmp;

#define CONCAT_INNER(a, b) a##b
#define CONCAT(a, b) CONCAT_INNER(a, b)

#define UNWRAP(out, in) UNWRAP_INNER(out, in, CONCAT(_tmp_val, __LINE__))

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

template <typename Derived> class ParserBase {
protected:
  VectorLUT<FatDynObjRef<>> identMap;
  VectorLUT<uint8_t> forwardDef;

public:
  TempBindVal<DynoLexer> lexer;
  using obj_parse_fn =
      MemberRef<FatDynObjRef<>(void *, DialectType type, ArrayRef<char> name)>;
  Interfaces<NUM_DIALECTS, obj_parse_fn> interfaces;
  SourceLocInfo<Instr> *sourceLocInfo = nullptr;

  auto &getInstrs() { return static_cast<Derived *>(this)->getInstrs(); }
  auto &getConstants() { return static_cast<Derived *>(this)->getConstants(); }
  auto &getCFG() { return static_cast<Derived *>(this)->getCFG(); }

private:
  struct ParseOperand {
    FatDynObjRef<> ref;
    bool isDef;
  };
  DialectInfos *infos;

protected:
  std::expected<FatDynObjRef<>, ParseError> parseObject(ArrayRef<char> name) {
    auto state = lexer->getState();
    UNWRAP(type, lexer->popType());
    auto fn = interfaces.template getVal<obj_parse_fn>(type.getDialectID());
    assert(fn);
    auto ref = fn(type, std::string_view{name});
    if (!ref) {
      lexer->restoreState(state);
      return std::unexpected{
          lexer->makeErrorOnNextToken("failed to parse object")};
    }
    return ref;
  }

  std::expected<ParseOperand, ParseError> parseConstantOperand() {
    FWD_ERR(lexer->tryPopEnsure(DynoLexer::op_hash));
    UNWRAP(litTok, lexer->tryPopEnsure(Token::BIG_INT_LITERAL));
    return ParseOperand{getConstants().findOrInsert(*litTok.bigIntLit.value),
                        false};
  }

  std::expected<ParseOperand, ParseError> parseNamedObject() {
    UNWRAP(ident, lexer->tryPopEnsure(Token::PCT_IDENTIFIER))

    bool isDef = false;
    FatDynObjRef<> obj = nullref;

    auto ref = identMap.find(ident.ident.idx);
    auto identStr = lexer->GetIdent(ident.ident.idx);
    assert(identStr[0] == '%');
    identStr = identStr.substr(1);

    if (lexer->popIf(DynoLexer::op_colon)) {
      isDef = !lexer->popIf(DynoLexer::op_qmark);
      UNWRAP(newObj, parseObject(ArrayRef{identStr}))
      obj = newObj;
      auto isFwdDef = forwardDef.find(ident.ident.idx);
      if (!(isFwdDef.has() && *isFwdDef))
        identMap.insertOrAssign(ident.ident.idx, FatDynObjRef{obj});
      else {
        // todo: delete object or don't parse at all
        obj = *ref;
      }
      forwardDef.insertOrAssign(ident.ident.idx, !isDef);
    } else {
      if (!ref)
        return std::unexpected{lexer->makeErrorOnPeekToken("undefined value")};
      obj = *ref;
    }

    return ParseOperand{obj, isDef};
  }

  std::expected<ParseOperand, ParseError> parseOperand() {
    auto tok = lexer->Peek();
    if (tok.type == DynoLexer::op_hash)
      return parseConstantOperand();

    if (tok.type == Token::PCT_IDENTIFIER)
      return parseNamedObject();

    if (tok.type == DynoLexer::op_colon) {
      lexer->Pop();
      UNWRAP(ref, parseObject(ArrayRef<char>::emptyRef()))
      return ParseOperand{ref, true};
    }

    if (lexer->peekType()) {
      UNWRAP(ref, parseObject(ArrayRef<char>::emptyRef()))
      return ParseOperand{ref, false};
    }

    return std::unexpected{lexer->makeErrorOnPeekToken(
        "invalid operand (expected constant or identifier)")};
  }

  std::expected<void, ParseError> parseBlockContents(BlockRef block) {
    FWD_ERR(lexer->tryPopEnsure(DynoLexer::op_cbropen));

    while (!lexer->popIf(DynoLexer::op_cbrclose)) {
      UNWRAP(instr, parseInstr());
      block.end().insertPrev(instr);
    }

    return {};
  }

  std::expected<void, ParseError> parseSourceLoc(InstrRef instr) {
    UNWRAP(tok, lexer->tryPeekEnsure(Token::STRING_LITERAL))
    if (!sourceLocInfo)
      return std::expected<void, ParseError>{};
    auto pos = tok.strLit.value.find_first_of(':');
    auto file = tok.strLit.value.substr(0, pos);
    auto lines = tok.strLit.value.substr(pos);
    auto linesSplit = Tokenizer{lines, ".-:"};
    SmallVec<uint32_t, 4> lineNums;
    bool bad = false;
    lineNums.push_back_range(
        Range{linesSplit}.transform([&bad](size_t, std::string_view view) {
          uint32_t val;
          auto res = std::from_chars(view.begin(), view.end(), val);
          if (res.ec != std::errc())
            bad = true;
          return val;
        }));
    if (bad)
      return std::unexpected{
          lexer->makeErrorOnPeekToken("invalid line numbers")};

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
      return std::unexpected{
          lexer->makeErrorOnPeekToken("invalid line numbers")};
    }

    lexer->Pop();
    sourceLocInfo->addSrcLoc(instr, file, lineNums[0], lineNums[1], lineNums[2],
                             lineNums[3]);
    return {};
  }

  std::expected<InstrRef, ParseError> parseInstr() {
    UNWRAP(opc, lexer->popOpcode());

    SmallVec<FatDynObjRef<>, 16> operands;
    uint numDefs = 0;

    SmallVec<BlockRef, 4> defBlocks;

    // continue parsing operands if we see #, : (anon def), %ident (ref or def)
    // or a valid object type (anon use)
    while (lexer->peekIs(DynoLexer::op_hash, DynoLexer::op_colon,
                         Token::PCT_IDENTIFIER) ||
           lexer->peekType()) {
      UNWRAP(op, parseOperand());
      if (op.isDef) {
        if (numDefs != operands.size()) {
          return std::unexpected{lexer->makeErrorOnPeekToken(
              "invalid def operand position (all def "
              "operands must be leading)")};
        }
        numDefs++;
        if (auto asBlock = op.ref.template dyn_as<BlockRef>())
          defBlocks.emplace_back(asBlock);
      }
      operands.emplace_back(op.ref);

      if (!lexer->popIf(DynoLexer::op_comma))
        break;
    }

    for (auto block : defBlocks)
      FWD_ERR(parseBlockContents(block));

    auto instr = getInstrs().create(operands.size(), opc);
    auto ib = InstrBuilder{instr};

    ib.addRefs(Range{operands.begin(), operands.begin() + numDefs});
    ib.other();
    ib.addRefs(Range{operands.begin() + numDefs, operands.end()});

    if (lexer->popIf(DynoLexer::op_abropen)) {
      while (lexer->peekIs(Token::STRING_LITERAL)) {
        FWD_ERR(parseSourceLoc(instr));

        if (!lexer->popIf(DynoLexer::op_comma))
          break;
      }
      FWD_ERR(lexer->tryPopEnsure(DynoLexer::op_abrclose));
    }

    while (lexer->popIf(DynoLexer::op_semicolon))
      ;

    return instr;
  }

public:
  void parse(ArrayRef<char> src, std::string fileName) {
    auto val = lexer.emplace(*infos, src, std::move(fileName));

    while (!lexer->peekIs(Token::NONE)) {
      if (auto res = parseInstr(); !res) {
        lexer->printError(res.error());
      }
    }
  }

  // parse, inserting into before iter
  void parse(ArrayRef<char> src, std::string fileName,
             BlockRef_iterator<true> insert) {
    auto val = lexer.emplace(*infos, src, std::move(fileName));

    while (!lexer->peekIs(Token::NONE)) {
      if (auto instr = parseInstr(); !instr)
        lexer->printError(instr.error());
      else
        insert.insertPrev(*instr);
    }
  }

  ParserBase(DialectInfos *infos) : infos(infos) {}
};

// Example parser wrapper. Derives from ParserBase and instantiates all dialect
// parser types. Dialect parser constructors register handlers in base parser.
// Same pattern used for printer.
template <typename Derived, typename... Parsers>
class Parser : public ParserBase<Derived> {
public:
  std::tuple<Parsers...> parsers;
  AutoDialectInfos<Parsers::dialect...> dialectInfos;
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wuninitialized"
  // we're just getting the address of dialectInfos.infos
  Parser()
      : ParserBase<Derived>(&dialectInfos.infos),
        parsers{((static_cast<void>(sizeof(Parsers))), this)...} {}
#pragma clang diagnostic pop
};

template <typename Derived> class CoreDialectParser {
  ParserBase<Derived> &base;

public:
  static constexpr DialectID dialect{DIALECT_CORE};
  CoreDialectParser(ParserBase<Derived> *base) : base(*base) {
    base->interfaces
        .template registerVal<typename ParserBase<Derived>::obj_parse_fn>(
            DIALECT_CORE,
            MemberRef{this, BindMethod<&CoreDialectParser::parseCore>::fv});
  }

  FatDynObjRef<> parseCore(DialectType type, ArrayRef<char> name) {
    assert(type.dialect == DIALECT_CORE);
    if (type == CORE_BLOCK) {
      auto block = base.getCFG().blocks.create(base.getCFG());
      return block;
    }

    return nullref;
  }
};

}; // namespace dyno
