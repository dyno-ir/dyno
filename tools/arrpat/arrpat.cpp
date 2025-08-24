
#include "dyno/DialectInfo.h"
#include "dyno/Instr.h"
#include "include/Lexer.h"
#include "include/Token.h"
#include "support/Optional.h"
#include "support/RTTI.h"
#include "support/SlabAllocator.h"
#include "support/Utility.h"
#include <array>
#include <cstdlib>
#include <fstream>
#include <optional>
#include <ostream>
#include <regex>
#include <sstream>
#include <string_view>

constexpr auto Operators = std::to_array(
    {":", "$", "{", "}", "...", ",", "#", "(", ")", "->", ";", "="});
enum OperatorEnum {
  _op_start = Lexer::TOK_OPS_START - 1,
  op_colon,
  op_dollarsign,
  op_cbropen,
  op_cbrclose,
  op_dots,
  op_comma,
  op_hash,
  op_rbropen,
  op_rbrclose,
  op_rightarrow,
  op_semicolon,
  op_equalsign
};

#define FOR_KEYWORDS(func)                                                     \
  func(match) func(requires) func(replace) func(anyorder) func(with)           \
      func(macro) func(defs)
#define FUNC(x) #x,
constexpr auto Keywords = std::to_array({FOR_KEYWORDS(FUNC)});
#undef FUNC
#define FUNC(x) kw_##x,
enum KeywordEnum {
  _pre_first_kw = Lexer::TOK_OPS_START + Operators.size() - 1,
  FOR_KEYWORDS(FUNC)
};

struct Object : public RTTIUtilMixin<Object> {
  enum ObjectKind {
    OPERAND,
    CONSTANT,
    INSTR,
    BLOCK,
    PACK,
    ANYORDER,
    REPL_USES,
    MACRO
  };
  ObjectKind kind;

protected:
  Object(ObjectKind kind) : kind(kind) {}
};

struct MatchConstant : public Object {
  const dyno::BigInt *bigInt;
  bool isUnsized;
  bool isSigned;

  static bool is_impl(const Object &obj) { return obj.kind == CONSTANT; }
  MatchConstant() : Object(CONSTANT) {}
};

struct MatchOperand : public Object {
  dyno::Optional<uint32_t> nameID;
  dyno::Optional<uint32_t> typeID;

  static bool is_impl(const Object &obj) { return obj.kind == OPERAND; }
  MatchOperand() : Object(OPERAND) {}
};

struct MatchInstr : public Object {
  dyno::Optional<uint32_t> opcodeNameID = dyno::nullopt;
  SmallVec<uint32_t, 4> opcodeIDs;
  SmallVec<Object *, 4> operands;
  uint32_t defOperands = 1;

  static bool is_impl(const Object &obj) { return obj.kind == INSTR; }
  MatchInstr() : Object(INSTR) {}
};

struct MatchBlock : public Object {
  SmallVec<Object *, 4> instrs;

  static bool is_impl(const Object &obj) { return obj.kind == BLOCK; }
  MatchBlock() : Object(BLOCK) {}
};

struct MatchPack : public Object {
  SmallVec<Object *, 4> objects;
  dyno::Optional<uint32_t> min;
  dyno::Optional<uint32_t> max;

  static bool is_impl(const Object &obj) { return obj.kind == PACK; }
  MatchPack() : Object(PACK) {}
};

struct MatchAnyorder : public Object {
  SmallVec<Object *, 4> objects;

  static bool is_impl(const Object &obj) { return obj.kind == ANYORDER; }
  MatchAnyorder() : Object(ANYORDER) {}
};

struct MatchMacro : public Object {
  uint nameID;
  dyno::Optional<uint32_t> bindName;
  SmallVec<Object *, 4> objects;

  static bool is_impl(const Object &obj) { return obj.kind == MACRO; }
  MatchMacro() : Object(MACRO) {}
};

struct ReplaceUses : public Object {
  struct Repl {
    Object *original;
    Object *replacement;
  };
  SmallVec<Repl, 4> repl;

  static bool is_impl(const Object &obj) { return obj.kind == REPL_USES; }
  ReplaceUses() : Object(REPL_USES) {}
};

struct MatchWithBlock {
  std::string_view inlineCode;
};

struct MatchPattern {
  MatchBlock *match;
  Object *replace;
  std::optional<MatchWithBlock> withBl;
};

struct Macro {
  std::string_view inlineCode;
  SmallVec<uint32_t, 4> params;
  SmallVec<uint32_t, 2> retvals;
};
SmallDenseMap<uint32_t, Macro, 1> macroMap;

SlabAllocator<std::string> stringsAlloc;
SlabAllocator<MatchPack> packAlloc;
SlabAllocator<MatchBlock> blockAlloc;
SlabAllocator<MatchInstr> instrAlloc;
SlabAllocator<MatchOperand> operandAlloc;
SlabAllocator<MatchConstant> constantAlloc;
SlabAllocator<MatchAnyorder> anyorderAlloc;
SlabAllocator<MatchMacro> macroAlloc;
SlabAllocator<ReplaceUses> replUsesAlloc;

template <auto Func> Object *parsePackable(Lexer &lexer) {
  auto parseBounds = [&](MatchPack *pack) {
    if (!lexer.popIf(op_cbropen))
      return;
    if (lexer.peekIs(Token::INT_LITERAL))
      pack->min = lexer.Pop().intLit.value;
    lexer.popEnsure(op_comma);
    if (lexer.peekIs(Token::INT_LITERAL))
      pack->max = lexer.Pop().intLit.value;
    lexer.popEnsure(op_cbrclose);
  };

  if (lexer.popIf(op_rbropen)) {
    MatchPack *pack = packAlloc.allocate();
    do {
      auto val = Func(lexer);
      pack->objects.emplace_back(val);
    } while (lexer.popIf(op_comma));

    lexer.popEnsure(op_rbrclose);
    lexer.popEnsure(op_dots);
    parseBounds(pack);
    return pack;
  } else {
    auto val = Func(lexer);
    if (lexer.popIf(op_dots)) {
      MatchPack *pack = packAlloc.allocate();
      pack->objects.emplace_back(val);
      parseBounds(pack);
      return pack;
    }
    return val;
  }
}

Object *parseSimple(Lexer &lexer);

void parseMacroParams(Lexer &lexer, MatchMacro *call) {
  lexer.popEnsure(op_rbropen);
  while (!lexer.peekIs(op_rbrclose)) {
    call->objects.emplace_back(parseSimple(lexer));
    if (!lexer.popIf(op_comma))
      break;
  }
  lexer.popEnsure(op_rbrclose);
}

Object *parseOperandImpl(Lexer &lexer) {
  lexer.popEnsure(op_dollarsign);
  auto identIdx = lexer.popEnsure(Token::IDENTIFIER).ident.idx;
  dyno::Optional<uint32_t> typeID;

  if (lexer.popIf(op_colon)) {
    auto typeIdentIdx = lexer.popEnsure(Token::IDENTIFIER).ident.idx;
    auto it = macroMap.find(typeIdentIdx);
    if (it) {
      auto *macro = macroAlloc.allocate();
      macro->bindName = identIdx;
      macro->nameID = typeIdentIdx;
      parseMacroParams(lexer, macro);
      return macro;
    } else
      typeID = typeIdentIdx;
  }

  auto *operand = operandAlloc.allocate();
  operand->nameID = identIdx;
  operand->typeID = typeID;
  return operand;
}

auto parseOperand(Lexer &lexer) {
  return parsePackable<parseOperandImpl>(lexer);
}

MatchConstant *parseConstant(Lexer &lexer) {
  lexer.popEnsure(op_hash);
  auto *constant = constantAlloc.allocate();

  if (lexer.peekIs(Token::BIG_INT_LITERAL)) {
    Token t = lexer.Pop();
    constant->bigInt = t.bigIntLit.value;
    constant->isSigned = t.bigIntLit.isSigned;
    constant->isUnsized = t.bigIntLit.isUnsized;
  } else
    assert(0);

  return constant;
}

void parseMacro(Lexer &lexer) {
  lexer.popEnsure(kw_macro);
  uint32_t identIdx = lexer.popEnsure(Token::IDENTIFIER).ident.idx;
  auto &macro = macroMap.insert(identIdx, Macro{}).val();

  while (lexer.popIf(op_dollarsign)) {
    auto param = lexer.popEnsure(Token::IDENTIFIER).ident.idx;
    macro.params.emplace_back(param);
    if (!lexer.popIf(op_comma))
      break;
  }
  if (lexer.popIf(op_rightarrow)) {
    while (lexer.popIf(op_dollarsign)) {
      auto param = lexer.popEnsure(Token::IDENTIFIER).ident.idx;
      macro.retvals.emplace_back(param);
      if (!lexer.popIf(op_comma))
        break;
    }
  }
  macro.inlineCode =
      lexer.popEnsure(Token::INLINE_CODE_LITERAL).inlineCodeLit.value;
}

MatchMacro *parseMacroUse(Lexer &lexer) {
  auto tok = lexer.popEnsure(Token::IDENTIFIER);
  auto it = macroMap.find(tok.ident.idx);
  assert(it && "unknown macro name");

  MatchMacro *ptr = macroAlloc.allocate();
  ptr->nameID = tok.ident.idx;
  ptr->bindName = true;

  parseMacroParams(lexer, ptr);

  return ptr;
}

Object *parseSimple(Lexer &lexer) {
  Object *val;
  if (lexer.peekIs(op_hash))
    val = parseConstant(lexer);
  else if (lexer.peekIs(Token::IDENTIFIER)) {
    val = parseMacroUse(lexer);
  } else
    val = parseOperand(lexer);

  return val;
}

MatchAnyorder *parseAnyorder(Lexer &lexer) {
  lexer.popEnsure(kw_anyorder);
  lexer.popEnsure(op_rbropen);
  MatchAnyorder *anyorder = anyorderAlloc.allocate();
  do {
    Object *val;

    if (lexer.peekIs(op_hash))
      val = parseConstant(lexer);
    else
      val = parseOperand(lexer);

    anyorder->objects.emplace_back(val);
  } while (lexer.popIf(op_comma));

  lexer.popEnsure(op_rbrclose);
  return anyorder;
}

MatchBlock *parseBlock(Lexer &lexer);
MatchInstr *parseInstrImpl(Lexer &lexer) {
  auto *instr = instrAlloc.allocate();

  if (lexer.peekIs(Token::IDENTIFIER)) {
    auto identIdx = lexer.Pop().ident.idx;
    instr->opcodeIDs.emplace_back(identIdx);
  } else if (lexer.peekIs(op_dollarsign)) {
    lexer.Pop();
    instr->opcodeNameID = lexer.popEnsure(Token::IDENTIFIER).ident.idx;
    if (lexer.popIf(op_cbropen)) {
      while (!lexer.peekIs(op_cbrclose)) {
        instr->opcodeIDs.emplace_back(
            lexer.popEnsure(Token::IDENTIFIER).ident.idx);
        if (!lexer.popIf(op_comma))
          break;
      }
      lexer.popEnsure(op_cbrclose);
    }
  }

  if (lexer.popIf(kw_defs)) {
    lexer.popEnsure(op_equalsign);
    instr->defOperands = lexer.popEnsure(Token::INT_LITERAL).intLit.value;
  }

  while (lexer.peekIs(op_dollarsign, op_cbropen, op_hash, op_rbropen,
                      kw_anyorder, Token::IDENTIFIER)) {
    if (lexer.peekIs(op_cbropen))
      instr->operands.emplace_back(parseBlock(lexer));
    else if (lexer.peekIs(kw_anyorder))
      instr->operands.emplace_back(parseAnyorder(lexer));
    else
      instr->operands.emplace_back(parseSimple(lexer));

    if (!lexer.popIf(op_comma))
      break;
  }
  lexer.popIf(op_semicolon);

  return instr;
}
auto parseInstr(Lexer &lexer) { return parsePackable<parseInstrImpl>(lexer); }

MatchBlock *parseBlock(Lexer &lexer) {
  lexer.popEnsure(op_cbropen);
  auto *block = blockAlloc.allocate();

  while (lexer.Peek().type != op_cbrclose) {
    block->instrs.emplace_back(parseInstr(lexer));
  }

  lexer.popEnsure(op_cbrclose);
  return block;
}

ReplaceUses *parseReplAllUses(Lexer &lexer) {
  auto *repl = replUsesAlloc.allocate();

  while (lexer.Peek().type == op_dollarsign) {
    ReplaceUses::Repl rule;
    rule.original = parseOperand(lexer);
    lexer.popEnsure(op_rightarrow);
    rule.replacement =
        lexer.peekIs(op_hash) ? parseConstant(lexer) : parseOperand(lexer);

    repl->repl.emplace_back(rule);
    if (!lexer.popIf(op_comma))
      break;
  }

  return repl;
}

std::string_view parseWith(Lexer &lexer) {
  lexer.popEnsure(kw_with);
  auto tok = lexer.popEnsure(Token::INLINE_CODE_LITERAL);
  return tok.inlineCodeLit.value;
}

MatchPattern parsePattern(Lexer &lexer) {
  MatchPattern pattern;
  lexer.popEnsure(kw_match);
  pattern.match = parseBlock(lexer);

  if (lexer.peekIs(kw_with))
    pattern.withBl = MatchWithBlock{parseWith(lexer)};

  lexer.popEnsure(kw_replace);
  if (lexer.peekIs(op_dollarsign)) {
    pattern.replace = parseReplAllUses(lexer);
  } else {
    pattern.replace = parseBlock(lexer);
  }
  return pattern;
}

static std::string readFileIntoStr(std::string path) {
  std::ifstream ifs(path);
  if (!ifs) {
    fprintf(stderr, "Aborting! File does not exist: %s\n", path.c_str());
    exit(-1);
  }
  std::string content((std::istreambuf_iterator<char>(ifs)),
                      std::istreambuf_iterator<char>());

  return content;
}

// idea: both instrs and blocks are containers iterating over
// operands and instrs resp. build an iterator based vm so we can share as much
// as possible between block and instr matching.
struct BytecodeOp {
  enum Opcode : uint8_t {
    // you get instr index as indirect (block index).
    // that way can trivially scan thru operands and instrs

    PUSH_CONTINUE,
    POP_CONTINUE,

    CHECK_OPCODE, // either immediate or register
    CHECK_TYPE,   // either immediate or register x2
    CHECK_NUM_OPERANDS,
    CHECK_EQUAL,      // a, b
    CHECK_ITER_EQUAL, // a, b
    CHECK_SIZE_LE,
    CHECK_SIZE_GE,

    GET_BEGIN,     // dst reg, container
    GET_END,       // dst reg, container
    GET_DEF_INSTR, // dst reg, instr, operand

    NEXT,

    LOOP,     // len, register low, register idx
    LOOP_FOR, // len, register low, register idx
    CONTINUE,
    BREAK,
    GOTO,

    REPLACE_ALL_USES,              // operand old, operand new
    CREATE_CONSTANT,               // dst reg, bigInt value
    CREATE_CONSTANT_LIKE_SIGNED,   // dst reg, like, uint32_t value
    CREATE_CONSTANT_LIKE_UNSIGNED, // dst reg, like, uint32_t value
    CREATE_INSTR,                  // opcode, size
    CREATE_INSTR_LIKE,             // opcode, size

    APPEND_COPY, // list reg, src

    COPY_OPERANDS,        // instr dst, instr src, operand, operand_end
    COPY_OPERAND,         // instr dst, instr src, operand, operand_end
    SET_OPERANDS_OTHER,   // instr
    DELETE,               // instr
    DELETE_IF_SINGLE_USE, // instr
    INLINE_CODE,
    CHECK_CONSTANT_SIGNED,
    CHECK_CONSTANT_UNSIGNED,
    CHECK_CONSTANT_EXACT,
  };

  Opcode opcode;

  union {

    struct {
      uint32_t len;
    } pushContinue;

    struct {
      uint32_t len;
    } gotoOp;

    struct {
      uint32_t instr;
      uint32_t opcode;
    } checkOpcode;

    struct {
      uint32_t operand;
      uint32_t type;
    } checkType;

    struct {
      uint32_t lhsReg;
      uint32_t rhsReg;
    } checkEqual, checkIterEqual;

    struct {
      uint32_t containerReg;
      uint32_t elems;
    } checkSizeLess, checkSizeGreater;

    struct {
      uint32_t dstReg;
      uint32_t operand;
    } getDefInstr;

    struct {
      uint32_t dstReg;
      uint32_t containerReg;
    } getBegin, getEnd;

    struct {
      uint32_t bodyLen;
      uint32_t start;
      uint32_t cur;
      uint32_t end;
    } scanForward;

    struct {
      uint32_t dstList;
      uint32_t srcContainer;
    } appendCopy;

    struct {
      uint32_t dstInstr;
      uint32_t begin;
      uint32_t end;
    } copyOperands;
    struct {
      uint32_t dstInstr;
      uint32_t src;
    } copyOperand;

    struct {
      uint32_t outReg;
      uint32_t srcA;
    } arith;

    struct {
      uint32_t oldOp;
      uint32_t newOp;
    } replAllUses;

    struct {
      uint32_t reg;
      const dyno::BigInt *bigInt;
    } createConstant;

    struct {
      uint32_t reg;
      uint32_t like;
      const dyno::BigInt *bigInt;
    } createConstantLike;

    struct {
      uint32_t outReg;
      uint32_t opcode;
    } createInstr;

    struct {
      uint32_t outReg;
      uint32_t likeReg;
    } createInstrLike;

    struct {
      std::string_view str;
    } inlineCode;

    struct {
      uint32_t instr;
    } deleteI, deleteIfSingleUse;

    struct {
      uint32_t instr;
    } setOperandsOther;

    struct {
      uint32_t operand;
      const dyno::BigInt *bigInt;
    } checkConstantExact;

    struct {
      uint32_t operand;
      const dyno::BigInt *bigInt;
    } checkConstant;

    struct {

    } empty;
  };

  static BytecodeOp makeCheckOpcode(uint32_t instr, uint32_t opcode) {
    return BytecodeOp{.opcode = CHECK_OPCODE, .checkOpcode{instr, opcode}};
  }

  static BytecodeOp makeCheckType(uint32_t operand, uint32_t type) {
    return BytecodeOp{.opcode = CHECK_TYPE, .checkType{operand, type}};
  }
  static BytecodeOp makeCopyOperand(uint32_t dstInstr, uint32_t operandBegin,
                                    uint32_t operandEnd) {
    return BytecodeOp{.opcode = COPY_OPERANDS,
                      .copyOperands{dstInstr, operandBegin, operandEnd}};
  }
};

struct CodeGen {
  Lexer &lexer;
  std::ostream &os;
  std::vector<BytecodeOp> ops;
  uint replaceMacroInsertIdx;
  MatchPattern *curPattern;
  SmallDenseSet<Object *, 1> visitedInstrs;

  CodeGen(Lexer &lexer, std::ostream &os) : lexer(lexer), os(os) {}

  struct Variable {
    uint32_t operand;
    uint32_t end;
  };
  DenseMap<uint32_t, Variable> vars;

  enum class RegType : uint8_t {
    OPERAND,
    INSTR,
    BLOCK,
    LIST,
    LIST_ITER,
    REPL_INSTR,
    CONSTANT
  };
  SmallVec<RegType, 64> regTypes;

  uint32_t makeOperandRef() {
    regTypes.emplace_back(RegType::OPERAND);
    return regTypes.size() - 1;
  }

  uint32_t makeInstrRef() {
    regTypes.emplace_back(RegType::INSTR);
    return regTypes.size() - 1;
  }

  uint32_t makeReplInstr() {
    regTypes.emplace_back(RegType::REPL_INSTR);
    return regTypes.size() - 1;
  }

  uint32_t makeListRef() {
    regTypes.emplace_back(RegType::LIST);
    return regTypes.size() - 1;
  }

  uint32_t makeBlockRef() {
    regTypes.emplace_back(RegType::BLOCK);
    return regTypes.size() - 1;
  }

  uint32_t makeListIter() {
    regTypes.emplace_back(RegType::LIST_ITER);
    return regTypes.size() - 1;
  }

  uint32_t makeConstant() {
    regTypes.emplace_back(RegType::CONSTANT);
    return regTypes.size() - 1;
  }

  uint32_t makeImm(uint n) { return n; }
  uint32_t nextOperand(uint32_t a) {
    auto reg = makeOperandRef();
    ops.emplace_back(
        BytecodeOp{.opcode = BytecodeOp::Opcode::NEXT, .arith{reg, a}});
    return reg;
  }

  auto findInDefs(uint32_t nameID) {
    SmallVec<MatchInstr *, 4> matches;
    for (auto obj : curPattern->match->instrs) {
      if (auto *asInstr = obj->dyn_as<MatchInstr>()) {
        if (asInstr->operands.size() == 0)
          continue;
        if (auto *asOp = asInstr->operands[0]->dyn_as<MatchOperand>()) {
          if (asOp->nameID == nameID)
            matches.emplace_back(asInstr);
        }
      }
    }
    assert(matches.size() <= 1 && "multi-def");

    return matches.empty() ? std::nullopt : std::optional(matches[0]);
  }

  void checkSimple(uint32_t operandID, Object *obj) {

    auto checkSubInstr = [&](uint32_t name) {
      if (auto defI = findInDefs(name);
          defI && !visitedInstrs.contains(*defI)) {

        auto instrReg = makeInstrRef();
        ops.push_back(BytecodeOp{.opcode = BytecodeOp::GET_DEF_INSTR,
                                 .getDefInstr = {instrReg, operandID}});
        generateMatchInstr(instrReg, *defI);
      }
    };

    if (auto operand = obj->dyn_as<MatchOperand>()) {
      if (operand->typeID)
        ops.emplace_back(
            BytecodeOp::makeCheckType(operandID, *operand->typeID));
      if (operand->nameID)
        checkSubInstr(*operand->nameID);
    } else if (auto constant = obj->dyn_as<MatchConstant>()) {
      ops.push_back(BytecodeOp{
          .opcode = BytecodeOp::CHECK_TYPE,
          .checkType = {operandID, lexer.GetIdentIdx("CORE_CONSTANT")}});
      if (constant->isUnsized) {
        ops.push_back(BytecodeOp{
            .opcode = constant->isSigned ? BytecodeOp::CHECK_CONSTANT_SIGNED
                                         : BytecodeOp::CHECK_CONSTANT_UNSIGNED,
            .checkConstant = {operandID, constant->bigInt}});
      } else {
        ops.push_back(
            BytecodeOp{.opcode = BytecodeOp::CHECK_CONSTANT_EXACT,
                       .checkConstantExact = {operandID, constant->bigInt}});
      }
    } else if (auto call = obj->dyn_as<MatchMacro>()) {
      auto it = macroMap.find(call->nameID);
      assert(it && "undefined macro");
      assert(it.val().params.size() >= 1);
      assert(it.val().retvals.size() == 0);
      std::string code(it.val().inlineCode.begin(), it.val().inlineCode.end());
      replaceParamsInMacro(
          call, ArrayRef{it.val().params.begin() + 1, it.val().params.back()},
          code);

      std::stringstream str;
      std::print(str, "Range{{r{0}, std::next(r{0})}}", operandID);
      replaceVarInMacro(code, it.val().params.front(), str.str());

      auto *codePtr = stringsAlloc.allocate(code);
      ops.push_back(BytecodeOp{
          .opcode = BytecodeOp::INLINE_CODE,
          .inlineCode{std::string_view(codePtr->begin(), codePtr->end())}});

      if (call->bindName)
        checkSubInstr(*call->bindName);
    }
  }

  // returns idx of next operand
  uint32_t checkPack(uint32_t operandID, uint32_t operandsEnd, Object *obj,
                     Object *nextObj) {
    auto pack = obj->as<MatchPack>();

    assert((!pack.min && !pack.max) && "todo, bounds unsupported here");

    auto startIdx = ops.size();

    auto idx = makeOperandRef();
    uint32_t nextIdx;
    if (nextObj)
      nextIdx = makeOperandRef();

    ops.push_back(
        BytecodeOp{.opcode = nextObj ? BytecodeOp::LOOP : BytecodeOp::LOOP_FOR,
                   .scanForward{0, operandID, idx, operandsEnd}});
    auto condIdx = ops.size();
    // if we can match the next operand the loop is over.
    if (nextObj) {
      auto pushContIdx = ops.size();
      ops.push_back(
          BytecodeOp{.opcode = BytecodeOp::PUSH_CONTINUE, .pushContinue = {0}});
      checkSimple(idx, nextObj);
      ops.push_back(
          BytecodeOp{.opcode = BytecodeOp::NEXT, .arith = {nextIdx, idx}});
      registerOrCheckNameOfSimple(nextObj, idx, nextIdx);

      ops.push_back(BytecodeOp{.opcode = BytecodeOp::BREAK, .empty{}});
      ops[pushContIdx].pushContinue.len = ops.size() - pushContIdx - 1;
    }

    for (auto elem : pack.objects) {
      checkSimple(idx, elem);
    }

    registerOrCheckNameOfSimple(&pack, operandID, idx);

    ops[startIdx].scanForward.bodyLen = ops.size() - condIdx;

    return nextObj ? nextIdx : idx;
  }

  uint32_t checkAnyorder(uint32_t opIdx, uint32_t opEnd,
                         MatchAnyorder *anyorder) {
    uint32_t listsBase = regTypes.size();
    for (uint i = 0; i < anyorder->objects.size(); i++)
      makeListRef();

    auto iter = makeOperandRef();
    auto scanFwdIdx = ops.size();
    ops.push_back(BytecodeOp{.opcode = BytecodeOp::LOOP_FOR,
                             .scanForward{0, opIdx, iter, opEnd}});

    for (auto [i, obj] : Range{anyorder->objects}.enumerate()) {
      auto pushContIdx = ops.size();

      auto pack = obj->dyn_as<MatchPack>();
      assert(!pack || pack->objects.size() == 1);

      ops.push_back(
          BytecodeOp{.opcode = BytecodeOp::PUSH_CONTINUE, .pushContinue = {}});

      if (!pack) {
        ops.push_back(BytecodeOp{.opcode = BytecodeOp::CHECK_SIZE_LE,
                                 .checkSizeLess = {uint(listsBase + i), 0}});
      } else if (pack->max) {
        ops.push_back(
            BytecodeOp{.opcode = BytecodeOp::CHECK_SIZE_LE,
                       .checkSizeLess = {uint(listsBase + i), *pack->max}});
      }
      checkSimple(iter, pack ? pack->objects[0] : obj);

      ops.push_back(BytecodeOp{.opcode = BytecodeOp::APPEND_COPY,
                               .appendCopy = {(uint)(listsBase + i), iter}});

      ops.push_back(BytecodeOp{.opcode = BytecodeOp::CONTINUE, .empty{}});
      ops[pushContIdx].pushContinue.len = ops.size() - pushContIdx - 1;
    }

    ops[scanFwdIdx].scanForward.bodyLen = ops.size() - scanFwdIdx - 1;

    for (uint i = 0; i < anyorder->objects.size(); i++) {
      dyno::Optional<uint32_t> min;
      if (auto pack = anyorder->objects[i]->dyn_as<MatchPack>())
        min = pack->min;
      else if (anyorder->objects[i]->is<MatchOperand>() ||
               anyorder->objects[i]->is<MatchConstant>())
        min = 1;
      if (!min)
        continue;
      ops.push_back(BytecodeOp{.opcode = BytecodeOp::CHECK_SIZE_GE,
                               .checkSizeLess = {uint(listsBase + i), *min}});
    }

    for (auto [i, obj] : Range{anyorder->objects}.enumerate()) {
      auto beginReg = makeListIter();
      ops.emplace_back(BytecodeOp{.opcode = BytecodeOp::GET_BEGIN,
                                  .getBegin = {beginReg, uint(listsBase + i)}});
      auto endReg = makeListIter();
      ops.emplace_back(BytecodeOp{.opcode = BytecodeOp::GET_END,
                                  .getEnd = {endReg, uint(listsBase + i)}});

      registerOrCheckNameOfSimple(obj, beginReg, endReg);
    }

    return iter;
  }

  void registerOrCheckName(uint32_t nameID, uint32_t begin, uint32_t end) {
    auto newVar = Variable{begin, end};
    auto [found, it] = vars.findOrInsert(nameID, newVar);
    if (!found) {
      std::print(std::cerr, "inserted {}: {}\n", nameID,
                 lexer.GetIdent(nameID));
    }
    if (found) {
      ops.emplace_back(BytecodeOp{.opcode = BytecodeOp::CHECK_EQUAL,
                                  .checkEqual = {begin, it.val().operand}});
    }
  }

  void registerOrCheckNameOfSimple(Object *object, uint32_t begin,
                                   uint32_t end) {
    if (auto *asPack = object->dyn_as<MatchPack>()) {
      registerOrCheckNameOfSimple(asPack->objects[0], begin, end);
    } else if (auto matchOperand = object->dyn_as<MatchOperand>())
      registerOrCheckName(*matchOperand->nameID, begin, end);
    else if (auto matchMacro = object->dyn_as<MatchMacro>();
             matchMacro && matchMacro->bindName)
      registerOrCheckName(*matchMacro->bindName, begin, end);
  }

  auto getMinMaxNumOperands(MatchInstr *instr) {
    dyno::Optional<uint32_t> min = 0;
    dyno::Optional<uint32_t> max = 0;

    auto incrOrUndef = [](dyno::Optional<uint32_t> &lhs,
                          dyno::Optional<uint32_t> rhs) {
      if (!lhs)
        return;
      if (!rhs) {
        lhs = dyno::nullopt;
        return;
      }
      *lhs += *rhs;
    };

    SmallVec<ArrayRef<Object *>, 4> stack;
    stack.emplace_back(instr->operands);

    while (!stack.empty()) {
      auto arr = stack.pop_back_val();
      for (auto op : arr) {
        if (auto pack = op->dyn_as<MatchPack>()) {
          uint multiple = pack->objects.size();
          auto packMax = pack->max;
          if (packMax)
            *packMax *= multiple;
          incrOrUndef(min, pack->min.value_or(0) * multiple);
          incrOrUndef(max, packMax);
          continue;
        }
        if (op->is<MatchConstant>()) {
          incrOrUndef(min, 1);
          incrOrUndef(max, 1);
          continue;
        }
        if (op->is<MatchOperand>()) {
          incrOrUndef(min, 1);
          incrOrUndef(max, 1);
          continue;
        }
        if (auto anyorder = op->dyn_as<MatchAnyorder>()) {
          stack.emplace_back(anyorder->objects);
          continue;
        }
        if (auto macro = op->dyn_as<MatchMacro>()) {
          // macro can return anywhere from 0 to infinity objects.
          max = dyno::nullopt;
        }
      }
    }
    return std::make_pair(min, max);
  }

  void generateMatchInstr(uint32_t instrReg, MatchInstr *instr) {
    visitedInstrs.insert(instr);
    size_t low = 0, high = instr->operands.size() - 1;

    if (instr->opcodeIDs.size() == 1)
      ops.emplace_back(
          BytecodeOp::makeCheckOpcode(instrReg, instr->opcodeIDs.front()));
    else if (instr->opcodeIDs.size() > 1) {
      uint len = (instr->opcodeIDs.size() - 1) * 3 + 1;
      for (auto [back, opc] : Range{instr->opcodeIDs}.mark_back()) {
        if (!back)
          ops.emplace_back(BytecodeOp{.opcode = BytecodeOp::PUSH_CONTINUE,
                                      .pushContinue = {2}});
        ops.emplace_back(BytecodeOp::makeCheckOpcode(instrReg, opc));
        if (!back)
          ops.emplace_back(
              BytecodeOp{.opcode = BytecodeOp::GOTO, .gotoOp = {len - 3}});
        len -= 3;
      }
    }

    if (instr->opcodeNameID) {
      std::print(std::cerr, "inserted {}: {}\n", *instr->opcodeNameID,
                 lexer.GetIdent(*instr->opcodeNameID));
      vars.insert(*instr->opcodeNameID, Variable{instrReg, 0});
    }

    auto [minOperands, maxOperands] = getMinMaxNumOperands(instr);
    if (maxOperands) {
      ops.emplace_back(BytecodeOp{.opcode = BytecodeOp::CHECK_SIZE_LE,
                                  .checkSizeGreater{instrReg, *maxOperands}});
      if (*maxOperands == 0)
        return;
    }

    if (minOperands)
      ops.emplace_back(BytecodeOp{.opcode = BytecodeOp::CHECK_SIZE_GE,
                                  .checkSizeGreater{instrReg, *minOperands}});

    uint32_t opIdx = makeOperandRef();
    ops.emplace_back(BytecodeOp{.opcode = BytecodeOp::GET_BEGIN,
                                .getBegin = {opIdx, instrReg}});

    uint32_t endIdx = makeOperandRef();
    ops.emplace_back(BytecodeOp{.opcode = BytecodeOp::GET_END,
                                .getEnd = {endIdx, instrReg}});

    while (low != high + 1) {
      Object *obj = instr->operands[low];
      bool highSide = false;
      size_t i = low;

      if (auto *pack = obj->dyn_as<MatchPack>()) {
        auto *nextIdx =
            i == instr->operands.size() - 1 ? nullptr : instr->operands[i + 1];
        opIdx = checkPack(opIdx, endIdx, pack, nextIdx);
        if (nextIdx) {
          low++;
        }
      } else if (auto *anyorder = obj->dyn_as<MatchAnyorder>()) {
        opIdx = checkAnyorder(opIdx, endIdx, anyorder);
        assert(i == high && "todo, anyorder must be trailing");
      } else {
        checkSimple(opIdx, instr->operands[i]);
        auto nextOpIdx = nextOperand(opIdx);
        registerOrCheckNameOfSimple(instr->operands[i], opIdx, nextOpIdx);
        opIdx = nextOpIdx;
      }

      if (highSide)
        high--;
      else
        low++;
    }

    if (!(minOperands && maxOperands))
      ops.emplace_back(BytecodeOp{.opcode = BytecodeOp::CHECK_ITER_EQUAL,
                                  .checkIterEqual{opIdx, endIdx}});
  }

  void generateMatch(MatchPattern *pattern) {
    curPattern = pattern;
    generateMatchInstr(makeInstrRef(),
                       &pattern->match->instrs.back()->as<MatchInstr>());
    curPattern = nullptr;
  }

  void replaceVarInMacro(std::string &input, uint nameID,
                         const std::string &replWith) {
    std::string_view paramName = lexer.GetIdent(nameID);
    std::string param = "\\$" + std::string(paramName);
    auto paramRegex = std::regex{param};
    input = std::regex_replace(input, paramRegex, replWith);
  }

  void replaceConstantInMacro(std::string &input, uint nameID,
                              MatchConstant *constant) {
    std::stringstream str;
    str << "\"";
    constant->bigInt->toStream(str, 16, false);
    str << "\"_bv";
    replaceVarInMacro(input, nameID, str.str());
  }

  void replaceParamsInMacro(MatchMacro *call, ArrayRef<uint32_t> paramNameIDs,
                            std::string &code) {
    for (auto [i, obj] : Range{call->objects}.enumerate()) {
      if (auto asConst = obj->dyn_as<MatchConstant>()) {
        replaceConstantInMacro(code, paramNameIDs[i], asConst);
        continue;
      }
      MatchOperand *asOperand;
      if (auto asPack = obj->dyn_as<MatchPack>()) {
        assert(asPack->objects.size() == 1 && "only pack of size 1 supported");
        asOperand = &asPack->objects.front()->as<MatchOperand>();
      } else
        asOperand = &obj->as<MatchOperand>();

      assert(!asOperand->typeID && "type check unsupported");
      auto varIt = vars.find(*asOperand->nameID);
      assert(varIt && "unknown name");
      std::stringstream str;
      std::print(str, "Range{{r{}, r{}}}", varIt.val().operand,
                 varIt.val().end);
      replaceVarInMacro(code, paramNameIDs[i], str.str());
    }
  }

  void generateReplaceMacro(MatchMacro *call, uint dstInstr) {
    // eval macro objects.
    // replace strings in macro with evald objects.
    // replace rv string with newly allocated rv list
    auto it = macroMap.find(call->nameID);

    auto inlineCodeIdx = replaceMacroInsertIdx;
    ops.insert(ops.begin() + replaceMacroInsertIdx,
               BytecodeOp{.opcode = BytecodeOp::INLINE_CODE, .inlineCode{}});
    replaceMacroInsertIdx++;

    assert(it);
    auto macro = it.val();
    std::string code{macro.inlineCode};
    replaceParamsInMacro(call, macro.params, code);

    for (auto retval : Range{macro.retvals}) {
      uint begin = makeListIter();
      uint end = makeListIter();

      if (auto name = call->bindName) {
        assert(macro.retvals.size() == 1 &&
               "can't bind multiple retvals to one name");
        std::print(std::cerr, "inserted {}: {}\n", *name,
                   lexer.GetIdent(*name));
        vars.insert(*name, Variable{begin, end});
      }

      ops.emplace_back(BytecodeOp{.opcode = BytecodeOp::COPY_OPERANDS,
                                  .copyOperands{dstInstr, begin, end}});

      std::stringstream str;
      std::print(str, "RefRange{{r{}, r{}}}", begin, end);
      replaceVarInMacro(code, retval, str.str());
    }

    auto stringPtr = stringsAlloc.allocate(code);
    ops[inlineCodeIdx].inlineCode.str =
        std::string_view{stringPtr->begin(), stringPtr->end()};
  }

  void generateReplaceInstr(MatchInstr *instr) {
    auto dstInstr = makeReplInstr();

    // todo: configurable #defs
    uint numDefOperands = instr->defOperands;
    if (instr->opcodeIDs.size() == 1) {
      ops.emplace_back(
          BytecodeOp{.opcode = BytecodeOp::CREATE_INSTR,
                     .createInstr{dstInstr, instr->opcodeIDs.front()}});
    } else {
      assert(instr->opcodeIDs.size() == 0);
      auto it = vars.find(*instr->opcodeNameID);
      ops.emplace_back(BytecodeOp{.opcode = BytecodeOp::CREATE_INSTR_LIKE,
                                  .createInstr{dstInstr, it.val().operand}});
    }

    for (auto [opIdx, op] : Range{instr->operands}.enumerate()) {
      if (opIdx == numDefOperands) {
        ops.emplace_back(BytecodeOp{.opcode = BytecodeOp::SET_OPERANDS_OTHER,
                                    .setOperandsOther = {dstInstr}});
      }
      if (auto operand = op->dyn_as<MatchOperand>()) {
        auto var = vars.find(*operand->nameID);
        if (!var) {
          std::cerr << "name not found: " << lexer.GetIdent(*operand->nameID)
                    << "\n";
          abort();
        }
        ops.emplace_back(BytecodeOp::makeCopyOperand(
            dstInstr, var.val().operand, var.val().end));
      } else if (auto constant = op->dyn_as<MatchConstant>()) {
        if (constant->isUnsized) {
          assert(0);
        } else {
          auto reg = makeConstant();
          ops.emplace_back(
              BytecodeOp{.opcode = BytecodeOp::CREATE_CONSTANT,
                         .createConstant = {reg, constant->bigInt}});
          ops.emplace_back(BytecodeOp{.opcode = BytecodeOp::COPY_OPERAND,
                                      .copyOperand = {dstInstr, reg}});
        }
      } else if (auto pack = op->dyn_as<MatchPack>()) {
        if (pack->objects.size() != 1)
          abort();
        if (!pack->objects.front()->is<MatchOperand>())
          abort();
        auto operand = pack->objects.front()->as<MatchOperand>();
        if (!operand.nameID)
          abort();
        auto &var = vars.find(*operand.nameID).val();
        ops.emplace_back(
            BytecodeOp::makeCopyOperand(dstInstr, var.operand, var.end));
      } else if (auto macro = op->dyn_as<MatchMacro>()) {
        generateReplaceMacro(macro, dstInstr);
      } else
        abort();
    }
  }

  void generateReplUses(ReplaceUses *repl) {
    for (auto rule : repl->repl) {
      auto orig = rule.original->as<MatchOperand>();
      auto origReg = vars.find(*orig.nameID).val().operand;
      // todo: packs?

      uint32_t replReg;
      if (auto *repl = rule.replacement->dyn_as<MatchOperand>()) {
        auto it = vars.find(*repl->nameID);
        if (!it) {
          std::cerr << "name not found: " << lexer.GetIdent(*repl->nameID)
                    << "\n";
          abort();
        }
        replReg = it.val().operand;
      } else if (auto *constant = rule.replacement->dyn_as<MatchConstant>()) {
        replReg = makeConstant();
        if (constant->isUnsized) {
          ops.emplace_back(BytecodeOp{
              .opcode = constant->isSigned
                            ? BytecodeOp::CREATE_CONSTANT_LIKE_SIGNED
                            : BytecodeOp::CREATE_CONSTANT_LIKE_UNSIGNED,
              .createConstantLike = {replReg, origReg, constant->bigInt}});
        } else {
          ops.emplace_back(
              BytecodeOp{.opcode = BytecodeOp::CREATE_CONSTANT,
                         .createConstant = {replReg, constant->bigInt}});
        }
      } else
        assert(0);

      ops.emplace_back(BytecodeOp{.opcode = BytecodeOp::REPLACE_ALL_USES,
                                  .replAllUses = {origReg, replReg}});
    }
  }

  void generateWith(MatchWithBlock &with) {
    ops.emplace_back(BytecodeOp{.opcode = BytecodeOp::INLINE_CODE,
                                .inlineCode = {with.inlineCode}});
    auto delimdIdent = std::regex(R"(\$[a-zA-Z_][a-zA-Z0-9_]*)");
    std::regex_iterator it(with.inlineCode.begin(), with.inlineCode.end(),
                           delimdIdent);
    std::regex_iterator<std::string_view::iterator> end;
    for (; it != end; ++it) {
      auto match = (*it)[0];
      uint32_t identID =
          lexer.GetIdentIdx(std::string_view{match.first + 1, match.second});
      vars.findOrInsert(
          identID, [&] { return Variable{makeListIter(), makeListIter()}; });
    }
  }

  void generateDeletes() {
    size_t i = regTypes.size() - 1;
    for (auto regT : Range{regTypes}.reverse()) {
      if (regT == RegType::INSTR) {
        auto opcode = BytecodeOp::DELETE_IF_SINGLE_USE;
        if (i == 0)
          opcode = BytecodeOp::DELETE;
        ops.emplace_back(
            BytecodeOp{.opcode = opcode, .deleteI = {(uint32_t)i}});
      }
      i--;
    }
  }

  void generateReplace(MatchPattern &pat) {
    if (auto block = pat.replace->dyn_as<MatchBlock>()) {
      for (auto instr : block->instrs)
        generateReplaceInstr(&instr->as<MatchInstr>());
    } else if (auto repl = pat.replace->dyn_as<ReplaceUses>()) {
      generateReplUses(repl);
    }
  }

  void generatePattern(MatchPattern &pat) {
    generateMatch(&pat);
    if (pat.withBl)
      generateWith(*pat.withBl);
    replaceMacroInsertIdx = ops.size();
    generateReplace(pat);
    generateDeletes();
  }

  void dump() {
    std::stringstream str;

    SmallVec<uint, 4> indentStack;

    for (auto [i, op] : Range{ops}.enumerate()) {

      uint *it;
      while (!indentStack.empty() &&
             (it = std::find(indentStack.begin(), indentStack.end(), i)) !=
                 indentStack.end())
        indentStack.erase(it);

      for (size_t i = 0; i < indentStack.size(); i++)
        std::print(str, "  ");

      switch (op.opcode) {
      case BytecodeOp::PUSH_CONTINUE:
        std::print(str, "PUSH_CONTINUE #{}\n", op.pushContinue.len);

        indentStack.emplace_back(i + op.pushContinue.len + 1);
        break;
      case BytecodeOp::GOTO:
        std::print(str, "GOTO #{}\n", op.gotoOp.len);

        indentStack.emplace_back(i + op.gotoOp.len + 1);
        break;

      case BytecodeOp::POP_CONTINUE:
        std::print(str, "POP_CONTINUE\n");
        break;

      case BytecodeOp::CHECK_OPCODE:
        std::print(str, "CHECK_OPCODE i{}, {}\n", op.checkOpcode.instr,
                   lexer.GetIdent(op.checkOpcode.opcode));
        break;

      case BytecodeOp::CHECK_TYPE:
        std::print(str, "CHECK_TYPE o{}, {}\n", op.checkType.operand,
                   lexer.GetIdent(op.checkType.type));
        break;

      case BytecodeOp::CHECK_EQUAL:
        std::print(str, "CHECK_EQUAL o{}, o{}\n", op.checkEqual.lhsReg,
                   op.checkEqual.rhsReg);
        break;

      case BytecodeOp::CHECK_ITER_EQUAL:
        std::print(str, "CHECK_ITER_EQUAL o{}, o{}\n", op.checkEqual.lhsReg,
                   op.checkEqual.rhsReg);
        break;

      case BytecodeOp::CHECK_SIZE_GE:
        std::print(str, "CHECK_SIZE_GE r{}, #{}\n",
                   op.checkSizeGreater.containerReg, op.checkSizeGreater.elems);
        break;

      case BytecodeOp::CHECK_SIZE_LE:
        std::print(str, "CHECK_SIZE_LE r{}, #{}\n",
                   op.checkSizeLess.containerReg, op.checkSizeLess.elems);
        break;

      case BytecodeOp::NEXT:
        std::print(str, "NEXT r{}, r{}\n", op.arith.outReg, op.arith.srcA);
        break;

      case BytecodeOp::GET_BEGIN: {
        std::print(str, "GET_BEGIN r{}, r{}\n", op.getBegin.dstReg,
                   op.getBegin.containerReg);
        break;
      }
      case BytecodeOp::GET_END: {
        std::print(str, "GET_END r{}, r{}\n", op.getEnd.dstReg,
                   op.getEnd.containerReg);
        break;
      }

      case BytecodeOp::LOOP:
      case BytecodeOp::LOOP_FOR:
        std::print(str, "{} #{}, r{}, r{}, r{}\n",
                   op.opcode == BytecodeOp::LOOP ? "LOOP" : "LOOP_FOR",
                   op.scanForward.bodyLen, op.scanForward.start,
                   op.scanForward.cur, op.scanForward.end);

        indentStack.emplace_back(i + op.scanForward.bodyLen + 1);
        break;

      case BytecodeOp::COPY_OPERANDS:
        std::print(str, "COPY_OPERANDS i{}, r{}, r{}\n",
                   op.copyOperands.dstInstr, op.copyOperands.begin,
                   op.copyOperands.end);
        break;
      case BytecodeOp::COPY_OPERAND:
        std::print(str, "COPY_OPERAND i{}, r{}\n", op.copyOperand.dstInstr,
                   op.copyOperand.src);
        break;

      case BytecodeOp::SET_OPERANDS_OTHER:
        std::print(str, "SET_OPERANDS_OTHER i{}\n", op.setOperandsOther.instr);
        break;

      case BytecodeOp::BREAK:
        std::print(str, "BREAK\n");
        break;

      case BytecodeOp::CONTINUE:
        std::print(str, "CONTINUE\n");
        break;

      case BytecodeOp::REPLACE_ALL_USES:
        std::print(str, "REPLACE_ALL_USES r{}, r{}\n", op.replAllUses.oldOp,
                   op.replAllUses.newOp);
        break;

      case BytecodeOp::CREATE_CONSTANT:
        std::print(str, "CREATE_CONSTANT r{}, #", op.createConstant.reg);
        op.createConstant.bigInt->toStream(str, 16);
        std::print(str, "\n");
        break;

      case BytecodeOp::CREATE_CONSTANT_LIKE_SIGNED:
        std::print(str, "CREATE_CONSTANT_LIKE_SIGNED r{}, r{}, #",
                   op.createConstantLike.reg, op.createConstantLike.like);
        op.createConstantLike.bigInt->toStream(str, 16);
        std::print(str, "\n");
        break;

      case BytecodeOp::CREATE_CONSTANT_LIKE_UNSIGNED:
        std::print(str, "CREATE_CONSTANT_LIKE_UNSIGNED r{}, r{}, #",
                   op.createConstantLike.reg, op.createConstantLike.like);
        op.createConstantLike.bigInt->toStream(str, 16);
        std::print(str, "\n");
        break;

      case BytecodeOp::CREATE_INSTR:
        std::print(str, "CREATE_INSTR i{}, {}\n", op.createInstr.outReg,
                   lexer.GetIdent(op.createInstr.opcode));
        break;
      case BytecodeOp::CREATE_INSTR_LIKE:
        std::print(str, "CREATE_INSTR_LIKE i{}, i{}\n",
                   op.createInstrLike.outReg, op.createInstr.outReg);
        break;

      case BytecodeOp::GET_DEF_INSTR:
        std::print(str, "GET_DEF_INSTR i{}, o{}\n", op.getDefInstr.dstReg,
                   op.getDefInstr.operand);
        break;

      case BytecodeOp::APPEND_COPY:
        std::print(str, "APPEND_COPY l{}, r{}\n", op.appendCopy.dstList,
                   op.appendCopy.srcContainer);
        break;

      case BytecodeOp::CHECK_CONSTANT_SIGNED: {
        std::print(str, "CHECK_CONSTANT_SIGNED r{}, #\n",
                   op.checkConstant.operand);
        op.checkConstant.bigInt->toStream(str, 16);
        std::print(str, "\n");
        break;
      }
      case BytecodeOp::CHECK_CONSTANT_UNSIGNED: {
        std::print(str, "CHECK_CONSTANT_UNSIGNED r{}, #\n",
                   op.checkConstant.operand);
        op.checkConstant.bigInt->toStream(str, 16);
        std::print(str, "\n");
        break;
      }

      case BytecodeOp::CHECK_CONSTANT_EXACT: {
        std::print(str, "CHECK_CONSTANT r{}, #", op.checkConstantExact.operand);
        op.checkConstantExact.bigInt->toStream(str, 16);
        std::print(str, "\n");
        break;
      }

      case BytecodeOp::INLINE_CODE:
        std::print(str, "INLINE_CODE [{{\n");
        std::print(str, "{}\n]}}\n", op.inlineCode.str);
        break;

      case BytecodeOp::DELETE:
        std::print(str, "DELETE r{}\n", op.deleteI.instr);
        break;
      case BytecodeOp::DELETE_IF_SINGLE_USE:
        std::print(str, "DELETE_IF_SINGLE_USE r{}\n", op.deleteI.instr);
        break;

      default:
        dyno_unreachable("");
      }
    }

    os << str.str();
  }
};

struct CPPBackend {
  Lexer &lexer;
  std::ostream &os;
  CodeGen &code;
  uint patternIdx;

  std::stringstream macroExpandInlineCode(std::string_view inlineCode,
                                          std::string failLabel) {
    std::stringstream copy;
    auto delimdIdent = std::regex(R"(\$[a-zA-Z_][a-zA-Z0-9_]*)");
    std::regex_iterator it(inlineCode.begin(), inlineCode.end(), delimdIdent);
    std::regex_iterator<std::string_view::iterator> end{};

    auto replacer = [&](std::string_view str) -> std::string {
      if (str.substr(1) == "fail")
        return failLabel;

      auto it = code.vars.find(lexer.GetIdentIdx(str.substr(1)));
      assert(it != code.vars.end());
      return "RefRange{r" + std::to_string(it.val().operand) + ", r" +
             std::to_string(it.val().end) + "}";
    };
    size_t last_pos = 0;
    while (it != end) {
      copy << inlineCode.substr(last_pos, it->position() - last_pos);
      copy << replacer(it->str());
      last_pos = it->position() + it->length();

      it++;
    }
    copy << inlineCode.substr(last_pos);
    return copy;
  }

  const char *typeStr(uint i) {
    static constexpr auto arr =
        std::to_array({"InstrRef::iterator", "InstrRef", "BlockRef",
                       "SmallVec<FatDynObjRef<>, 4>", "FatDynObjRef<>*",
                       "InstrBuilder", "ConstantRef"});
    return arr[(size_t)code.regTypes[i]];
  }

  void dumpVars() {
    for (auto [i, regT] : Range{code.regTypes}.enumerate().drop_front()) {
      if (regT != CodeGen::RegType::REPL_INSTR) {
        std::print(os, "{} r{};\n", typeStr(i), i);
      }
    }
  }

  auto getNumOpsExpr(BytecodeOp *create) {
    assert(create->opcode == BytecodeOp::CREATE_INSTR ||
           create->opcode == BytecodeOp::CREATE_INSTR_LIKE);
    size_t idx = create - code.ops.begin().base();
    std::stringstream expr;

    auto instrReg = create->opcode == BytecodeOp::CREATE_INSTR
                        ? create->createInstr.outReg
                        : create->createInstrLike.outReg;

    bool first = true;
    for (size_t i = idx + 1; i < code.ops.size(); i++) {
      if (code.ops[i].opcode == BytecodeOp::COPY_OPERANDS &&
          code.ops[i].copyOperands.dstInstr == instrReg) {
        std::print(expr, "{}(r{}-r{})", first ? "" : "+",
                   code.ops[i].copyOperands.end,
                   code.ops[i].copyOperands.begin);
        first = false;
      } else if (code.ops[i].opcode == BytecodeOp::COPY_OPERAND &&
                 code.ops[i].copyOperand.dstInstr == instrReg) {
        std::print(expr, "{}1", first ? "" : "+");
        first = false;
      }
    }
    return expr;
  }

  void dump() {

    std::print(os, "{{\n");

    dumpVars();

    struct Loop {
      uint endIdx;
      uint id;
    };
    SmallVec<Loop, 4> loopStack;

    struct Continue {
      uint32_t endIdx;
      uint32_t id;
    };
    SmallVec<Continue, 4> contStack;

    struct Goto {
      uint32_t endIdx;
      uint32_t id;
    };
    SmallVec<Goto, 4> gotoStack;

    uint loopIdCnt = 0;
    uint contIdCnt = 0;
    uint gotoIdCnt = 0;

    auto getFailLabel = [&]() {
      std::stringstream str;
      if (!contStack.empty()) {
        std::print(str, "cont_{}_{}", patternIdx, contStack.back().id);
        return str.str();
      }
      std::print(str, "fail_{}", patternIdx);
      return str.str();
    };

    auto emitFail = [&]() { std::print(os, "goto {};\n", getFailLabel()); };

    auto operandPrefix = [](uint32_t opIdx) {
      std::stringstream str;
      std::print(str, "r{}", opIdx);
      return str.str();
    };
    auto checkType = [&](uint32_t opIdx, uint32_t typeID) {
      std::print(os, "if ((*{})->as<FatDynObjRef<>>().getType() != {}) ",
                 operandPrefix(opIdx), lexer.GetIdent(typeID));
      emitFail();
    };

    for (size_t i = 0; i < code.ops.size(); i++) {
      auto &op = code.ops[i];

      // continue inside loop.
      if (!contStack.empty() && contStack.back().endIdx == i) {
        std::print(os, "cont_{}_{}:;\n", patternIdx, contStack.back().id);
        contStack.pop_back();
      }

      if (!loopStack.empty() && loopStack.back().endIdx == i) {
        std::print(os, "loop_{}_{}_continue:;\n", patternIdx,
                   loopStack.back().id);
        std::print(os, "}}\n");
        std::print(os, "loop_{}_{}_break:;\n", patternIdx, loopStack.back().id);
        loopStack.pop_back();
      }

      while (!gotoStack.empty() && gotoStack.back().endIdx == i) {
        std::print(os, "goto_{}_{}:;\n", patternIdx, gotoStack.back().id);
        gotoStack.pop_back();
      }

      switch (op.opcode) {

      case BytecodeOp::PUSH_CONTINUE: {
        contStack.emplace_back(uint32_t(op.pushContinue.len + i + 1),
                               uint32_t(contIdCnt++));
        break;
      }

      case BytecodeOp::GOTO: {
        gotoStack.emplace_back(uint32_t(op.gotoOp.len + i + 1),
                               uint32_t(gotoIdCnt++));
        std::print(os, "goto goto_{}_{};\n", patternIdx, gotoStack.back().id);
        break;
      }

      case BytecodeOp::POP_CONTINUE: {
        break;
      }

      case BytecodeOp::CHECK_OPCODE:
        std::print(os, "if (!r{}.isOpc({})) ", op.checkOpcode.instr,
                   lexer.GetIdent(op.checkOpcode.opcode));
        emitFail();
        break;

      case BytecodeOp::CHECK_TYPE:
        checkType(op.checkType.operand, op.checkType.type);
        break;

      case BytecodeOp::CHECK_EQUAL: {
        auto getObjRef = [this](uint i) {
          switch (this->code.regTypes[i]) {
          case CodeGen::RegType::OPERAND:
            return "(*r" + std::to_string(i) + ")->fat()";
          case CodeGen::RegType::LIST_ITER:
            return "*r" + std::to_string(i) + "";
          default:
            assert(0);
          }
        };
        std::print(os, "if ({} != {}) ", getObjRef(op.checkEqual.lhsReg),
                   getObjRef(op.checkEqual.rhsReg));
        emitFail();
        break;
      }

      case BytecodeOp::CHECK_ITER_EQUAL: {
        std::print(os, "if (r{} != r{}) ", op.checkEqual.lhsReg,
                   op.checkEqual.rhsReg);
        emitFail();
        break;
      }

      case BytecodeOp::CHECK_SIZE_GE:
        if (code.regTypes[op.checkSizeGreater.containerReg] ==
            CodeGen::RegType::INSTR) {
          std::print(os, "if (r{}.getNumOperands() < {}) ",
                     op.checkSizeGreater.containerReg, op.checkSizeLess.elems);
        } else {
          std::print(os, "if (r{}.size() < {}) ",
                     op.checkSizeGreater.containerReg,
                     op.checkSizeGreater.elems);
        }
        emitFail();
        break;

      case BytecodeOp::CHECK_SIZE_LE:
        if (code.regTypes[op.checkSizeLess.containerReg] ==
            CodeGen::RegType::INSTR) {
          std::print(os, "if (r{}.getNumOperands() > {}) ",
                     op.checkSizeLess.containerReg, op.checkSizeLess.elems);
        } else {
          std::print(os, "if (r{}.size() > {}) ", op.checkSizeLess.containerReg,
                     op.checkSizeLess.elems);
        }
        emitFail();
        break;

      case BytecodeOp::GET_BEGIN: {
        std::print(os, "r{} = r{}.begin();\n", op.getBegin.dstReg,
                   op.getBegin.containerReg);
        break;
      }
      case BytecodeOp::GET_END: {
        std::print(os, "r{} = r{}.end();\n", op.getEnd.dstReg,
                   op.getEnd.containerReg);
        break;
      }
      case BytecodeOp::NEXT: {
        std::print(os, "r{} = std::next(r{});\n", op.arith.outReg,
                   op.arith.srcA);
        break;
      }

      case BytecodeOp::LOOP:
      case BytecodeOp::LOOP_FOR:
        std::print(os, "for (r{0} = r{1};; r{0}++) {{\nif (r{0} == r{2}) ",
                   op.scanForward.cur, op.scanForward.start,
                   op.scanForward.end);

        if (op.opcode == BytecodeOp::LOOP)
          emitFail();
        else
          std::print(os, "break;\n");
        loopStack.emplace_back(op.scanForward.bodyLen + i + 1, loopIdCnt++);
        break;

      case BytecodeOp::COPY_OPERANDS: {
        std::print(os, "copyOperands(r{}, r{}, r{});\n",
                   op.copyOperands.dstInstr, op.copyOperands.begin,
                   op.copyOperands.end);
        break;
      }
      case BytecodeOp::COPY_OPERAND: {
        std::print(os, "copyOperand(r{}, r{});\n", op.copyOperand.dstInstr,
                   op.copyOperand.src);
        break;
      }

      case BytecodeOp::SET_OPERANDS_OTHER:
        std::print(os, "r{}.other();\n", op.setOperandsOther.instr);
        break;

      case BytecodeOp::REPLACE_ALL_USES:
        std::print(os, "replaceAllUses(replaced, r{}, r{});\n",
                   op.replAllUses.oldOp, op.replAllUses.newOp);
        break;

      case BytecodeOp::CREATE_CONSTANT:
        std::print(os, "r{} = cbuild.val(\"", op.createConstant.reg);
        op.createConstant.bigInt->toStream(os, 16);
        std::print(os, "\"_bv).get();\n");
        break;

      case BytecodeOp::CREATE_CONSTANT_LIKE_UNSIGNED:
      case BytecodeOp::CREATE_CONSTANT_LIKE_SIGNED: {
        bool isSigned = op.opcode == BytecodeOp::CREATE_CONSTANT_LIKE_SIGNED;
        std::print(os, "r{} = cbuild.val(\"", op.createConstantLike.reg);
        op.createConstantLike.bigInt->toStream(os, 16);
        std::print(os,
                   "\"_bv).resize(*(*r{})->as<HWValue>().getNumBits(), {});\n",
                   op.createConstantLike.like, isSigned ? "true" : "false");
        break;
      }

      case BytecodeOp::CREATE_INSTR: {
        std::print(os, "auto r{} = build.buildInstrRaw({}, {});\n",
                   op.createInstr.outReg, lexer.GetIdent(op.createInstr.opcode),
                   getNumOpsExpr(&op).view());
        break;
      }

      case BytecodeOp::CREATE_INSTR_LIKE: {
        std::print(
            os, "auto r{} = build.buildInstrRaw(r{}.getDialectOpcode(), {});\n",
            op.createInstrLike.outReg, op.createInstrLike.likeReg,
            getNumOpsExpr(&op).view());
        break;
      }

      case BytecodeOp::BREAK:
        std::print(os, "goto loop_{}_{}_break;\n", patternIdx,
                   loopStack.back().id);
        break;

      case BytecodeOp::CONTINUE:
        std::print(os, "goto loop_{}_{}_continue;\n", patternIdx,
                   loopStack.back().id);
        break;

      case BytecodeOp::GET_DEF_INSTR:
        std::print(os, "r{} = getDefInstr(r{});\n", op.getDefInstr.dstReg,
                   op.getDefInstr.operand);
        break;

      case BytecodeOp::APPEND_COPY:
        std::print(os, "appendCopy(r{}, r{});\n", op.appendCopy.dstList,
                   op.appendCopy.srcContainer);
        break;

      case BytecodeOp::CHECK_CONSTANT_SIGNED: {
        std::print(os, "if (!(*r{})->as<ConstantRef>().valueEqualsS(\"",
                   op.checkConstant.operand);
        op.checkConstant.bigInt->toStream(os, 16);
        std::print(os, "\"_bv)) ");
        emitFail();
        break;
      }
      case BytecodeOp::CHECK_CONSTANT_UNSIGNED: {
        std::print(os, "if (!(*r{})->as<ConstantRef>().valueEquals(\"",
                   op.checkConstant.operand);
        op.checkConstant.bigInt->toStream(os, 16);
        std::print(os, "\"_bv)) ");
        emitFail();
        break;
      }

      case BytecodeOp::CHECK_CONSTANT_EXACT: {
        std::print(os, "if ((*r{})->as<ConstantRef>() != \"",
                   op.checkConstantExact.operand);
        op.checkConstantExact.bigInt->toStream(os, 16);
        std::print(os, "\"_bv) ");
        emitFail();
        break;
      }

      case BytecodeOp::INLINE_CODE: {
        std::print(
            os, "{}",
            macroExpandInlineCode(op.inlineCode.str, getFailLabel()).str());
        break;
      }

      case BytecodeOp::DELETE:
        std::print(os, "deleteF(matched, ctx, r{});\n", op.deleteI.instr);
        break;
      case BytecodeOp::DELETE_IF_SINGLE_USE:
        std::print(os, "deleteIfSingleUse(matched, ctx, r{});\n",
                   op.deleteI.instr);
        break;

      default:
        dyno_unreachable("");
      }
    }
    while (!loopStack.empty()) {
      std::print(os, "}}\n");
      loopStack.pop_back();
    }

    std::print(os, "return true;\n}}\nfail_{}:;\n", patternIdx);
  }
};

int main(int argc, char **argv) {

  if (argc != 3) {
    fprintf(stderr, "usage: %s <input file> <output file>\n", argv[0]);
    exit(-1);
  }
  Lexer lexer{readFileIntoStr(argv[1]), argv[1], Operators, Keywords};

  uint patternIdx = 0;

  std::ofstream ofile(argv[2]);
  while (!lexer.popIf(Token::NONE)) {

    if (lexer.peekIs(kw_macro)) {
      parseMacro(lexer);
      continue;
    }

    auto pat = parsePattern(lexer);
    CodeGen cg{lexer, std::cout};
    cg.generatePattern(pat);
    std::cout << "\n\n";
    cg.dump();

    std::cout << "\n\n";

    CPPBackend backend{lexer, ofile, cg, patternIdx};
    backend.dump();

    patternIdx++;
  }
}
