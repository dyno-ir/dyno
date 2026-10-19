#include "dsl/DSLParser.h"
#include "dsl/DSLInstr.h"
#include "op/IDs.h"
#include <algorithm>
#include <limits>

using namespace dyno;

constexpr DSLParser::Precedence DSLParser::getPrecedence(DialectOpcode opc) {
  switch (*opc) {
  case *OP_MUL:
  case *OP_UDIV:
    return PREC_MUL;
  case *OP_ADD:
  case *OP_SUB:
    return PREC_ADD;
  case *OP_SLL:
  case *OP_SRL:
    return PREC_SHIFT;
  case *OP_ICMP_ULT:
  case *OP_ICMP_UGT:
  case *OP_ICMP_ULE:
  case *OP_ICMP_UGE:
    return PREC_RELATIONAL;
  case *OP_ICMP_EQ:
  case *OP_ICMP_NE:
    return PREC_EQUALITY;
  case *OP_AND:
    return PREC_BIT_AND;
  case *OP_OR:
    return PREC_BIT_OR;
  case *DSL_ASSIGN:
    return PREC_ASSIGN;
  case *DSL_TUPLE:
    return PREC_TUPLE;
  case *DSL_FUNC:
    return PREC_FUNC;
  case *DSL_APPLY:
    return PREC_APPLY;
  default:
    return PREC_NONE;
  }
}

constexpr DialectOpcode DSLParser::getUnaryOpc(const Token &tok) {
  switch (tok.type) {
  case DSLLexer::op_minus:
    return OP_SUB;
  case DSLLexer::op_exmark:
    return OP_NOT;
  default:
    return DialectOpcode::invalid();
  }
}

constexpr DialectOpcode DSLParser::getBinaryOpc(const Token &tok) {
  switch (tok.type) {
  case DSLLexer::op_plus:
    return OP_ADD;
  case DSLLexer::op_minus:
    return OP_SUB;
  case DSLLexer::op_star:
    return OP_MUL;
  case DSLLexer::op_and:
    return OP_AND;
  case DSLLexer::op_or:
    return OP_OR;
  case DSLLexer::op_lt:
    return OP_ICMP_ULT;
  case DSLLexer::op_gt:
    return OP_ICMP_UGT;
  case DSLLexer::op_lte:
    return OP_ICMP_ULE;
  case DSLLexer::op_gte:
    return OP_ICMP_UGE;
  case DSLLexer::op_lsh:
    return OP_SLL;
  case DSLLexer::op_rsh:
    return OP_SRL;
  case DSLLexer::op_eqeq:
    return OP_ICMP_EQ;
  case DSLLexer::op_neq:
    return OP_ICMP_NE;
  case DSLLexer::op_eq:
    return DSL_ASSIGN;
  case DSLLexer::op_comma:
    return DSL_TUPLE;
  case DSLLexer::op_rarrow:
    return DSL_FUNC;
  default:
    return DialectOpcode::invalid();
  }
}

constexpr bool DSLParser::isRightAssoc(Precedence prec) {
  return prec == PREC_ASSIGN || prec == PREC_FUNC;
}

SimpleDebugInfo DSLParser::peekSource() {
  lex.Peek();
  auto position = [&](size_t offset) {
    auto it = std::upper_bound(lineStarts.begin(), lineStarts.end(), offset);
    auto index = size_t(it - lineStarts.begin() - 1);
    return std::pair{uint32_t(index + 1),
                     uint32_t(offset - lineStarts[index] + 1)};
  };
  auto [line, col] = position(lex.lastState.i);
  auto [endLine, endCol] = position(lex.state.i);
  return {true, lex.path, line, col, endLine, endCol};
}

static DSLTypeRef getBuiltinType(uint32_t token) {
  switch (token) {
  case DSLLexer::kw_int:
  case DSLLexer::kw_int32:
    return DSLIntRef(32);
  case DSLLexer::kw_uint:
  case DSLLexer::kw_uint32:
    return DSLIntRef(32, false);
  case DSLLexer::kw_int8:
    return DSLIntRef(8);
  case DSLLexer::kw_int16:
    return DSLIntRef(16);
  case DSLLexer::kw_int64:
    return DSLIntRef(64);
  case DSLLexer::kw_uint8:
    return DSLIntRef(8, false);
  case DSLLexer::kw_uint16:
    return DSLIntRef(16, false);
  case DSLLexer::kw_uint64:
    return DSLIntRef(64, false);
  case DSLLexer::kw_bool:
    return dsl_bool;
  case DSLLexer::kw_string:
    return dsl_string;
  case DSLLexer::kw_typename:
    return dsl_typename;
  default:
    return nullref;
  }
}

DSLParser::ParseResult DSLParser::parseAtom() {
  DYNO_EXPECT(tok, lex.peekExpect());
  auto source = sourceScope(peekSource());
  if (DSLTypeRef builtin = getBuiltinType(tok.type)) {
    lex.Pop();
    return builtin;
  }
  switch (tok.type) {
  case DSLLexer::op_rbropen: {
    lex.Pop();
    if (lex.popIf(DSLLexer::op_rbrclose))
      return dsl_unit;
    DYNO_EXPECT(expr, parseExpr());
    DYNO_EXPECT(lex.popExpect(DSLLexer::op_rbrclose));
    return expr;
  }
  case Token::INT_LITERAL: {
    if (tok.intLit.value > uint32_t(std::numeric_limits<int32_t>::max()))
      return error("integer literal does not fit int32; use a sized literal");
    lex.Pop();
    return build
        .buildOp(DSL_INT_LIT,
                 {ConstantRef::fromU32(tok.intLit.value),
                  ConstantRef::fromBool(true), ConstantRef::fromBool(true)})
        .def()
        ->fat();
  }
  case Token::BIG_INT_LITERAL: {
    lex.Pop();
    auto constant = ctx.getStore<Constant>().findOrInsert(*tok.bigIntLit.value);
    return build
        .buildOp(DSL_INT_LIT,
                 {constant, ConstantRef::fromBool(tok.bigIntLit.isSigned),
                  ConstantRef::fromBool(tok.bigIntLit.isUnsized)})
        .def()
        ->fat();
  }
  case DSLLexer::kw_true:
  case DSLLexer::kw_false:
    lex.Pop();
    return build
        .buildOp(DSL_BOOL_LIT,
                 {ConstantRef::fromBool(tok.type == DSLLexer::kw_true)})
        .def()
        ->fat();
  case Token::IDENTIFIER: {
    lex.Pop();
    std::string_view ident = lex.GetIdent(tok.ident.idx);
    auto name = ctx.getStore<StringObj>().create(std::string(ident));
    return build.buildOp(DSL_VAR, {name}).def()->fat();
  }
  case Token::STRING_LITERAL: {
    lex.Pop();
    auto str = ctx.getStore<StringObj>().create(std::string(tok.strLit.value));
    return build.buildOp(DSL_STRING_LIT, {str}).def()->fat();
  }
  case DSLLexer::op_cbropen: {
    DYNO_EXPECT(block, parseBlock());
    return build.buildBlock(block.as<BlockRef>()).def()->fat();
  }
  case DSLLexer::kw_struct: {
    lex.Pop();
    DYNO_EXPECT(blk, parseBlock());
    return build.buildStruct(blk.as<BlockRef>()).def()->fat();
  }
  case DSLLexer::kw_if: {
    lex.Pop();
    DYNO_EXPECT(cond, parseExpr(PREC_NONE, ExprContext::ControlHeader));
    DYNO_EXPECT(trueBlk, parseBlock());
    if (lex.popIf(DSLLexer::kw_else)) {
      DYNO_EXPECT(falseBlk, parseBlock());
      auto instr = build.buildIfElse(cond, trueBlk.as<BlockRef>(),
                                     falseBlk.as<BlockRef>(), 1);
      return instr.getYieldValue()->fat();
    }
    build.buildIf(cond, trueBlk.as<BlockRef>());
    return dsl_unit;
  }
  default:
    return error("unexpected token in expression");
  }
}

DSLParser::ParseResult DSLParser::parseUnary() {
  SimpleDebugInfo loc = peekSource();
  DialectOpcode unary = getUnaryOpc(lex.Peek());
  if (unary) {
    lex.Pop();
    DYNO_EXPECT(operand, parseUnary());
    auto source = sourceScope(loc);
    return build.buildOp(unary, {operand}).def()->fat();
  }
  DYNO_EXPECT(value, parseAtom());
  return parseTightPostfix(value);
}

DSLParser::ParseResult DSLParser::parseTightPostfix(DSLValue value) {
  while (true) {
    auto source = sourceScope(peekSource());
    if (lex.popIf(DSLLexer::op_dot)) {
      DYNO_EXPECT(name, lex.peekExpect());
      if (name.type != Token::IDENTIFIER)
        return error("expected a member name");
      auto label = ctx.getStore<StringObj>().create(
          std::string(std::string_view(lex.GetIdent(name.ident.idx))));
      lex.Pop();
      value = build.buildOp(DSL_ACCESS, {value, label}).def()->fat();
      continue;
    }
    if (!lex.peekIs(DSLLexer::op_rbropen) ||
        lex.lastState.i != lex.preWhitespaceState.i)
      return value;
    DYNO_EXPECT(arg, parseAtom());
    value = build.buildOp(DSL_APPLY, {value, arg}).def()->fat();
  }
}

DSLParser::ParseResult DSLParser::parseFunction(DSLValue pattern,
                                                Precedence prec) {
  BlockRef body;
  if (lex.peekIs(DSLLexer::op_cbropen)) {
    DYNO_EXPECT(parsed, parseBlock());
    body = parsed.as<BlockRef>();
  } else {
    body = build.cfg.createBlock();
    auto insertion = build.changeInsertIt(body);
    DYNO_EXPECT(result, parseExpr(prec));
    build.buildYield(result);
  }
  {
    auto insertion = build.changeInsertIt(body.begin());
    InstrRef instr = build.buildParam(pattern);
    if (auto value = pattern.dyn_as<DSLValRef>()) {
      // Attribute PARAM to its pattern rather than the function arrow.
      SourceLocInfo<Instr> &sources =
          ctx.getCtx<CoreDialectContext>().instrSourceLocInfo;
      sources.resetDebugInfo(instr);
      sources.copyDebugInfo(value.getDefI(), instr);
    }
  }
  return build.buildFunc(body).def()->fat();
}

bool DSLParser::startsArg(const Token &tok) {
  // TODO: Add !/enum/while/for/list/match.
  // - is sub and { is struct instance. need to be paranthesized as arg.
  switch (tok.type) {
  case Token::IDENTIFIER:
  case Token::INT_LITERAL:
  case Token::BIG_INT_LITERAL:
  case Token::STRING_LITERAL:
  case DSLLexer::op_rbropen:
  case DSLLexer::kw_struct:
  case DSLLexer::kw_if:
    return true;
  default:
    return DSLLexer::isBuiltinVal(tok.type);
  }
}

DSLParser::ParseResult DSLParser::parseExpr(Precedence prec,
                                            ExprContext context) {
  DYNO_EXPECT(lhs, parseUnary());
  DialectOpcode prevOpc = DialectOpcode::invalid();
  SimpleDebugInfo opLoc;
  SmallVec<DSLValue, 4> operands{lhs};
  auto buildOp = [&]() -> DSLValue {
    if (operands.size() == 1)
      return operands[0];
    auto source = sourceScope(opLoc);
    DSLValue value = build.buildOp(prevOpc, operands).def()->fat();
    operands.clear();
    operands.push_back(value);
    prevOpc = DialectOpcode::invalid();
    return value;
  };
  while (true) {
    Token tok = lex.Peek();
    SimpleDebugInfo loc = peekSource();
    if (tok.type == DSLLexer::op_cbropen && prec <= PREC_APPLY &&
        context == ExprContext::Normal) {
      DSLValue target = buildOp();
      auto source = sourceScope(loc);
      DYNO_EXPECT(body, parseBlock());
      DSLValue instance =
          build.buildInstance(target, body.as<BlockRef>()).def()->fat();
      DYNO_EXPECT(value, parseTightPostfix(instance));
      operands[0] = value;
      continue;
    }
    DialectOpcode opc = getBinaryOpc(tok);
    bool application = !opc && startsArg(tok);
    if (application)
      opc = DSL_APPLY;
    Precedence opcPrec = getPrecedence(opc);
    if (!opc || opcPrec < prec)
      return buildOp();
    if (prevOpc && (prevOpc != opc || opc != DSL_TUPLE))
      buildOp();
    if (!application)
      lex.Pop();
    Precedence newPrec =
        isRightAssoc(opcPrec) ? opcPrec : Precedence(opcPrec + 1);
    auto source = sourceScope(loc);
    if (opc == DSL_FUNC) {
      DYNO_EXPECT(function, parseFunction(buildOp(), newPrec));
      operands[0] = function;
      continue;
    }
    DYNO_EXPECT(rhs, parseExpr(newPrec, context));
    prevOpc = opc;
    opLoc = loc;
    operands.push_back(rhs);
  }
}

DSLParser::ParseResult DSLParser::parseStmt() {
  auto source = sourceScope(peekSource());
  if (lex.popIf(DSLLexer::kw_return)) {
    DSLValue value = dsl_unit;
    if (!lex.popIf(DSLLexer::op_semicolon)) {
      DYNO_EXPECT(expr, parseExpr());
      value = expr;
      DYNO_EXPECT(lex.popExpect(DSLLexer::op_semicolon));
    }
    build.buildReturn(value);
    return dsl_unit;
  }
  bool isLet = lex.popIf(DSLLexer::kw_let);
  DYNO_EXPECT(expr, parseExpr());
  if (!isLet && lex.peekIs(DSLLexer::op_cbrclose)) {
    build.buildYield(expr);
    return dsl_unit;
  }
  if (!isLet && expr.is<DSLUnitRef>()) {
    lex.popIf(DSLLexer::op_semicolon);
    return dsl_unit;
  }
  DYNO_EXPECT(lex.popExpect(DSLLexer::op_semicolon));
  isLet ? build.buildLet(expr) : build.buildDef(expr);
  return dsl_unit;
}

DSLParser::ParseResult DSLParser::parseStmts() {
  BlockRef blk = build.cfg.createBlock();
  auto insertion = build.changeInsertIt(blk);
  while (!lex.peekIs(Token::NONE, DSLLexer::op_cbrclose)) {
    DYNO_EXPECT(parseStmt());
  }
  return blk;
}

DSLParser::ParseResult DSLParser::parseBlock() {
  auto source = sourceScope(peekSource());
  DYNO_EXPECT(lex.popExpect(DSLLexer::op_cbropen));
  DYNO_EXPECT(res, parseStmts());
  DYNO_EXPECT(lex.popExpect(DSLLexer::op_cbrclose));
  auto block = res.as<BlockRef>();
  InstrRef terminator = block.getTerminator();
  if (!terminator || !terminator.isOpc(OP_YIELD, OP_RETURN)) {
    auto insertion = build.changeInsertIt(block);
    build.buildYield(dsl_unit);
  }
  return res;
}
