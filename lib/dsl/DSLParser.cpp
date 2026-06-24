#include "dsl/DSLParser.h"
#include "dsl/IDs.h"
#include "dyno/Opcode.h"
#include "op/IDs.h"
#include "support/Debug.h"
#include "support/MacroUtil.h"

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
    return PREC_RELATIONAL;
  case *OP_ICMP_EQ:
  case *OP_ICMP_NE:
    return PREC_EQUALITY;
  case *OP_AND:
    return PREC_LOG_AND;
  case *OP_OR:
    return PREC_LOG_OR;
  case *DSL_ASSIGN:
    return PREC_ASSIGN;
  case *DSL_TUPLE:
    return PREC_TUPLE;
  case *DSL_FUNC:
    return PREC_FUNC;
  case *DSL_ACCESS:
    return PREC_ACCESS;
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
  case DSLLexer::op_dot:
    return DSL_ACCESS;
  default:
    return DialectOpcode::invalid();
  }
}

constexpr bool DSLParser::isRightAssoc(Precedence prec) {
  return prec == PREC_ASSIGN || prec == PREC_FUNC;
}

DSLParser::ParseResult DSLParser::parseUnary() {
  DYNO_EXPECT(tok, lex.peekExpect());
  switch (tok.type) {
  case DSLLexer::op_rbropen: {
    lex.Pop();
    DYNO_EXPECT(expr, parseExpr());
    DYNO_EXPECT(lex.popExpect(DSLLexer::op_rbrclose));
    return expr;
  }
  case Token::INT_LITERAL:
    lex.Pop();
    return ConstantRef::fromU32(tok.intLit.value);
  case Token::BIG_INT_LITERAL:
    lex.Pop();
    return ctx.getStore<Constant>().findOrInsert(*tok.bigIntLit.value);
  case Token::IDENTIFIER: {
    lex.Pop();
    std::string_view ident = lex.GetIdent(tok.ident.idx);
    if (ident == "true")
      return ConstantRef::fromBool(true);
    if (ident == "false")
      return ConstantRef::fromBool(false);
    return ctx.getStore<StringObj>().create(std::string(ident));
  }
  case Token::STRING_LITERAL: {
    lex.Pop();
    auto str = ctx.getStore<StringObj>().create(std::string(tok.strLit.value));
    return build.buildOp(DSL_STRING_LIT, {str}).def()->fat();
  }
  case DSLLexer::kw_struct: {
    lex.Pop();
    DYNO_EXPECT(blk, parseBlock());
    return build.buildStruct(blk.as<BlockRef>()).def()->fat();
  }
  case DSLLexer::kw_if: {
    lex.Pop();
    DYNO_EXPECT(cond, parseExpr());
    DYNO_EXPECT(trueBlk, parseBlock());
    if (lex.popIf(DSLLexer::kw_else)) {
      DYNO_EXPECT(falseBlk, parseBlock());
      build.buildIfElse(cond, trueBlk.as<BlockRef>(), falseBlk.as<BlockRef>());
      return dsl_nullref;
    }
    build.buildIf(cond, trueBlk.as<BlockRef>());
    return dsl_nullref;
  }
  // case DSLLexer::kw_for: {
  //   DYNO_EXPECT(cond, parseExpr());
  //   DYNO_EXPECT(trueBlk, parseBlock());
  //   DYNO_EXPECT(lex.popExpect(DSLLexer::kw_else));
  //   DYNO_EXPECT(falseBlk, parseBlock());
  //   return cond;
  // }
  default:
    return error("unexpected token in expression");
  }

  DialectOpcode unaryOpc = getUnaryOpc(tok);
  if (!unaryOpc)
    return error("Expected unary op");
  lex.Pop();
  DYNO_EXPECT(operand, parseUnary());
  if (unaryOpc == OP_SUB) {
    return build.buildOp(OP_SUB, {ConstantRef::zeroBitZero(), operand})
        .def()
        ->fat();
  }
  return build.buildOp(unaryOpc, {operand}).def()->fat();
}

DSLParser::ParseResult DSLParser::parsePostfix(DSLValue val) {
  if (val.is<DSLNullRef>() || lex.peekIs(DSLLexer::op_semicolon))
    return val;

  return val;
}

DSLParser::ParseResult DSLParser::parseExpr(Precedence prec) {
  DYNO_EXPECT(lhs, parseUnary());
  while (true) {
    Token tok = lex.Peek();
    if (tok.type == DSLLexer::op_qmark && prec <= PREC_TERNARY) {
      lex.Pop();
      auto ifI =
          build.buildIfElse(lhs.as<DSLValue>(), ctx.getCFG().createBlock(),
                            ctx.getCFG().createBlock(), 1);
      build.pushInsertIt(ifI.getTrueBlock());
      DYNO_EXPECT(ternaryLhs, parseExpr());
      build.buildYield(ternaryLhs);
      build.popInsertIt();
      build.pushInsertIt(ifI.getFalseBlock());
      DYNO_EXPECT(lex.popExpect(DSLLexer::op_colon));
      DYNO_EXPECT(ternaryRhs, parseExpr());
      build.buildYield(ternaryRhs);
      build.popInsertIt();
      return ifI.getYieldValue()->fat();
    }
    DialectOpcode opc = getBinaryOpc(tok);
    Precedence opcPrec = getPrecedence(opc);
    if (!opc || opcPrec < prec)
      return lhs;
    lex.Pop();
    Precedence newPrec =
        isRightAssoc(opcPrec) ? opcPrec : Precedence(opcPrec + 1);
    DYNO_EXPECT(rhs, parseExpr(newPrec));
    lhs = build.buildOp(opc, {lhs, rhs}).def()->fat();
  }
  return lhs;
}
