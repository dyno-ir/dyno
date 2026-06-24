#pragma once

#include "dsl/DSLBuilder.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "support/MacroUtil.h"
#include <support/Lexer.h>

namespace dyno {

class DSLLexer : public Lexer<true, false, true, false> {
public:
  constexpr static auto Keywords =
      std::array{"let", "if", "else", "for", "struct", "enum", "fn", "while"};
  constexpr static auto Operators =
      std::array{".",  ":",  ",",  "[",  "]", "?", "#",  "(", ")", "{",
                 "}",  ";",  "->", "==", "+", "-", "*",  "&", "|", "<<",
                 ">>", "<=", ">=", "<",  ">", "=", "!=", "!"};
  enum OperatorEnum {
    _op_start = Lexer::TOK_OPS_START - 1,
    op_dot,
    op_colon,
    op_comma,
    op_sbropen,
    op_sbrclose,
    op_qmark,
    op_hash,
    op_rbropen,
    op_rbrclose,
    op_cbropen,
    op_cbrclose,
    op_semicolon,
    op_rarrow,
    op_eqeq,
    op_plus,
    op_minus,
    op_star,
    op_and,
    op_or,
    op_lsh,
    op_rsh,
    op_lte,
    op_gte,
    op_lt,
    op_gt,
    op_eq,
    op_neq,
    op_exmark
  };

  enum KeywordEnum {
    _kw_start = Lexer::TOK_OPS_START + Operators.size() - 1,
    kw_let,
    kw_if,
    kw_else,
    kw_for,
    kw_struct,
    kw_enum,
    kw_fn,
    kw_while,
  };

  DSLLexer(ArrayRef<char> src, std::string &&fileName)
      : Lexer(src, std::move(fileName), Operators, Keywords) {
    this->config.numericParseType = Lexer::Config::NumericParseType::DYNO;
  }
};

class DSLParser {
public:
  using ParseResult = Result<DSLValue, ParseError>;
  enum Precedence : int {
    PREC_NONE = -1,
    PREC_ASSIGN,
    PREC_FUNC,
    PREC_TUPLE,
    PREC_TERNARY,
    PREC_LOG_OR,
    PREC_LOG_AND,
    PREC_BIT_OR,
    PREC_BIT_XOR,
    PREC_BIT_AND,
    PREC_EQUALITY,
    PREC_RELATIONAL,
    PREC_SHIFT,
    PREC_ADD,
    PREC_MUL,
    PREC_ACCESS
  };

  constexpr Precedence getPrecedence(DialectOpcode opc);
  constexpr DialectOpcode getUnaryOpc(const Token &tok);
  constexpr DialectOpcode getBinaryOpc(const Token &tok);
  constexpr bool isRightAssoc(Precedence prec);

  Context &ctx;
  DSLLexer &lex;
  BuilderStack<DSLBuilder> build;

  DSLParser(Context &ctx, DSLLexer &lex) : ctx(ctx), lex(lex), build(ctx) {}

  ParseResult parseUnary();
  ParseResult parsePostfix(DSLValue val);
  ParseResult parseExpr(Precedence prec = PREC_NONE);

  ParseResult parseStmt() {
    bool isLet = false;
    if (lex.popIf(DSLLexer::kw_let))
      isLet = true;
    DYNO_EXPECT(expr, parseExpr());
    if (expr.is<DSLNullRef>())
      return dsl_nullref;
    DYNO_EXPECT(lex.popExpect(DSLLexer::op_semicolon));
    isLet ? build.buildLet(expr) : build.buildDef(expr);
    return dsl_nullref;
  }

  ParseResult parseStmts() {
    auto blk = build.cfg.createBlock();
    auto _ = build.changeInsertIt(blk);
    while (!lex.peekIs(Token::NONE, DSLLexer::op_cbrclose)) {
      DYNO_EXPECT(parseStmt());
    }
    return blk;
  }

  ParseResult parseBlock() {
    DYNO_EXPECT(lex.popExpect(DSLLexer::op_cbropen));
    DYNO_EXPECT(res, parseStmts());
    DYNO_EXPECT(lex.popExpect(DSLLexer::op_cbrclose));
    return res;
  }

  ParseResult parse() { return parseStmts(); }

  ParseResult error(const char *msg) { return {lex.makeErrorOnPeekToken(msg)}; }
};

}; // namespace dyno
