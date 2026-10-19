#pragma once

#include "dsl/DSLBuilder.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/AutoDebugInfo.h"
#include "support/MacroUtil.h"
#include <support/Lexer.h>

namespace dyno {

class DSLLexer : public Lexer<true, false, true, false> {
public:
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
    op_exmark,

    _op_end,
    _op_size = _op_end - _op_start - 1
  };

  enum KeywordEnum {
    _kw_start = Lexer::TOK_OPS_START + (_op_size)-1,
    kw_let,
    kw_if,
    kw_else,
    kw_for,
    kw_struct,
    kw_enum,
    kw_fn,
    kw_while,
    kw_return,
    kw_int,
    kw_uint,
    kw_int8,
    kw_int16,
    kw_int32,
    kw_int64,
    kw_uint8,
    kw_uint16,
    kw_uint32,
    kw_uint64,
    kw_bool,
    kw_string,
    kw_typename,
    kw_true,
    kw_false,

    _kw_end,
    _kw_size = _kw_end - _kw_start - 1
  };

  constexpr static std::array<StringRef, _kw_size> Keywords = {
      "let",    "if",    "else",   "for",      "struct", "enum",
      "fn",     "while", "return", "int",      "uint",   "int8",
      "int16",  "int32", "int64",  "uint8",    "uint16", "uint32",
      "uint64", "bool",  "string", "typename", "true",   "false"};
  constexpr static std::array<StringRef, _op_size> Operators = {
      ".",  ":",  ",",  "[",  "]", "?", "#",  "(", ")", "{",
      "}",  ";",  "->", "==", "+", "-", "*",  "&", "|", "<<",
      ">>", "<=", ">=", "<",  ">", "=", "!=", "!"};

  static constexpr bool isBuiltinVal(uint32_t kind) {
    return kind >= kw_int && kind <= kw_false;
  }

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
    PREC_APPLY,
    PREC_ACCESS
  };

  static constexpr Precedence getPrecedence(DialectOpcode opc);
  static constexpr DialectOpcode getUnaryOpc(const Token &tok);
  static constexpr DialectOpcode getBinaryOpc(const Token &tok);
  static constexpr bool isRightAssoc(Precedence prec);

  Context &ctx;
  DSLLexer &lex;
  BuilderStack<DSLBuilder> build;
  AutoDebugInfoStack debugInfo;
  Vec<size_t> lineStarts{0};

  DSLParser(Context &ctx, DSLLexer &lex)
      : ctx(ctx), lex(lex), build(ctx), debugInfo(ctx) {
    for (size_t i = 0; i < lex.src.size(); ++i)
      if (lex.src[i] == '\n')
        lineStarts.push_back(i + 1);
  }

  SimpleDebugInfo peekSource();
  AutoDebugInfoStack::ScopeToken sourceScope(SimpleDebugInfo loc) {
    return debugInfo.addWithToken(loc.name, loc.beginLine, loc.beginCol,
                                  loc.endLine, loc.endCol);
  }
  ParseResult parseAtom();
  ParseResult parseFunction(DSLValue pattern, Precedence prec);

  enum class ExprContext { Normal, ControlHeader };

  static bool startsArg(const Token &tok);
  ParseResult parseUnary();
  ParseResult parseTightPostfix(DSLValue value);
  ParseResult parseExpr(Precedence prec = PREC_NONE,
                        ExprContext context = ExprContext::Normal);

  ParseResult parseStmt();
  ParseResult parseStmts();
  ParseResult parseBlock();

  ParseResult parse() { return parseStmts(); }

  ParseResult error(const char *msg) { return {lex.makeErrorOnPeekToken(msg)}; }
};

}; // namespace dyno
