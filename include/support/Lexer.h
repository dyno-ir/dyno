#pragma once
#include "dyno/Constant.h"
#include "support/ArrayRef.h"
#include "support/ErrorRecovery.h"
#include "support/SlabAllocator.h"
#include <cassert>
#include <cctype>
#include <cstdio>
#include <iostream>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>

struct Token {
  enum BaseType {
    NONE,
    IDENTIFIER,
    PCT_IDENTIFIER,

    BIG_INT_LITERAL,
    INT_LITERAL,

    NUMERIC_LITERAL,

    STRING_LITERAL,
    INLINE_CODE_LITERAL,
    SPECIAL_END,
  };

  uint32_t type;
  union {
    struct {
      uint32_t idx;
    } ident;
    struct {
      const dyno::BigInt *value;
      bool isUnsized;
      bool isSigned;
    } bigIntLit;
    struct {
      uint32_t value;
      bool isSigned;
    } intLit;
    struct {
      std::string_view value;
    } strLit, inlineCodeLit, numericLit;
  };

private:
  Token(uint32_t type) : type(type) {}

public:
  static Token makeNone() {
    auto rv = Token{NONE};
    return rv;
  }
  static Token makePlain(uint32_t type) {
    assert(type >= SPECIAL_END);
    auto rv = Token{type};
    return rv;
  }
  static Token makeKeyword(uint32_t type) { return makePlain(type); }
  static Token makeIdent(uint32_t identIdx, bool pctIdent = false) {
    auto rv = Token{pctIdent ? PCT_IDENTIFIER : IDENTIFIER};
    rv.ident.idx = identIdx;
    return rv;
  }
  static Token makeBigIntLit(const dyno::BigInt *value, bool isSigned,
                             bool isUnsized) {
    auto rv = Token{BIG_INT_LITERAL};
    rv.bigIntLit.value = value;
    rv.bigIntLit.isSigned = isSigned;
    rv.bigIntLit.isUnsized = isUnsized;
    return rv;
  }
  static Token makeIntLit(uint32_t value, bool isSigned) {
    auto rv = Token{INT_LITERAL};
    rv.intLit.value = value;
    rv.intLit.isSigned = isSigned;
    return rv;
  }
  static Token makeStrLit(std::string_view value) {
    auto rv = Token{STRING_LITERAL};
    rv.strLit.value = value;
    return rv;
  }
  static Token makeInlineCodeLit(std::string_view value) {
    auto rv = Token{INLINE_CODE_LITERAL};
    rv.inlineCodeLit.value = value;
    return rv;
  }
  static Token makeNumericLit(std::string_view value) {
    auto rv = Token{NUMERIC_LITERAL};
    rv.numericLit.value = value;
    return rv;
  }

  bool operator==(Token const &b) const {
    if (b.type != type)
      return false;
    switch (type) {
    case IDENTIFIER:
      return b.ident.idx == ident.idx;
    case STRING_LITERAL:
      return b.strLit.value == strLit.value;
    case BIG_INT_LITERAL:
      return b.bigIntLit.value == bigIntLit.value;
    case INT_LITERAL:
      return b.intLit.value == intLit.value;
    case INLINE_CODE_LITERAL:
      return b.inlineCodeLit.value == inlineCodeLit.value;
    default:
      return true;
    }
  }
  bool operator!=(Token const &b) const { return !(*this == b); }
};

struct ParseError {
  const char *message;
  size_t start;
  size_t end;
  unsigned lineNumber;
};

// todo config struct
template <bool ParseInlineCode = true, bool IgnoreBackslash = true,
          bool CPPStyleComments = true, bool LeadingPercentIdent = false>
struct Lexer {

  struct Config {
    enum class NumericParseType {
      RAW,
      DYNO,
      VERILOG,
    };
    NumericParseType numericParseType = NumericParseType::VERILOG;
  };
  Config config;

  const std::string path;
  ArrayRef<char> src;

  struct State {
    size_t i;
    size_t lastI;
    unsigned lineNumber;
  };
  State state;
  SlabAllocator<dyno::BigInt> bigIntLiterals;

  ArrayRef<const char *> operators;
  ArrayRef<const char *> keywords;

  static constexpr size_t TOK_OPS_START = Token::SPECIAL_END;
  size_t TOK_KW_START = TOK_OPS_START + operators.size();

private:
  std::optional<Token> peekToken;
  std::unordered_map<std::string_view, uint32_t> strings = initStrings();
  std::vector<std::string_view> rvStrings;

  std::unordered_map<std::string_view, uint32_t> initStrings() {
    std::unordered_map<std::string_view, uint32_t> map;
    size_t i = 0;
    for (auto kw : keywords)
      map.emplace(kw, i++);
    return map;
  }

  const size_t NUM_KEYWORDS = strings.size();

public:
  Lexer(const std::string &src, std::string &&srcPath,
        ArrayRef<const char *> operators, ArrayRef<const char *> keywords)
      : path(srcPath), src(src.c_str(), src.size()), operators(operators),
        keywords(keywords) {}

  Lexer(ArrayRef<char> src, std::string &&srcPath,
        ArrayRef<const char *> operators, ArrayRef<const char *> keywords)
      : path(srcPath), src(src), operators(operators), keywords(keywords) {}

  Token lexNext() {
    auto &i = state.i;
    auto &lastI = state.lastI;
    auto &lineNumber = state.lineNumber;

    size_t len = src.size();
    const char *srcC = src.data();

    while (1) {
      // Skip whitespace
      while (isspace(srcC[i])) {
        if (srcC[i] == '\n')
          lineNumber++;
        i++;
      }

      if (CPPStyleComments) {
        // Single Line Comment
        if (srcC[i] == '/' && srcC[i + 1] == '/') {
          while (i < len && srcC[i] != '\n')
            i++;
          continue;
        }

        /* Multi-line
           Comment  */
        if (srcC[i] == '/' && srcC[i + 1] == '*') {
          i += 2;
          while (i < len && !(srcC[i - 2] == '*' && src[i - 1] == '/')) {
            if (srcC[i - 2] == '\n')
              lineNumber++;
            i++;
          }
          continue;
        }
      }

      if (IgnoreBackslash) {
        if (src[i] == '\\') {
          i++;
          continue;
        }
      }

      break;
    }

    if (i == len)
      return Token::makeNone();

    lastI = i;

    { // Try lexing operator
      for (auto [j, op] : Range{operators}.enumerate()) {
        size_t k = 0;
        while (op[k] && srcC[i + k]) {
          if (op[k] != srcC[i + k])
            break;
          k++;
        }
        if (!op[k]) {
          i += k;
          return Token::makePlain(TOK_OPS_START + j);
        }
      }
    }

    // Try lexing keywords or tokens
    if (isalpha(srcC[i]) || srcC[i] == '_' ||
        (src[i] == '%' && LeadingPercentIdent)) {
      size_t identLen = 1;
      bool pct = src[i] == '%';
      while (1) {
        char c = srcC[i + identLen];
        if (!isdigit(c) && !isalpha(c) && c != '_')
          break;
        identLen++;
      }
      std::string_view substr(srcC + i, identLen);
      i += identLen;
      auto iter = strings.find(substr);
      if (iter != strings.end()) {
        if (iter->second < NUM_KEYWORDS)
          return Token::makeKeyword(TOK_KW_START + iter->second);
        else
          return Token::makeIdent(iter->second - NUM_KEYWORDS, pct);
      } else {
        rvStrings.push_back(substr);
        uint32_t idx = (strings[substr] = strings.size());
        return Token::makeIdent(idx - NUM_KEYWORDS, pct);
      }
    }

    // Int Literal
    if (config.numericParseType != Config::NumericParseType::RAW) {
      if (isdigit(srcC[i]) || srcC[i] == '\'') {
        const char *charPtr = &srcC[i];

        auto res =
            config.numericParseType == Lexer::Config::NumericParseType::VERILOG
                ? dyno::BigInt::parseVlog(charPtr)
                : dyno::BigInt::parseDyno(charPtr, src.end());

        if (!res.has_value())
          return Token::makeNone();
        i += charPtr - &srcC[i];

        if (res->type == dyno::BigInt::ParseResult::SIMPLE)
          return Token::makeIntLit(res->bigInt.getExactVal(), res->isSigned);

        auto *ptr = bigIntLiterals.allocate(std::move(res->bigInt));
        return Token::makeBigIntLit(ptr, res->isSigned,
                                    res->type ==
                                        dyno::BigInt::ParseResult::UNSIZED);
      }
    } else if (isdigit(src[i]) || src[i] == '.') {
      size_t len = 0;
      while (isalnum(src[i + len]) || src[i + len] == '_' ||
             src[i + len] == '.' || src[i + len] == '+' || src[i + len] == '-')
        len++;
      i += len;
      return Token::makeNumericLit(std::string_view{&src[i - len], len});
    }

    // String Literal
    bool multiline = (srcC[i] == '[' && srcC[i + 1] == '{') && ParseInlineCode;
    if (srcC[i] == '\"' || multiline) {
      size_t delimLen = multiline ? 2 : 1;
      size_t litLen = delimLen;
      if (i + litLen >= len)
        return Token::makeNone();
      while (multiline
                 ? (srcC[i + litLen] != '}' || srcC[i + litLen + 1] != ']')
                 : (srcC[i + litLen] != '\"')) {
        if (!multiline && srcC[i + litLen] == '\n')
          return Token::makeNone();
        litLen++;
        if (i + litLen >= len)
          return Token::makeNone();
      }

      Token t = Token::makeNone();
      if (multiline)
        t = Token::makeInlineCodeLit(
            std::string_view(srcC + i + delimLen, litLen - delimLen));
      else
        t = Token::makeStrLit(
            std::string_view(srcC + i + delimLen, litLen - delimLen));
      i += litLen + delimLen;
      return t;
    }

    return Token::makeNone();
  }

  Token Pop() {
    if (peekToken.has_value()) {
      Token t = peekToken.value();
      peekToken.reset();
      return t;
    }
    return lexNext();
  }

  Token Peek() {
    if (!peekToken.has_value())
      peekToken = lexNext();
    return peekToken.value();
  }

  auto getState() const {
    auto rv = state;
    if (peekToken) {
      // we do not back up the peek token, so reset to before it.
      rv.i = rv.lastI;
      rv.lastI = 0; // invalid w/o peek token
    }
    return rv;
  }
  void restoreState(State old) {
    peekToken.reset();
    state = old;
  }

  unsigned GetIdentIdx(std::string_view ident) {
    auto it = strings.find(ident);
    if (it == strings.end()) {
      rvStrings.push_back(ident);
      return (strings[ident] = strings.size()) - NUM_KEYWORDS;
    }
    return it->second - NUM_KEYWORDS;
  }

  std::string_view GetIdent(unsigned identIdx) { return rvStrings[identIdx]; }

  static std::string_view extractEnclosingLine(std::string_view input, size_t i,
                                               bool trimLeadingSpace = true) {
    const char *start = input.begin() + i;
    while (start != input.begin() && *start != '\n')
      --start;

    const char *end = input.begin() + i;
    while (end != input.end() && *end != '\n')
      ++end;

    if (start != end && start != input.begin())
      ++start;

    if (trimLeadingSpace) {
      while (isspace(*start))
        ++start;
    }

    return std::string_view(start, end);
  }

  template <typename... Ts> Token popEnsure(Ts... types) {
    peekEnsure(types...);
    return Pop();
  }
  template <typename... Ts>
  std::expected<Token, ParseError> tryPopEnsure(Ts... types) {
    if (auto tok = tryPeekEnsure(types...)) {
      return Pop();
    } else {
      return std::unexpected{tok.error()};
    }
  }

  void printError(const ParseError &error) {
    fprintf(stderr, "%s:%u: %s\n", path.c_str(), error.lineNumber,
            error.message);
    auto line = extractEnclosingLine(std::string_view{src}, error.start);
    unsigned pos;
    fprintf(stderr, "%s:%u: %n", path.c_str(), error.lineNumber, &pos);
    std::cerr << line << "\n";
    pos += &src[error.start] - line.begin();
    for (unsigned i = 0; i < pos; i++)
      putc(' ', stderr);
    putc('^', stderr);
    if (error.start != error.end)
      for (size_t i = error.start; i < error.end - 1; i++)
        putc('~', stderr);
    putc('\n', stderr);
  }

  [[noreturn]] void printErrorOnPeekToken(const char *error) {
    printError(makeErrorOnPeekToken(error));
    report_fatal_error("parser error");
  }
  ParseError makeErrorOnPeekToken(const char *error) {
    assert(peekToken);
    return ParseError{error, state.lastI, state.i, state.lineNumber};
  }

  ParseError makeErrorOnNextToken(const char *error) {
    assert(peekToken);
    return ParseError{error, state.i, state.i, state.lineNumber};
  }

  template <typename... Ts> Token peekEnsure(Ts... types) {
    Token t = Peek();
    if (!((t.type == types) || ...)) {
      printErrorOnPeekToken("unexpected token");
    }
    return t;
  }

  template <typename... Ts>
  std::expected<Token, ParseError> tryPeekEnsure(Ts... types) {
    Token t = Peek();
    if (!((t.type == types) || ...)) {
      return std::unexpected{makeErrorOnPeekToken("unexpected token")};
    }
    return t;
  }

  template <typename... Ts> bool peekIs(Ts... ts) {
    auto pk = Peek();
    return ((pk.type == ts) || ...);
  }
  template <typename... Ts> bool popIf(Ts... ts) {
    auto rv = peekIs(ts...);
    if (rv)
      Pop();
    return rv;
  }

  void reset(ArrayRef<char> src) {
    this->src = src;
    this->state = State{0, 0, 1};
    this->peekToken.reset();
  }
};
