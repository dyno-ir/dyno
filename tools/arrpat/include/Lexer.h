#pragma once
#include "Token.h"
#include "dyno/Constant.h"
#include "support/ArrayRef.h"
#include "support/SlabAllocator.h"
#include <cassert>
#include <iostream>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>

struct Lexer {
  const std::string path;
  const std::string src;
  size_t i = 0;
  size_t lastI = 0;
  uint lineNumber = 1;

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
  Lexer(std::string &&src, std::string &&srcPath,
        ArrayRef<const char *> operators, ArrayRef<const char *> keywords)
      : path(srcPath), src(src), operators(operators), keywords(keywords) {}

  Token Pop() {
    if (peekToken.has_value()) {
      Token t = peekToken.value();
      peekToken.reset();
      return t;
    }

    size_t len = src.length();
    const char *srcC = src.c_str();

    while (1) {
      // Skip whitespace
      while (isspace(srcC[i])) {
        if (srcC[i] == '\n')
          lineNumber++;
        i++;
      }

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
    if (isalpha(srcC[i]) || srcC[i] == '_') {
      size_t identLen = 1;
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
          return Token::makeIdent(iter->second - NUM_KEYWORDS);
      } else {
        rvStrings.push_back(substr);
        uint32_t idx = (strings[substr] = strings.size());
        return Token::makeIdent(idx - NUM_KEYWORDS);
      }
    }

    // Int Literal
    if (isdigit(srcC[i]) || srcC[i] == '\'') {
      const char *charPtr = &srcC[i];
      auto res = dyno::BigInt::parseVlog(charPtr);
      if (!res.has_value())
        return Token::makeNone();
      i += charPtr - &srcC[i];

      if (res->type == dyno::BigInt::ParseVlogResult::SIMPLE)
        return Token::makeIntLit(res->bigInt.getExactVal(), res->isSigned);

      auto *ptr = bigIntLiterals.allocate(std::move(res->bigInt));
      return Token::makeBigIntLit(ptr, res->isSigned,
                                  res->type ==
                                      dyno::BigInt::ParseVlogResult::UNSIZED);
    }

    // String Literal
    bool multiline = (srcC[i] == '[' && srcC[i + 1] == '{');
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
        Token::makeStrLit(
            std::string_view(srcC + i + delimLen, litLen - delimLen));
      i += litLen + delimLen;
      return t;
    }

    return Token::makeNone();
  }
  Token Peek() {
    if (!peekToken.has_value())
      peekToken = Pop();
    return peekToken.value();
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

  static std::string_view extractEnclosingLine(std::string_view input,
                                               size_t i) {
    const char *start = input.begin() + i;
    while (start != input.begin() && *start != '\n')
      --start;

    const char *end = input.begin() + i;
    while (end != input.end() && *end != '\n')
      ++end;

    if (start != end)
      ++start;

    return std::string_view(start, end);
  }

  template <typename... Ts> Token popEnsure(Ts... types) {
    peekEnsure(types...);
    return Pop();
  }

  template <typename... Ts> Token peekEnsure(Ts... types) {
    Token t = Peek();
    if (!((t.type == types) || ...)) {
      fprintf(stderr, "%s:%u: unexpected token\n", path.c_str(), lineNumber);
      auto line = extractEnclosingLine(std::string_view{src}, lastI);
      uint pos;
      fprintf(stderr, "%s:%u:%n", path.c_str(), lineNumber, &pos);
      std::cerr << line << "\n";
      pos += &src[lastI] - line.begin();
      for (uint i = 0; i < pos; i++)
        putc(' ', stderr);
      fprintf(stderr, "^\n");
      exit(-1);
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
};
