#pragma once
#include "ANSITerminal.h"
#include "ErrorRecovery.h"
#include "SIMD.h"
#include "StringRef.h"
#include "Tokenizer.h"
#include "dyno/Constant.h"
#include "support/ANSITerminal.h"
#include "support/ArrayRef.h"
#include "support/Bits.h"
#include "support/Debug.h"
#include "support/ErrorRecovery.h"
#include "support/Format.h"
#include "support/Ranges.h"
#include "support/Result.h"
#include "support/SIMD.h"
#include "support/SlabAllocator.h"
#include "support/StringHelpers.h"
#include "support/TemplateUtil.h"
#include "support/TwoLevelSet.h"
#include <array>
#include <bit>
#include <cassert>
#include <cstdio>
#include <format>
#include <memory>
#include <optional>
#include <ostream>
#include <string>

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
  static constexpr auto baseTypeNames = std::to_array({
      "<invalid token>",
      "<identifier>",
      "<percent identifier>",
      "<big int literal>",
      "<int literal>",
      "<numeric literal>",
      "<string literal>",
      "<inline code literal>",
  });

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
    std::construct_at(&rv.strLit);
    rv.strLit.value = value;
    return rv;
  }
  static Token makeInlineCodeLit(std::string_view value) {
    auto rv = Token{INLINE_CODE_LITERAL};
    std::construct_at(&rv.inlineCodeLit);
    rv.inlineCodeLit.value = value;
    return rv;
  }
  static Token makeNumericLit(std::string_view value) {
    auto rv = Token{NUMERIC_LITERAL};
    std::construct_at(&rv.numericLit);
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

  bool isNone() { return type == NONE; }
};

struct ParseError {
  Format message;
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
    size_t i = 0;
    size_t columnStartI = 0;
    unsigned lineNumber = 1;
  };
  State state, lastState, preWhitespaceState;

  SlabAllocator<dyno::BigInt> bigIntLiterals;

  ArrayRef<StringRef> operators;
  ArrayRef<StringRef> keywords;

  static constexpr size_t TOK_OPS_START = Token::SPECIAL_END;
  size_t TOK_KW_START = TOK_OPS_START + operators.size();

private:
  std::optional<Token> peekToken;
  TwoLevelMap<SSOStringRef, uint32_t> strings = initStrings();
  Vec<StringRef> rvStrings;

  TwoLevelMap<SSOStringRef, uint32_t> initStrings() {
    TwoLevelMap<SSOStringRef, uint32_t> map;
    size_t i = 0;
    for (auto kw : keywords)
      map.insert(kw, i++);
    return map;
  }

  const size_t NUM_KEYWORDS = strings.size();
  using mask_t = vec_mask_unsigned_t<vec_u8_t>;
  using mask2x_t = uint_of_size_t<sizeof(mask_t) * 2>;

  template <typename Match>
  const char *lexUntil(const char *begin, Match matcher) {
    size_t j = begin - src.begin();
    size_t prevStart = j;
    size_t prevRem = std::min<size_t>(src.size() - j, SIMD_WIDTH);
    mask_t prevValid = bit_mask_ones<mask_t>(prevRem);
    vec_u8_t prevData = vec_u8_t::load(
        reinterpret_cast<const uint8_t *>(src.data() + j), prevValid, 0);
    j += prevRem;

    for (size_t curRem = 0;; j += curRem) {
      curRem = std::min<size_t>(src.size() - j, SIMD_WIDTH);
      mask_t curValid = bit_mask_ones<mask_t>(curRem);
      vec_u8_t curData = vec_u8_t::load(
          reinterpret_cast<const uint8_t *>(src.data() + j), curValid, 0);

      mask2x_t match = matcher(prevData, curData, prevValid, curValid);
      size_t endIdx = std::countr_zero(mask_t(match));
      mask_t active = bit_mask_ones<mask_t>(endIdx);
      mask_t matchNewline = (prevData == vec_u8_t('\n')).mask() & active;
      state.lineNumber += std::popcount(matchNewline);
      auto lastNl = bit_mask_sz<mask_t> - std::countl_zero(matchNewline) - 1;
      state.columnStartI = __builtin_unpredictable(matchNewline)
                               ? prevStart + lastNl
                               : state.columnStartI;
      if (endIdx < bit_mask_sz<mask_t>)
        return &src[prevStart + endIdx];

      if (!curRem)
        return src.end();

      prevStart = j;
      prevValid = curValid;
      prevData = curData;
    }
  }

  const char *lexUntilDelimiter(const char *begin, StringRef delim) {
    size_t n = delim.size();
    assert(n > 0 && n <= SIMD_WIDTH);

    return lexUntil(begin, [&](vec_u8_t prevData, vec_u8_t curData,
                               mask_t prevValid, mask_t curValid) {
      mask2x_t match = bit_mask_ones<mask2x_t>();
      for (size_t k = 0; k < n; k++) {
        auto m0 = (prevData == vec_u8_t(delim[k])).mask() & prevValid;
        auto m1 = (curData == vec_u8_t(delim[k])).mask() & curValid;
        mask2x_t mk = (mask2x_t(m1) << bit_mask_sz<mask_t>) | m0;
        match &= mk >> k;
      }
      return match;
    });
  }

  const char *skipWhitespace(const char *begin) {
    return lexUntil(begin, [](vec_u8_t data, vec_u8_t, mask_t valid, mask_t) {
      auto control = (data - vec_u8_t('\t')) <= vec_u8_t('\r' - '\t');
      auto space = data == vec_u8_t(' ');
      return mask2x_t(~(control | space).mask() & valid);
    });
  }

  const char *lexSingleLineComment(const char *begin) {
    return lexUntil(begin, [](vec_u8_t data, vec_u8_t, mask_t valid, mask_t) {
      return mask2x_t((data == vec_u8_t('\n')).mask() & valid);
    });
  }

  static mask_t isIdentCharMask(vec_u8_t data) {
    vec_u8_t lower = data | vec_u8_t('a' - 'A');
    auto letter = (lower - vec_u8_t('a')) <= vec_u8_t('z' - 'a');
    auto digit = (data - vec_u8_t('0')) <= vec_u8_t('9' - '0');
    auto underscore = data == vec_u8_t('_');
    return (letter | digit | underscore).mask();
  }

  Token lexInlineCodeLiteral() {
    auto &i = state.i;

    if (i + 2 >= src.size())
      return Token::makeNone();

    auto begin = &src[i + 2];
    constexpr StringRef delim = "}]";
    auto end = lexUntilDelimiter(begin, delim);
    if (end == src.end())
      return Token::makeNone();
    i = end - src.begin() + delim.size();
    return Token::makeInlineCodeLit(std::string_view(begin, end));
  }

  Token lexStringLiteral() {
    auto &i = state.i;
    auto &lineNumber = state.lineNumber;
    auto &columnStartI = state.columnStartI;

    i++;
    mask_t matchSlashPrev = 0;
    size_t j;
    for (j = i; j < src.size(); j += SIMD_WIDTH) {
      auto rem = src.size() - j;
      auto mask =
          bit_mask_ones<mask_t>(std::min<unsigned>(rem, bit_mask_sz<mask_t>));
      auto data = vec_char_t::load(&src[j], mask, 0);

      auto matchSlash = (data == vec_char_t('\\')).mask();
      auto slashWide =
          (mask2x_t(matchSlash) << bit_mask_sz<mask_t>) | matchSlashPrev;
      auto [even, odd] = bit_even_odd_run(slashWide);
      even >>= bit_mask_sz<mask_t> - 1;
      odd >>= bit_mask_sz<mask_t> - 1;

      auto matchParen = (data == vec_char_t('\"')).mask();
      matchParen &= ~odd;

      size_t endIdx = std::countr_zero(matchParen);
      auto active = bit_mask_ones<mask_t>(endIdx);

      mask_t matchNewline = (data == vec_char_t('\n')).mask() & active;
      lineNumber += std::popcount(matchNewline);
      auto lastNl = bit_mask_sz<mask_t> - std::countl_zero(matchNewline) - 1;
      columnStartI =
          __builtin_unpredictable(matchNewline) ? lastNl + j : columnStartI;

      matchSlashPrev = 0;
      if (__builtin_unpredictable(even >> (bit_mask_sz<mask_t>)))
        matchSlashPrev = bit_mask_ms_nbits<mask_t>(2);
      if (__builtin_unpredictable(odd >> (bit_mask_sz<mask_t>)))
        matchSlashPrev = bit_mask_msb<mask_t>();

      if (endIdx != bit_mask_sz<mask_t>) {
        j += endIdx + 1;
        goto found_end;
      }
    }
    return Token::makeNone();
  found_end:
    std::string_view rv{src.data() + i, (j - i) - 1};
    i = j;
    return Token::makeStrLit(rv);
  }

public:
  Lexer(ArrayRef<char> src, std::string &&srcPath,
        ArrayRef<StringRef> operators, ArrayRef<StringRef> keywords)
      : path(srcPath), src(src), operators(operators), keywords(keywords) {
    fillPunctChars();
  }

#define DYNO_PARSER_SIMD
#ifdef DYNO_PARSER_SIMD
  constexpr static size_t maxPunctChars = 3;
  SmallVec<std::array<std::array<uint8_t, SIMD_WIDTH>, maxPunctChars>, 2>
      punctChars;

  void fillPunctChars() {
    // build SIMD layout for punctuation
    punctChars.resize(round_up_div(operators.size(), SIMD_WIDTH));

    for (auto [i, op] : Range{operators}.enumerate()) {
      assert(op.size() <= maxPunctChars && "bump maxPunctChars");
      for (size_t j = 0; j < maxPunctChars; j++)
        punctChars[i / SIMD_WIDTH][j][i % SIMD_WIDTH] =
            ((j < op.size()) ? op[j] : 0);
    }
  }

  std::optional<Token> lexNextPunct() {
    size_t srcLen = src.size();
    auto &srcOffs = state.i;

    for (size_t i = 0; i < punctChars.size(); i++) {
      size_t base = i * SIMD_WIDTH;
      size_t count = std::min<size_t>(operators.size() - base, SIMD_WIDTH);
      mask_t matchAcc = bit_mask_ones<mask_t>(count);
      for (size_t j = 0; j < maxPunctChars; j++) {
        auto srcIdx = srcOffs + j;
        vec_u8_t charSplat = vec_u8_t(
            srcIdx >= srcLen ? 0xFF : static_cast<uint8_t>(src[srcIdx]));
        auto punctCharVec = vec_u8_t::load(punctChars[i][j].data());
        auto matchVec =
            (punctCharVec == charSplat) | (punctCharVec == vec_u8_t(0));
        matchAcc &= matchVec.mask();
      }
      if (matchAcc == 0)
        continue;
      size_t matchIdx =
          i * SIMD_WIDTH + static_cast<size_t>(std::countr_zero(matchAcc));

      srcOffs += operators[matchIdx].size();
      return Token::makePlain(TOK_OPS_START + matchIdx);
    }
    return std::nullopt;
  }
#else
  void fillPunctChars() {}
  std::optional<Token> lexNextPunct() {
    const char *srcData = src.data();
    size_t len = src.size();
    auto &i = state.i;

    for (auto [j, op] : Range{operators}.enumerate()) {
      size_t k = 0;
      while (k < op.size() && i + k < len) {
        if (op[k] != srcData[i + k])
          break;
        k++;
      }
      if (k == op.size()) {
        i += k;
        return Token::makePlain(TOK_OPS_START + j);
      }
    }
    return std::nullopt;
  }
#endif

  Token lexNext() {
    auto &i = state.i;
    size_t len = src.size();
    preWhitespaceState = state;

    while (1) {
      if (i == len)
        return Token::makeNone();
      i = skipWhitespace(src.data() + i) - src.begin();
      if (i == len)
        return Token::makeNone();

      if (CPPStyleComments && i + 1 < len) {
        if (src[i] == '/' && src[i + 1] == '/') {
          if (i + 2 == len) {
            i = len;
            continue;
          }
          i = lexSingleLineComment(src.data() + i + 2) - src.begin();
          continue;
        }

        if (src[i] == '/' && src[i + 1] == '*') {
          if (i + 2 == len)
            return Token::makeNone();
          auto begin = &src[i + 2];
          auto end = lexUntilDelimiter(begin, "*/");
          if (end == src.end())
            return Token::makeNone();
          i = end - src.begin() + 2;
          continue;
        }
      }

      if (IgnoreBackslash && src[i] == '\\') {
        i++;
        continue;
      }

      break;
    }

    lastState = state;

    // String Literal
    if (ParseInlineCode && i + 1 < len && src[i] == '[' && src[i + 1] == '{')
      return lexInlineCodeLiteral();
    if (src[i] == '\"')
      return lexStringLiteral();

    if (auto punct = lexNextPunct())
      return *punct;

    // Try lexing keywords or tokens
    if (isalpha(src[i]) || src[i] == '_' ||
        (src[i] == '%' && LeadingPercentIdent)) {
      bool pct = src[i] == '%';
      auto begin = src.data() + i;
      auto end =
          StringHelpers::find(begin + 1, src.end(), [pct](vec_u8_t data) {
            mask_t valid = isIdentCharMask(data);
            if (pct)
              valid |= (data == vec_u8_t('.')).mask();
            return valid;
          });
      size_t identLen = end - begin;
      std::string_view substr(begin, identLen);
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
      if (isdigit(src[i]) || src[i] == '\'') {
        auto charPtr = &src[i];

        auto res =
            config.numericParseType == Lexer::Config::NumericParseType::VERILOG
                ? dyno::BigInt::parseVlog(charPtr)
                : dyno::BigInt::parseDyno(charPtr, src.end());

        if (!res.has_value())
          return Token::makeNone();
        i += charPtr - &src[i];

        if (res->type == dyno::BigInt::ParseResult::SIMPLE)
          return Token::makeIntLit(res->bigInt.getExactVal(), res->isSigned);

        auto *bigIntPtr = bigIntLiterals.allocate(std::move(res->bigInt));
        return Token::makeBigIntLit(bigIntPtr, res->isSigned,
                                    res->type ==
                                        dyno::BigInt::ParseResult::UNSIZED);
      }
    } else if (isdigit(src[i]) || src[i] == '.') {
      auto begin = src.data() + i;
      auto end = StringHelpers::find(begin, src.end(), [](vec_u8_t data) {
        mask_t valid = isIdentCharMask(data);
        valid |= (data == vec_u8_t('.')).mask();
        valid |= (data == vec_u8_t('+')).mask();
        valid |= (data == vec_u8_t('-')).mask();
        return valid;
      });
      i = end - src.begin();
      return Token::makeNumericLit(
          std::string_view{begin, static_cast<size_t>(end - begin)});
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
      rv = lastState;
    }
    return rv;
  }
  void restoreState(State old) {
    peekToken.reset();
    state = old;
  }

  unsigned GetIdentIdx(std::string_view ident) {
    auto [found, it] = strings.findOrInsert(
        ident, [&] { return (strings.size() - NUM_KEYWORDS); });
    if (!found)
      rvStrings.emplace_back(ident);
    return it->second - NUM_KEYWORDS;
  }

  StringRef GetIdent(unsigned identIdx) { return rvStrings[identIdx]; }

  StringRef extractEnclosingLine(StringRef input) const {
    assert(input.begin() >= src.begin() && input.end() <= src.end());
    const char *start = input.begin();
    while (start != src.begin() && *start != '\n')
      --start;

    const char *end = input.end();
    while (end != src.end() && *end != '\n')
      ++end;

    if (start != end && start != input.begin())
      ++start;

    return StringRef(start, end);
  }

  template <typename... Ts> Token popEnsure(Ts... types) {
    peekEnsure(types...);
    return Pop();
  }
  template <typename... Ts> Result<Token, ParseError> popExpect(Ts... types) {
    if (auto tok = peekExpect(types...)) {
      return Pop();
    } else {
      return {std::move(tok.error())};
    }
  }

  template <typename... Ts> Result<Token, ParseError> popExpect() {
    if (auto tok = peekExpect()) {
      return Pop();
    } else {
      return {std::move(tok.error())};
    }
  }

  void printError(std::ostream &os, const ParseError &error) const {
    StringRef errorLinesStr =
        StringRef{src.begin() + error.start, error.end - error.start};
    errorLinesStr = extractEnclosingLine(errorLinesStr);

    Tokenizer linesTok{errorLinesStr, "\n"};
    SmallVec<StringRef, 16> lines(Range{linesTok});

    unsigned col = &src[error.start] - errorLinesStr.begin() + 1;
    std::print(os, "{}{}:{}:{}: {}error: {}", stderrBold(), path.c_str(),
               error.lineNumber, col, stderrRed(), stderrWhite());
    error.message.toStream(os);
    std::print(os, "{}\n", stderrReset());

    unsigned indent =
        std::formatted_size("{}", error.lineNumber + lines.size() - 1);

    for (auto [i, line] : Range{lines}.enumerate()) {
      std::print(os, "  {:<{}} |{}\n", error.lineNumber + i, indent, line);
      if (i == 0) {
        auto num = std::min(line.end(), src.begin() + error.end) - line.begin();
        num -= error.start - (line.begin() - src.begin()) + 1;
        if (num < 0)
          std::print(os, "{:>{}}{}{:>{}}{}\n", "|", indent + 4, stderrGreen(),
                     "^", col, stderrReset());
        else
          std::print(os, "{:>{}}{}{:>{}}{:~>{}}{}\n", "|", indent + 4,
                     stderrGreen(), "^", col, "", num, stderrReset());
      } else {
        auto stripped = line.strip();
        auto start = stripped.begin() - line.begin();
        auto num = std::min(stripped.end(), src.begin() + error.end) -
                   stripped.begin() + 1;
        std::print(os, "{:>{}}{:>{}}{}{:~>{}}{}\n", "|", indent + 4, "", start,
                   stderrGreen(), "", num, stderrReset());
      }
    }
  }
  void printError(const ParseError &error) const {
    printError(std::cerr, error);
  }
  std::string errorToString(const ParseError &error) const {
    std::stringstream str;
    printError(str, error);
    return std::move(str).str();
  }
  LazyFormat errorToPrintable(const ParseError &error) const {
    return LazyFormat{[&](std::ostream &os) { printError(os, error); }};
  }

  [[noreturn]] void printErrorOnPeekToken(const char *error, auto &&...args) {
    printError(
        makeErrorOnPeekToken(error, std::forward<decltype(args)>(args)...));
    report_fatal_error();
  }
  [[nodiscard]] ParseError makeErrorOnPeekToken(const char *error,
                                                auto &&...args) {
    assert(peekToken);
    return ParseError{Format{error, std::forward<decltype(args)>(args)...},
                      lastState.i, state.i, state.lineNumber};
  }

  [[nodiscard]] ParseError makeErrorOnNextToken(const char *error,
                                                auto &&...args) {
    if (!peekToken)
      Peek();
    return ParseError{Format{error, std::forward<decltype(args)>(args)...},
                      lastState.i, state.i, state.lineNumber};
  }

  [[nodiscard]] ParseError makeErrorStartingAtToLast(const State &startState,
                                                     const char *error,
                                                     auto &&...args) {
    return ParseError{Format{error, std::forward<decltype(args)>(args)...},
                      startState.i, !!peekToken ? lastState.i : state.i,
                      startState.lineNumber};
  }

  const char *getTokenTypeString(uint32_t type) {
    if (type < Token::SPECIAL_END)
      return Token::baseTypeNames[type];
    type -= TOK_OPS_START;
    if (type < operators.size()) {
      return operators[type].data();
    }
    type -= operators.size();
    if (type < keywords.size()) {
      return keywords[type].data();
    }
    type -= keywords.size();
    return rvStrings[type].data();
  }

  template <typename... Ts>
  ParseError makeExpectedTokenError(uint32_t actualType, Ts... types) {
    if constexpr (sizeof...(types) > 1) {
      std::array typesL = {uint32_t(types)...};
      auto rng = Range(typesL).transform(
          [&](size_t, uint32_t t) { return getTokenTypeString(t); });
      std::array<const char *, sizeof...(types)> arr;
      std::copy_n(rng.begin(), sizeof...(types), arr.begin());
      return makeErrorOnPeekToken("unexpected token: \'{}\', expected: {}",
                                  getTokenTypeString(actualType),
                                  LazyFormat{[arr](std::ostream &str) {
                                    for (auto elem : Range{arr}.drop_back())
                                      str << "\'" << elem << "\', ";
                                    str << "\'" << arr.back() << "\'";
                                  }});
    } else {
      return makeErrorOnPeekToken("unexpected token: \'{}\', expected: \'{}\'",
                                  getTokenTypeString(actualType),
                                  getTokenTypeString(types...));
    }
  }

  template <typename... Ts> Token peekEnsure(Ts... types) {
    Token t = Peek();
    if (!((t.type == types) || ...)) {
      printError(makeExpectedTokenError(t.type, types...));
      report_fatal_error();
    }
    return t;
  }

  template <typename... Ts> Result<Token, ParseError> peekExpect(Ts... types) {
    Token t = Peek();
    if (!((t.type == types) || ...)) {
      return {makeExpectedTokenError(t.type, types...)};
    }
    return t;
  }

  template <typename... Ts> Result<Token, ParseError> peekExpect() {
    Token t = Peek();
    if (t.type == Token::NONE)
      return {makeErrorOnPeekToken("unexpected eof")};
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
    this->state = State{};
    this->peekToken.reset();
  }

  std::pair<uint32_t, uint32_t> getStartOfPeekTokenLineCol() {
    if (!peekToken)
      Peek();
    return std::make_pair(lastState.lineNumber,
                          lastState.i - lastState.columnStartI);
  }
  std::pair<uint32_t, uint32_t> getEndOfTokenLineCol() {
    if (peekToken)
      return std::make_pair(preWhitespaceState.lineNumber,
                            preWhitespaceState.i -
                                preWhitespaceState.columnStartI);
    return std::make_pair(state.lineNumber, state.i - state.columnStartI);
  }
};
