#pragma once
#include "dyno/Constant.h"
#include <stdint.h>
#include <string_view>

struct Token {
  enum BaseType {
    NONE,
    IDENTIFIER,

    BIG_INT_LITERAL,

    INT_LITERAL,

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
    } strLit, inlineCodeLit;
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
  static Token makeIdent(uint32_t identIdx) {
    auto rv = Token{IDENTIFIER};
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
