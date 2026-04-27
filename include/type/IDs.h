#pragma once

#include "dyno/IDImpl.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Parser.h"
namespace dyno {
constexpr DialectID DIALECT_TYPE{9}; // fixme: dialect ID assignment

using TypeDialectType = SpecificDialectType<DialectID{DIALECT_TYPE}>;
using TypeDialectOpcode = SpecificDialectOpcode<DialectID{DIALECT_TYPE}>;

// clang-format off
#define TYPES(x) \
  x("generic",      TYPE_GENERIC,   0) \
  x("base",         TYPE_BASE,      1) \
  x("struct",       TYPE_STRUCT,    2) \
  x("enum",         TYPE_ENUM,      3) \
  x("array",        TYPE_ARRAY,     4)
// clang-format on

#define ENUM_EXPAND(name, ident, idx) ident = idx,

enum class TypeDialTyID : uint8_t { TYPES(ENUM_EXPAND) };
#undef ENUM_EXPAND

#define CEXPR_EXPAND(name, ident, idx)                                         \
  constexpr TypeDialectType ident{uint8_t(TypeDialTyID::ident)};
TYPES(CEXPR_EXPAND)
#undef CEXPR_EXPAND

// in general to define dialect/type/opcode info specialize this struct or void
// registerDialect<>
template <> struct DialectTraits<DIALECT_TYPE> {
  constexpr static DialectInfo info{"type"};
  constexpr static TyInfo tyInfo[] = {
#define TYINFO_EXPAND(name, ident, idx) {name, !!((idx) & TY_DEF_USE_START)},
      TYPES(TYINFO_EXPAND)
#undef TYINFO_EXPAND
  };
  constexpr static OpcodeInfo opcInfo[] = {};
};
}; // namespace dyno
