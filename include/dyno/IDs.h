#pragma once

#include "DialectInfo.h"
#include "dyno/Type.h"
#include <cstdint>
#include <support/Bits.h>

namespace dyno {
using CoreType = SpecificDialectType<DialectID{DIALECT_CORE}>;

// clang-format off
#define TYPES(x) \
  /* _CORE_INVALID not for real use (use ObjID::invalid()). \
     Just so accidental zero ref doesn't print as instr. */ \
  x("_invalid", _CORE_INVALID, 0                   ) \
  x("instr",    CORE_INSTR,    1                   ) \
  x("constant", CORE_CONSTANT, 2                   ) \
  x("block",    CORE_BLOCK,    3 | TY_DEF_USE_START)
// clang-format on

#define ENUM_EXPAND(name, ident, idx) ident = idx,
enum class CoreTyID : uint8_t { TYPES(ENUM_EXPAND) };
#undef ENUM_EXPAND

#define CEXPR_EXPAND(name, ident, idx)                                         \
  constexpr CoreType ident{uint8_t(CoreTyID::ident)};
TYPES(CEXPR_EXPAND)
#undef CEXPR_EXPAND

template <> struct DialectTraits<DIALECT_CORE> {
  constexpr static DialectInfo info{"core"};
  constexpr static TyInfo tyInfo[] = {
#define TYINFO_EXPAND(name, ident, idx) {name, !!((idx) & TY_DEF_USE_START)},
      TYPES(TYINFO_EXPAND)
#undef TYINFO_EXPAND
  };
  constexpr static OpcodeInfo opcInfo[] = {{"block_instr"}};
};

#undef TYPES
} // namespace dyno
