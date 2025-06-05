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
  x(_CORE_INVALID, 0                   ) \
  x(CORE_INSTR,    1                   ) \
  x(CORE_CONSTANT, 2                   ) \
  x(CORE_BLOCK,    3 | TY_DEF_USE_START)
// clang-format on

#define ENUM_EXPAND(ident, idx) ident = idx,

enum class CoreTyID : uint8_t { TYPES(ENUM_EXPAND) };

#define CEXPR_EXPAND(ident, idx)                                               \
  constexpr CoreType ident{uint8_t(CoreTyID::ident)};

TYPES(CEXPR_EXPAND)

#undef TYPES
#undef ENUM_EXPAND
#undef CEXPR_EXPAND

template <> struct DialectTraits<DIALECT_CORE> {
  constexpr static DialectInfo info{"core"};
  constexpr static TyInfo tyInfo[] = {
      {"_invalid"}, {"instr"}, {"constant"}, {"block"}};
  constexpr static OpcodeInfo opcInfo[] = {{"block_instr"}};
};

} // namespace dyno
