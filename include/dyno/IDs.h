#pragma once

#include "DialectInfo.h"
#include "dyno/Obj.h"
#include "dyno/Type.h"
#include <cstdint>
#include <support/Bits.h>

namespace dyno {
using CoreType = SpecificDialectType<DialectID{DIALECT_CORE}>;

// clang-format off
#define TYPES(x) \
  x(CORE_INSTR,    0                   ) \
  x(CORE_CONSTANT, 1                   ) \
  x(CORE_BLOCK,    2 | TY_DEF_USE_START)
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
  constexpr static TyInfo tyInfo[] = {{"instr"}, {"constant"}, {"block"}};
  constexpr static OpcodeInfo opcInfo[] = {{"block_instr"}};
};

} // namespace dyno
