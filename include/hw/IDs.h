

#pragma once

#include "dyno/IDImpl.h"
#include "dyno/Obj.h"
#include "dyno/Opcode.h"
#include "dyno/Type.h"
#include <cstdint>
#include <dyno/DialectInfo.h>

namespace dyno {

enum HWDialectID : uint8_t { DIALECT_HW = DIALECT_CUSTOM };
using HWType = SpecificDialectType<DialectID{DIALECT_HW}>;
using HWOpcode = SpecificDialectOpcode<DialectID{DIALECT_HW}>;

// clang-format off
#define TYPES(x) \
  x(HW_WIRE,         0 | TY_DEF_USE_START) \
  x(HW_REGISTER,     1 | TY_DEF_USE_START) \
  x(HW_PROCESS,      2 | TY_DEF_USE_START) \
  x(HW_MODULE,       3 | TY_DEF_USE_START) \
  x(HW_SENS_MODES,   4) \
  x(HW_TRIGGER,      5 | TY_DEF_USE_START) \
  x(HW_MEMORY,       6 | TY_DEF_USE_START)
// clang-format on

#define ENUM_EXPAND(ident, idx) ident = idx,

enum class HWTyID : uint8_t { TYPES(ENUM_EXPAND) };

#define CEXPR_EXPAND(ident, idx) constexpr HWType ident{uint8_t(HWTyID::ident)};

TYPES(CEXPR_EXPAND)

#undef TYPES
#undef ENUM_EXPAND
#undef CEXPR_EXPAND

#define HEADER enum class HWOpcID : uint16_t {
#define ADD_OP(x) HW_##x,
#include "HWInstrs.inc"

#define HEADER
#define FOOTER
#define ADD_OP(x) constexpr HWOpcode HW_##x{uint16_t(HWOpcID::HW_##x)};
#include "HWInstrs.inc"

template <> struct DialectTraits<DIALECT_HW> {
  constexpr static DialectInfo info{"rtl"};
  constexpr static TyInfo tyInfo[] = {
      {"wire"}, {"register"}, {"process"}, {"module"}, {"sens_modes"}, {"trigger"}, {"memory"}};
  constexpr static OpcodeInfo opcInfo[] = {
#define HEADER
#define FOOTER
#define ADD_OP(x) {#x},
#include "HWInstrs.inc"
  };
};

} // namespace dyno
