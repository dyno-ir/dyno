#pragma once

#include "dyno/DialectInfo.h"
#include "dyno/Obj.h"
#include "dyno/Opcode.h"
#include "dyno/Type.h"
#include <cstdint>

namespace dyno {
using DSLType = SpecificDialectType<DialectID{DIALECT_DSL}>;
using DSLOpcode = SpecificDialectOpcode<DialectID{DIALECT_DSL}>;

// clang-format off
#define TYPES(x) \
  x("val",      DSL_VAL,      0 | TY_DEF_USE_START) \
  x("null",      DSL_NULL,      1)
// clang-format on

#define ENUM_EXPAND(name, ident, idx) ident = idx,
enum class DSLTyID : uint8_t { TYPES(ENUM_EXPAND) };
#undef ENUM_EXPAND

#define CEXPR_EXPAND(name, ident, idx)                                         \
  constexpr DSLType ident{uint8_t(DSLTyID::ident)};
TYPES(CEXPR_EXPAND)
#undef CEXPR_EXPAND

#define HEADER enum class DSLOpcID : uint16_t {
#define ADD_OP(x) DSL_##x,
#include "DSLInstrs.inc"

#define HEADER
#define FOOTER
#define ADD_OP(x) constexpr DSLOpcode DSL_##x{uint16_t(DSLOpcID::DSL_##x)};
#include "DSLInstrs.inc"

template <> struct DialectTraits<DIALECT_DSL> {
  constexpr static DialectInfo info{"dsl"};
  constexpr static TyInfo tyInfo[] = {
#define TYINFO_EXPAND(name, ident, idx) {name, !!((idx) & TY_DEF_USE_START)},
      TYPES(TYINFO_EXPAND)
#undef TYINFO_EXPAND
  };
  constexpr static OpcodeInfo opcInfo[] = {
#define HEADER
#define FOOTER
#define ADD_OP(x) {#x},
#include "DSLInstrs.inc"
  };
};

#undef TYPES

} // namespace dyno
