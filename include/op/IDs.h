

#pragma once
#include "dyno/Obj.h"
#include "dyno/Opcode.h"
#include "dyno/Type.h"
#include <cstdint>
#include <dyno/DialectInfo.h>

namespace dyno {
using OpType = SpecificDialectType<DialectID{DIALECT_OP}>;
using OpOpcode = SpecificDialectOpcode<DialectID{DIALECT_OP}>;

// clang-format off
#define TYPES(x) \
  x(OP_FUNC, 0 | TY_DEF_USE_START)
// clang-format on

#define ENUM_EXPAND(ident, idx) ident = idx,

enum class OpTyID : uint8_t { TYPES(ENUM_EXPAND) };

#define CEXPR_EXPAND(ident, idx) constexpr OpType ident{uint8_t(OpTyID::ident)};

TYPES(CEXPR_EXPAND)

#undef TYPES
#undef ENUM_EXPAND
#undef CEXPR_EXPAND

#define HEADER enum class OpOpcID : uint16_t {
#define ADD_OP(x) OP_##x,
#include "OpInstrs.inc"

#define HEADER
#define FOOTER
#define ADD_OP(x) constexpr OpOpcode OP_##x{uint16_t(OpOpcID::OP_##x)};
#include "OpInstrs.inc"

template <> struct DialectTraits<DIALECT_OP> {
  constexpr static DialectInfo info{"op"};
  constexpr static TyInfo tyInfo[] = {{"func"}};
  constexpr static OpcodeInfo opcInfo[] = {
#define HEADER
#define FOOTER
#define ADD_OP(x) {#x},
#include "OpInstrs.inc"
  };
};

} // namespace dyno
