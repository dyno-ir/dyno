#pragma once

#include "dyno/DialectInfo.h"
#include "dyno/Obj.h"
#include "dyno/Opcode.h"
#include "dyno/Type.h"
#include <cstdint>

namespace dyno {
using AIGType = SpecificDialectType<DialectID{DIALECT_AIG}>;
using AIGOpcode = SpecificDialectOpcode<DialectID{DIALECT_AIG}>;

// clang-format off
#define TYPES(x) \
  x("aig",      AIG_AIG,      0 | TY_DEF_USE_START) \
  x("fat_node", AIG_FAT_NODE, 1 | TY_DEF_USE_START) \
  x("node",     AIG_NODE,     2 | TY_DEF_USE_START)
// clang-format on

#define ENUM_EXPAND(name, ident, idx) ident = idx,
enum class AIGTyID : uint8_t { TYPES(ENUM_EXPAND) };
#undef ENUM_EXPAND

#define CEXPR_EXPAND(name, ident, idx)                                         \
  constexpr AIGType ident{uint8_t(AIGTyID::ident)};
TYPES(CEXPR_EXPAND)
#undef CEXPR_EXPAND

#define HEADER enum class AIGOpcID : uint16_t {
#define ADD_OP(x) AIG_##x,
#include "AIGInstrs.inc"

#define HEADER
#define FOOTER
#define ADD_OP(x) constexpr AIGOpcode AIG_##x{uint16_t(AIGOpcID::AIG_##x)};
#include "AIGInstrs.inc"

template <> struct DialectTraits<DIALECT_AIG> {
  constexpr static DialectInfo info{"aig"};
  constexpr static TyInfo tyInfo[] = {
#define TYINFO_EXPAND(name, ident, idx) {name, !!((idx) & TY_DEF_USE_START)},
      TYPES(TYINFO_EXPAND)
#undef TYINFO_EXPAND
  };
  constexpr static OpcodeInfo opcInfo[] = {
#define HEADER
#define FOOTER
#define ADD_OP(x) {#x},
#include "AIGInstrs.inc"
  };
};

#undef TYPES

} // namespace dyno
