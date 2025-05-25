

#pragma once
#include "dyno/Obj.h"
#include "dyno/Opcode.h"
#include <cstdint>
#include <dyno/DialectInfo.h>

namespace dyno {
enum OpTyID : uint8_t { OP_FUNC = 0 | TY_DEF_USE_START };

using OpOpcode = SpecificDialectOpcode<DialectID{DIALECT_OP}>;

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
