

#pragma once
#include "dyno/Obj.h"
#include "dyno/Opcode.h"
#include "dyno/Type.h"
#include "support/Utility.h"
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

#define FOR_OP_COMPARE_OPS(FUNC)                                               \
  FUNC(OP_ICMP_EQ, BigInt::ICMP_EQ)                                            \
  FUNC(OP_ICMP_NE, BigInt::ICMP_NE)                                            \
  FUNC(OP_ICMP_ULT, BigInt::ICMP_ULT)                                          \
  FUNC(OP_ICMP_SLT, BigInt::ICMP_SLT)                                          \
  FUNC(OP_ICMP_ULE, BigInt::ICMP_ULE)                                          \
  FUNC(OP_ICMP_SLE, BigInt::ICMP_SLE)                                          \
  FUNC(OP_ICMP_UGT, BigInt::ICMP_UGT)                                          \
  FUNC(OP_ICMP_SGT, BigInt::ICMP_SGT)                                          \
  FUNC(OP_ICMP_UGE, BigInt::ICMP_UGE)                                          \
  FUNC(OP_ICMP_SGE, BigInt::ICMP_SGE)

#define FOR_OP_SPECIAL_COMPARE_OPS(FUNC)                                       \
  FUNC(OP_ICMP_CEQ, BigInt::ICMP_CEQ)                                          \
  FUNC(OP_ICMP_CNE, BigInt::ICMP_CNE)                                          \
  FUNC(OP_ICMP_WEQ, BigInt::ICMP_WEQ)                                          \
  FUNC(OP_ICMP_WNE, BigInt::ICMP_WNE)                                          \
  FUNC(OP_ICMP_CZEQ, BigInt::ICMP_CZEQ)                                        \
  FUNC(OP_ICMP_CZNE, BigInt::ICMP_CZNE)                                        \
  FUNC(OP_ICMP_CXEQ, BigInt::ICMP_CXEQ)                                        \
  FUNC(OP_ICMP_CXNE, BigInt::ICMP_CXNE)

#define FOR_OP_ALL_COMPARE_OPS(FUNC)                                           \
  FOR_OP_COMPARE_OPS(FUNC)                                                     \
  FOR_OP_SPECIAL_COMPARE_OPS(FUNC)

inline constexpr DialectOpcode
reverseOperandOrderICmpOpcode(DialectOpcode opc) {
  switch (*opc) {
  case *OP_ICMP_EQ:
    return OP_ICMP_EQ;
  case *OP_ICMP_NE:
    return OP_ICMP_NE;
  case *OP_ICMP_SLT:
    return OP_ICMP_SGE;
  case *OP_ICMP_ULT:
    return OP_ICMP_UGE;
  case *OP_ICMP_SLE:
    return OP_ICMP_SGT;
  case *OP_ICMP_ULE:
    return OP_ICMP_UGT;
  case *OP_ICMP_SGT:
    return OP_ICMP_SLE;
  case *OP_ICMP_UGT:
    return OP_ICMP_ULE;
  case *OP_ICMP_SGE:
    return OP_ICMP_SLT;
  case *OP_ICMP_UGE:
    return OP_ICMP_ULT;
  default:
    dyno_unreachable("");
  }
}

} // namespace dyno
