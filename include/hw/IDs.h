

#pragma once

#include "dyno/IDImpl.h"
#include "dyno/Obj.h"
#include "dyno/Opcode.h"
#include "dyno/Type.h"
#include <cstdint>
#include <dyno/DialectInfo.h>

namespace dyno {

using HWType = SpecificDialectType<DialectID{DIALECT_HW}>;
using HWOpcode = SpecificDialectOpcode<DialectID{DIALECT_HW}>;

// clang-format off
#define TYPES(x) \
  x("wire",         HW_WIRE,         0 | TY_DEF_USE_START) \
  x("register",     HW_REGISTER,     1 | TY_DEF_USE_START) \
  x("process",      HW_PROCESS,      2 | TY_DEF_USE_START) \
  x("module",       HW_MODULE,       3 | TY_DEF_USE_START) \
  x("sens_modes",   HW_SENS_MODES,   4) \
  x("trigger",      HW_TRIGGER,      5 | TY_DEF_USE_START) \
  x("stdcell_info", HW_STDCELL_INFO, 6)
// clang-format on

#define ENUM_EXPAND(name, ident, idx) ident = idx,

enum class HWTyID : uint8_t { TYPES(ENUM_EXPAND) };
#undef ENUM_EXPAND

#define CEXPR_EXPAND(name, ident, idx) constexpr HWType ident{uint8_t(HWTyID::ident)};
TYPES(CEXPR_EXPAND)
#undef CEXPR_EXPAND


#define HEADER enum class HWOpcID : uint16_t {
#define ADD_OP(x) HW_##x,
#include "HWInstrs.inc"

#define HEADER
#define FOOTER
#define ADD_OP(x) constexpr HWOpcode HW_##x{uint16_t(HWOpcID::HW_##x)};
#include "HWInstrs.inc"

template <> struct DialectTraits<DIALECT_HW> {
  constexpr static DialectInfo info{"hw"};
  constexpr static TyInfo tyInfo[] = {
    #define TYINFO_EXPAND(name, ident, idx) {name, !!((idx) & TY_DEF_USE_START)},
    TYPES(TYINFO_EXPAND)
    #undef TYINFO_EXPAND
  };
  constexpr static OpcodeInfo opcInfo[] = {
#define HEADER
#define FOOTER
#define ADD_OP(x) {#x},
#include "HWInstrs.inc"
  };
};

#undef TYPES

// opcode, builder, constant builder, big int
#define FOR_HW_COMM_OPS(FUNC)                                                  \
  FUNC(OP_ADD, buildAdd, add, BigInt::addOp4S)                                 \
  FUNC(OP_AND, buildAnd, bitAND, BigInt::andOp4S)                              \
  FUNC(OP_OR, buildOr, bitOR, BigInt::orOp4S)                                  \
  FUNC(OP_XOR, buildXor, bitXOR, BigInt::xorOp4S)                              \
  FUNC(OP_MUL, buildMul, mul, BigInt::mulOp4S)

#define FOR_HW_BIN_OPS(FUNC)                                                   \
  FUNC(OP_SUB, buildSub, sub, BigInt::subOp4S)                                 \
  FUNC(OP_UDIV, buildUDiv, udiv, BigInt::udivOp4S)                             \
  FUNC(OP_UMOD, buildUMod, umod, BigInt::umodOp4S)                             \
  FUNC(OP_SDIV, buildSDiv, sdiv, BigInt::sdivOp4S)                             \
  FUNC(OP_SMOD, buildSMod, smod, BigInt::smodOp4S)                             \
  FUNC(OP_SLL, buildSLL, shl, BigInt::shlOp4S)                                 \
  FUNC(OP_SRL, buildSRL, lshr, BigInt::lshrOp4S)                               \
  FUNC(OP_SRA, buildSRA, ashr, BigInt::ashrOp4S)                               \
  FUNC(HW_UPOW, buildUPow, upow, BigInt::upowOp4S)                             \
  FUNC(HW_SPOW, buildSPow, spow, BigInt::spowOp4S)

#define FOR_HW_N_OPS(FUNC) FOR_HW_COMM_OPS(FUNC)

#define FOR_HW_SIMPLE_OPS(FUNC) FOR_HW_COMM_OPS(FUNC) FOR_HW_BIN_OPS(FUNC)

#define MAKE_CASE(opc, ...) case *opc:

} // namespace dyno
