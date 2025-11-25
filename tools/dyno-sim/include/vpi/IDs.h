

#pragma once

#include "dyno/IDImpl.h"
#include "dyno/Obj.h"
#include "dyno/Opcode.h"
#include "dyno/Type.h"
#include <cstdint>
#include <dyno/DialectInfo.h>

namespace dyno {

enum VPIDialectID : uint8_t { DIALECT_VPI = 4 };
using VPIType = SpecificDialectType<DialectID{DIALECT_VPI}>;
using VPIOpcode = SpecificDialectOpcode<DialectID{DIALECT_VPI}>;

// clang-format off
#define TYPES(x) \
  x("iterator",         VPI_ITERATOR, 0) \
  x("range",            VPI_RANGE, 0) \
// clang-format on

#define ENUM_EXPAND(name, ident, idx) ident = idx,

enum class VPITyID : uint8_t { TYPES(ENUM_EXPAND) };
#undef ENUM_EXPAND

#define CEXPR_EXPAND(name, ident, idx) constexpr VPIType ident{uint8_t(VPITyID::ident)};
TYPES(CEXPR_EXPAND)
#undef CEXPR_EXPAND


#define HEADER enum class VPIOpcID : uint16_t {
#define ADD_OP(x) VPI_##x,
#include "VPIInstrs.inc"

#define HEADER
#define FOOTER
#define ADD_OP(x) constexpr VPIOpcode VPI_##x{uint16_t(VPIOpcID::VPI_##x)};
#include "VPIInstrs.inc"

template <> struct DialectTraits<DIALECT_VPI> {
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
#include "VPIInstrs.inc"
  };
};

#undef TYPES


} // namespace dyno
