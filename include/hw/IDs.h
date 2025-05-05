

#pragma once

#include "dyno/Obj.h"
#include <cstdint>
#include <dyno/DialectInfo.h>

namespace dyno {

enum HWDialectID : uint8_t { DIALECT_RTL = DIALECT_CUSTOM };
enum RTLTyID : uint8_t {
  RTL_WIRE = 0 | TY_DEF_USE_START,
  RTL_REGISTER = 1 | TY_DEF_USE_START,
  RTL_PROCESS = 2 | TY_DEF_USE_START,
  RTL_MODULE = 3 | TY_DEF_USE_START
};

#define HEADER enum HWOpID : uint16_t {
#define ADD_OP(x) HW_##x
#include "HWOps.inc"

template <> struct DialectTraits<DIALECT_RTL> {
  constexpr static DialectInfo info{"rtl"};
  constexpr static TyInfo tyInfo[] = {
      {"wire"}, {"register"}, {"process"}, {"module"}};

  constexpr static OpcodeInfo opcInfo[] = {
#define HEADER
#define FOOTER
#define ADD_OP(x) {#x}
#include "HWOps.inc"
  };
};

} // namespace dyno
