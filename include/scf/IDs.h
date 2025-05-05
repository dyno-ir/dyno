

#pragma once
#include "dyno/Obj.h"
#include <cstdint>
#include <dyno/DialectInfo.h>

namespace dyno {
enum SCFTyID : uint8_t {
  SCF_CONSTRUCT = 0 | TY_DEF_USE_START,
  SCF_FUNC = 1 | TY_DEF_USE_START
};

#define HEADER enum SCFOpID : uint16_t {
#define ADD_OP(x) SCF_##x
#include "SCFOps.inc"


template <> struct DialectTraits<DIALECT_SCF> {
  constexpr static DialectInfo info{"scf"};
  constexpr static TyInfo tyInfo[] = {{"construct"}, {"func"}};
  constexpr static OpcodeInfo opcInfo[] = {
#define HEADER
#define FOOTER
#define ADD_OP(x) {#x}
#include "SCFOps.inc"
  };
};

} // namespace dyno
