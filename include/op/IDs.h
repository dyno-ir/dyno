

#pragma once
#include "dyno/Obj.h"
#include <cstdint>
#include <dyno/DialectInfo.h>

namespace dyno {
enum OpTyID : uint8_t {
  SCF_FUNC = 0 | TY_DEF_USE_START
};

#define HEADER enum OpOpcID : uint16_t {
#define ADD_OP(x) OP_##x
#include "OpInstrs.inc"


template <> struct DialectTraits<DIALECT_OP> {
  constexpr static DialectInfo info{"op"};
  constexpr static TyInfo tyInfo[] = {{"func"}};
  constexpr static OpcodeInfo opcInfo[] = {
#define HEADER
#define FOOTER
#define ADD_OP(x) {#x}
#include "OpInstrs.inc"
  };
};

} // namespace dyno
