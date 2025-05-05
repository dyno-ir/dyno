#pragma once

#include "DialectInfo.h"
#include "dyno/Obj.h"
#include <cstdint>
#include <string_view>
#include <support/Bits.h>

namespace dyno {

enum CoreTyID : uint8_t {
  CORE_INSTR = 0,
  CORE_CONSTANT,
  CORE_BLOCK = 2 | TY_DEF_USE_START,
};

struct CorePrint {
  static TyInfo::print_func printConstant;
};

template <> struct DialectTraits<DIALECT_CORE> {
  constexpr static DialectInfo info{"core"};
  constexpr static TyInfo tyInfo[] = {
      {"instr"}, {"constant", CorePrint::printConstant}, {"block"}};
  constexpr static OpcodeInfo opcInfo[] = {{"block_instr"}};
};

} // namespace dyno
