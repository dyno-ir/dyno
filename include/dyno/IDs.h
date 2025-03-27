#pragma once

#include "dyno/Obj.h"
#include <cstdint>
#include <support/Bits.h>

namespace dyno {

enum CoreDialectID : uint8_t {
  DIALECT_CORE = 0,
  DIALECT_CUSTOM,
};

enum CoreTyID : uint8_t {
  CORE_INSTR = 0,
  CORE_CONSTANT,
  CORE_BLOCK = 2 | TY_DEF_USE_START,
};

} // namespace dyno
