#pragma once

#include <cstdint>
#include <support/Bits.h>

namespace dyno {

enum CoreDialectID : uint8_t {
  DIALECT_CORE = 0,
  DIALECT_CUSTOM,
};

enum CoreTyID : uint8_t {
  CORE_INSTR = 0,
  CORE_BLOCK,
  CORE_CONSTANT,
};

} // namespace dyno
