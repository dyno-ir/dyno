

#pragma once

#include <cstdint>
#include <dyno/IDs.h>

namespace dyno {

enum HWDialectID : uint8_t { DIALECT_RTL = DIALECT_CUSTOM };

enum RTLTyID : uint8_t { RTL_WIRE = bit_mask_msb<uint8_t>() };

} // namespace dyno
