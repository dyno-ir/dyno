

#pragma once
#include "dyno/Obj.h"
#include <cstdint>
#include <dyno/IDs.h>

namespace dyno {

// enum SCFDialectID : uint8_t { DIALECT_SCF = DIALECT_SCF };
enum SCFTyID : uint8_t { SCF_CONSTRUCT = 0 | TY_DEF_USE_START, SCF_FUNC = 1 | TY_DEF_USE_START };

#define HEADER enum SCFOpID : uint16_t {
#define ADD_OP(x) SCF_##x
#include "SCFOps.inc"

} // namespace dyno
