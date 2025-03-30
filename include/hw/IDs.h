

#pragma once

#include "dyno/Obj.h"
#include <cstdint>
#include <dyno/IDs.h>

namespace dyno {

enum HWDialectID : uint8_t { DIALECT_RTL = DIALECT_CUSTOM };
enum RTLTyID : uint8_t { RTL_WIRE = TY_DEF_USE_START, RTL_REGISTER, RTL_PROCESS, RTL_MODULE };

#define HEADER enum HWOpID : uint16_t {
#define ADD_OP(x) HW_##x
#include "HWOps.inc"

} // namespace dyno
