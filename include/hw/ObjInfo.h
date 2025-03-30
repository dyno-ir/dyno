#pragma once

#include <dyno/Instr.h>
#include <dyno/ObjInfo.h>
#include <support/StringHandling.h>

namespace dyno {

constexpr DialectInfo rtlDialectInfo{"rtl"};

constexpr TyInfo rtlTyInfo[] = {{"wire"}, {"register"}, {"process"}, {"module"}};

constexpr OpcodeInfo rtlOpcodeInfo[] = {
#define HEADER
#define FOOTER
#define ADD_OP(x)                                                              \
  { #x }
#include "HWOps.inc"
};

}; // namespace dyno
