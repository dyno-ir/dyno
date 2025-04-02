#pragma once

#include <dyno/Instr.h>
#include <dyno/ObjInfo.h>

namespace dyno {

constexpr DialectInfo scfDialectInfo{"scf"};

constexpr TyInfo scfTyInfo[] = {{"construct"}, {"func"}};

constexpr OpcodeInfo scfOpcodeInfo[] = {
#define HEADER
#define FOOTER
#define ADD_OP(x)                                                              \
  { #x }
#include "SCFOps.inc"
};

}; // namespace dyno
