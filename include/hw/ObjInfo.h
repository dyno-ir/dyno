#pragma once

#include <dyno/Instr.h>
#include <dyno/ObjInfo.h>

namespace dyno {

constexpr DialectInfo rtlDialectInfo{"rtl"};

constexpr TyInfo rtlTyInfo[] = {{"wire"}};

constexpr OpcodeInfo rtlOpcodeInfo[] = {{"a"}, {"b"}, {"c"}, {"d"}};

}; // namespace dyno
