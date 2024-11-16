#pragma once

#include <dyno/Instr.h>
#include <dyno/Obj.h>
#include <hw/IDs.h>

namespace dyno {

class Wire {
public:
  InstrDefUse defUse;

  Wire(DynObjRef) {}
};

using WireRef = FatDynObjRef<Wire>;

template <> struct ObjTraits<Wire> {
  static constexpr DialectID dialect{DIALECT_RTL};
  static constexpr TyID ty{RTL_WIRE};
  using FatRefT = WireRef;
};
} // namespace dyno
