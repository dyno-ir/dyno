#pragma once

#include <dyno/Instr.h>
#include <dyno/Obj.h>
#include <hw/IDs.h>

namespace dyno {
class Wire {
public:
  InstrDefUse defUse;

  auto getSingleDef() { return defUse.getSingleDef(); }
  auto hasSingleDef() { return defUse.hasSingleDef(); }
  auto getSingleUse() { return defUse.getSingleUse(); }
  auto hasSingleUse() { return defUse.hasSingleUse(); }

  auto getSingleDefI() { return getSingleDef()->instr(); }
  auto getSingleDefW();

  Wire(DynObjRef) {}
};

using WireRef = FatObjRef<Wire>;

template <> struct ObjTraits<Wire> {
  static constexpr DialectID dialect{DIALECT_RTL};
  static constexpr TyID ty{RTL_WIRE};
  using FatRefT = WireRef;
};

inline auto Wire::getSingleDefW() { return (*getSingleDef())->as<WireRef>(); }

} // namespace dyno
