#pragma once

#include "support/Optional.h"
#include <dyno/Instr.h>
#include <dyno/Obj.h>
#include <hw/DefUseMixin.h>
#include <hw/IDs.h>

namespace dyno {
class Wire {
public:
  InstrDefUse defUse;
  Optional<uint32_t> numBits;
  Wire(DynObjRef, Optional<uint32_t> numBits = nullopt) : numBits(numBits) {}
};

class WireRef : public FatObjRef<Wire>, public InstrDefUseMixin<WireRef> {
public:
  using FatObjRef<Wire>::FatObjRef;
  WireRef(FatObjRef<Wire> ref) : FatObjRef<Wire>(ref) {}

  Optional<uint32_t> getNumBits() { return ptr->numBits; }

  auto getDefI() { return getDef().instr(); }
};

template <> struct ObjTraits<Wire> {
  static constexpr DialectID dialect{DIALECT_HW};
  static constexpr TyID ty{HW_WIRE};
  using FatRefT = WireRef;
};

} // namespace dyno
