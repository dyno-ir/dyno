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

  static bool isInitialized(const Wire *wire) {
    return !(reinterpret_cast<const char *>(wire)[0] &&
             reinterpret_cast<const char *>(wire)[1]);
  }
  static void setUninitialized(Wire *wire) {
    reinterpret_cast<char *>(wire)[0] = 0xFF;
    reinterpret_cast<char *>(wire)[1] = 0xFF;
  }
};

class WireRef : public FatObjRef<Wire>, public InstrDefUseMixin<WireRef> {
public:
  using FatObjRef<Wire>::FatObjRef;
  WireRef(FatObjRef<Wire> ref) : FatObjRef<Wire>(ref) {}

  Optional<uint32_t> getNumBits() { return ptr->numBits; }

  auto getDefI() { return getDef().instr(); }
};

template <> struct ObjTraits<Wire> {
  // static constexpr DialectID dialect{DIALECT_HW};
  static constexpr DialectType ty{HW_WIRE};
  using FatRefT = WireRef;
};

} // namespace dyno
