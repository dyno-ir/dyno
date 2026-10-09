#pragma once

#include "hw/Register.h"
#include "support/Optional.h"
#include <dyno/Instr.h>
#include <dyno/InstrMixin.h>
#include <dyno/Obj.h>
#include <hw/IDs.h>

namespace dyno {
class Wire {
public:
  InstrDefUse defUse;
  OptionalU32OrReg numBits;
  Wire(DynObjRef, OptionalU32OrReg numBits = nullopt) : numBits(numBits) {}
  Wire(DynObjRef, FatObjRef<Wire> other) : numBits(other->numBits) {}

  static bool isInitialized(const Wire *wire) {
    return !(reinterpret_cast<const unsigned char *>(wire)[0] == 0xFF &
             reinterpret_cast<const unsigned char *>(wire)[1] == 0xFF);
  }
  static void setUninitialized(Wire *wire) {
    reinterpret_cast<unsigned char *>(wire)[0] = 0xFF;
    reinterpret_cast<unsigned char *>(wire)[1] = 0xFF;
  }
};

class WireRef : public FatObjRef<Wire>, public InstrDefUseMixin<WireRef> {
public:
  using FatObjRef<Wire>::FatObjRef;
  WireRef(FatObjRef<Wire> ref) : FatObjRef<Wire>(ref) {}

  auto &getNumBits() const { return ptr->numBits; }

  auto getDefI() { return getDef().instr(); }
};

template <> struct ObjTraits<Wire> {
  // static constexpr DialectID dialect{DIALECT_HW};
  static constexpr DialectType ty{HW_WIRE};
  using FatRefT = WireRef;
};

} // namespace dyno
