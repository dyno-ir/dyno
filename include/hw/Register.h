#pragma once

#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/IDs.h"
#include "support/Optional.h"

namespace dyno {

class Register {
  friend class RegisterRef;
  friend class ModuleIRef;

public:
  // enum PortType : uint8_t {
  //   PORT_NONE,
  //   PORT_IN,
  //   PORT_OUT,
  //   PORT_INOUT,
  //   PORT_REF,
  //   PORT_PARAM_IN
  // };
  InstrDefUse defUse;
  Optional<uint32_t> numBits;

  Register(DynObjRef, Optional<uint32_t> numBits = nullopt)
      : numBits(numBits) {}
};

class RegisterRef : public FatObjRef<Register> {
public:
  using FatObjRef<Register>::FatObjRef;
  RegisterRef(FatObjRef<Register> ref) : FatObjRef<Register>(ref) {}

  auto &getNumBits() { return ptr->numBits; }
};

template <> struct ObjTraits<Register> {
  static constexpr DialectID dialect{DIALECT_HW};
  static constexpr TyID ty{HW_REGISTER};
  using FatRefT = RegisterRef;
};

}; // namespace dyno
