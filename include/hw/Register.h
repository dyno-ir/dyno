#pragma once

#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/DefUseMixin.h"
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

class RegisterRef : public FatObjRef<Register>,
                    public InstrDefUseMixin<RegisterRef> {
public:
  using FatObjRef<Register>::FatObjRef;
  RegisterRef(FatObjRef<Register> ref) : FatObjRef<Register>(ref) {}

  auto &getNumBits() { return ptr->numBits; }
};

class RegisterIRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  RegisterIRef(InstrRef ref) : InstrRef(ref) {}
  RegisterRef oref() { return def(0)->as<RegisterRef>(); }

  static bool is_impl(FatObjRef<Instr> instr) {
    return InstrRef{instr}.isOpc(
        HW_REGISTER_INSTR, HW_INPUT_REGISTER_INSTR, HW_OUTPUT_REGISTER_INSTR,
        HW_INOUT_REGISTER_INSTR, HW_REF_REGISTER_INSTR);
  }
};

template <> struct ObjTraits<Register> {
  //static constexpr DialectID dialect{DIALECT_HW};
  static constexpr DialectType ty{HW_REGISTER};
  using FatRefT = RegisterRef;
};

}; // namespace dyno
