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

class RegisterIRef;

class RegisterRef : public FatObjRef<Register>,
                    public InstrDefUseMixin<RegisterRef> {
public:
  using FatObjRef<Register>::FatObjRef;
  RegisterRef(FatObjRef<Register> ref) : FatObjRef<Register>(ref) {}

  auto &getNumBits() { return ptr->numBits; }

  RegisterIRef iref();
};

class RegisterIRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  RegisterIRef(InstrRef ref) : InstrRef(ref) {}
  RegisterRef oref() { return def(0)->as<RegisterRef>(); }

  auto &getNumBits() { return oref().getNumBits(); }

  static bool is_impl(FatObjRef<Instr> instr) {
    return InstrRef{instr}.isOpc(HW_REGISTER_DEF, HW_INPUT_REGISTER_DEF,
                                 HW_OUTPUT_REGISTER_DEF, HW_INOUT_REGISTER_DEF,
                                 HW_REF_REGISTER_DEF);
  }
  static bool is_impl(FatDynObjRef<> ref) {
    if (auto asInstr = ref.dyn_as<InstrRef>())
      return is_impl(asInstr);
    return false;
  }

  InstrRef getSingleStore() {
    InstrRef rv = nullref;
    for (auto use : oref().uses()) {
      if (use.instr().isOpc(HW_STORE, HW_STORE_DEFER)) {
        if (rv)
          return nullref;
        rv = use.instr();
      }
    }
    return rv;
  }
  InstrRef getSingleLoad() {
    InstrRef rv = nullref;
    for (auto use : oref().uses()) {
      if (use.instr().isOpc(HW_LOAD)) {
        if (rv)
          return nullref;
        rv = use.instr();
      }
    }
    return rv;
  }
};

inline RegisterIRef RegisterRef::iref() {
  return getSingleDef()->instr().as<RegisterIRef>();
}

template <> struct ObjTraits<Register> {
  // static constexpr DialectID dialect{DIALECT_HW};
  static constexpr DialectType ty{HW_REGISTER};
  // static constexpr auto altTys = {HW_REGISTER_POS, HW_REGISTER_NEG,
  //                                 HW_REGISTER_ANY};
  using FatRefT = RegisterRef;
};

}; // namespace dyno
