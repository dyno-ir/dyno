#pragma once

#include "dyno/Instr.h"
#include "dyno/InstrMixin.h"
#include "dyno/Obj.h"
#include "hw/IDs.h"
#include "support/Optional.h"
#include "support/Utility.h"

namespace dyno {

class Register;

class OptionalU32OrReg {
  enum Kind { EMPTY, VALUE, PARAM };
  Kind kind;
  union {
    uint32_t val;
    ObjRef<Register> ref;
  };

public:
  OptionalU32OrReg(uint32_t val) : kind(VALUE), val(val) {}
  OptionalU32OrReg(Optional<uint32_t> val)
      : kind(val ? VALUE : EMPTY), val(val.value_or(0)) {}
  OptionalU32OrReg(nullopt_t) : kind(EMPTY) {}
  OptionalU32OrReg() : kind(EMPTY) {}
  OptionalU32OrReg(ObjRef<Register> param) : kind(PARAM), ref(param) {}
  uint32_t operator*() const {
    switch (kind) {
    case VALUE:
      return val;
    default:
      dyno_unreachable("value not defined");
    }
  }
  explicit operator bool() const { return kind == VALUE; }
  explicit operator uint32_t() const { return **this; }
  operator Optional<uint32_t>() const {
    return (*this) ? Optional<uint32_t>(**this) : nullopt;
  }
  bool operator==(uint32_t o) const { return Optional<uint32_t>(*this) == o; }

  bool isReg() const { return kind == PARAM; }
  ObjRef<Register> getReg() const {
    assert(isReg());
    return ref;
  }
  ObjRef<Register> reg() { return isReg() ? ref : nullref; }
  uint32_t value_or(uint32_t alt) const { return (*this) ? (**this) : alt; }
};

class Register {
  friend class RegisterRef;
  friend class ModuleIRef;

public:
  InstrDefUse defUse;
  // todo: split into separate type
  OptionalU32OrReg numBits;

  Register(DynObjRef, OptionalU32OrReg numBits = nullopt) : numBits(numBits) {}
  // todo: pass context into copier s.t. we can copy reg name and init value
  // (side tables) as well
  Register(DynObjRef, FatObjRef<Register> other) : numBits(other->numBits) {}
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
                                 HW_REF_REGISTER_DEF, HW_PARAM_REGISTER_DEF);
  }
  static bool is_impl(FatDynObjRef<> ref) {
    if (auto asInstr = ref.dyn_as<InstrRef>())
      return is_impl(asInstr);
    return false;
  }

  auto useInstrs(auto... opcs) {
    return oref()
        .uses()
        .filter([opcs...](auto use) { return use.instr().isOpc(opcs...); })
        .transform([](size_t, auto op) { return op.instr(); });
  }
  auto loads() { return useInstrs(HW_LOAD); }
  auto stores() { return useInstrs(HW_STORE); }
  auto storeDefers() { return useInstrs(HW_STORE_DEFER); }
  auto storeOrStoreDefers() { return useInstrs(HW_STORE_DEFER, HW_STORE); }
  auto memLoads() { return useInstrs(HW_MEM_LOAD); }
  auto memStores() { return useInstrs(HW_MEM_STORE); }

  bool isDynAddressed();

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
