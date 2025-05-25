#pragma once
#include "dyno/CFG.h"
#include "dyno/DialectInfo.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/DefUseMixin.h"
#include "hw/IDs.h"
#include "op/IDs.h"
#include "support/Optional.h"

namespace dyno {

class Function {
public:
  InstrDefUse defUse;

  // alternative to maintaining full copies here would be category-ordered
  // defUse.
  SmallVec<InstrRef, 4> params;

  Function(DynObjRef) {}
};

class FunctionIRef;

class FunctionRef : public FatObjRef<Function>,
                    public InstrDefUseMixin<FunctionRef> {
public:
  using FatObjRef<Function>::FatObjRef;
  FunctionRef(FatObjRef<Function> ref) : FatObjRef<Function>(ref) {}

  void addParam(InstrRef ref) {
    // todo: store arg index somewhere in instr.
    assert(ref.getDialectOpcode() == OP_PARAM);
    ptr->params.emplace_back(ref);
  }

  FunctionIRef iref() const;
};

template <> struct ObjTraits<Function> {
  static constexpr DialectID dialect{DIALECT_OP};
  static constexpr TyID ty{OP_FUNC};
  using FatRefT = Function;
};

class FunctionIRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  FunctionIRef(const InstrRef &ref) : InstrRef(ref) {}

  FunctionRef func() { return this->def()->as<FunctionRef>(); }
  uint getNumParams() { return func()->params.size(); }
  BlockRef getBlock() { return this->def(1)->as<BlockRef>(); }

  static bool is_impl(const FatObjRef<Instr> &ref) {
    return InstrRef{ref}.isOpc(OP_FUNC_INSTR);
  }
  static bool is_impl(const FatDynObjRef<> &ref) {
    if (auto asInstr = ref.dyn_as<InstrRef>())
      return is_impl(asInstr);
    return false;
  }
};

inline FunctionIRef FunctionRef::iref() const {
  return ptr->defUse.getDef(0)->instr();
}

class CallInstrRef : public InstrRef {
public:
  using InstrRef::InstrRef;

  FunctionRef func() { return this->other(0)->as<FunctionRef>(); }

  Range<iterator> retvals() { return this->defs(); }
  auto getNumRetvals() { return this->getNumDefs(); }

  Range<iterator> params() {
    return Range{this->other_begin() + 1, this->other_end()};
  }
  auto getNumParams() { return this->getNumOthers() - 1; }
};
}; // namespace dyno
