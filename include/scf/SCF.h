#pragma once
#include "dyno/CFG.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/DefUseMixin.h"
#include "scf/IDs.h"

namespace dyno {

// use this for all scf constructs to keep obj bloat down. two purposes:
// vreg for yields, and as parent to owned blocks (not yet implemented)
class SCFConstruct {
public:
  InstrDefUse defUse;

  SCFConstruct(DynObjRef) {}
};

class SCFConstructRef : public FatObjRef<SCFConstruct>,
                        public InstrDefUseMixin<SCFConstructRef> {
public:
  using FatObjRef<SCFConstruct>::FatObjRef;
  SCFConstructRef(FatObjRef<SCFConstruct> ref) : FatObjRef<SCFConstruct>(ref) {}
};

template <> struct ObjTraits<SCFConstruct> {
  static constexpr DialectID dialect{DIALECT_SCF};
  static constexpr TyID ty{SCF_CONSTRUCT};
  using FatRefT = SCFConstruct;
};

// defs: (yield_vreg, wire...); uses: (true_block, false_block)
class IfInstrRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  IfInstrRef(const InstrRef &ref) : InstrRef(ref) {}

  // todo: could use some custom bits in instr to figure out whether false block
  // or yield_vreg exist.

  FatDynObjRef<> getCondValue() {
    return this->operand(getNumOperands() - 3)->as<FatDynObjRef<>>();
  }
  BlockRef getTrueBlock() {
    return this->operand(getNumOperands() - 2)->as<BlockRef>();
  }
  BlockRef getFalseBlock() {
    return this->operand(getNumOperands() - 1)->as<BlockRef>();
  }
  SCFConstructRef getSCFConstruct() {
    return this->operand(0)->as<SCFConstructRef>();
  }
  // do not ref specific value vreg like Wire here
  FatDynObjRef<> getYieldValue(int n = 0) {
    return this->operand(1 + n)->as<FatDynObjRef<>>();
  }
  uint numYieldWires() { return getNumOperands() - 3; }
};

}; // namespace dyno
