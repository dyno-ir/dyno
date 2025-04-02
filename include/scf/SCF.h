#pragma once
#include "dyno/CFG.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/DefUseMixin.h"
#include "scf/IDs.h"

namespace dyno {

// use this for all scf constructs to keep obj bloat down. two purposes:
// vreg for yields, and as parent to owned blocks
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

// defs: (scfconstr, vreg...); uses: (cond_vreg, true_block, false_block)
class IfInstrRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  IfInstrRef(const InstrRef &ref) : InstrRef(ref) {}

  bool hasFalseBlock() { return getNumOperands() > 2; }
  uint getNumYieldValues() { return getNumDefs() - 1; }

  FatDynObjRef<> getCondValue() {
    return this->operand(getNumOperands() - 3)->as<FatDynObjRef<>>();
  }
  BlockRef getTrueBlock() {
    return this->operand(getNumOperands() - (hasFalseBlock() ? 2 : 1))
        ->as<BlockRef>();
  }
  BlockRef getFalseBlock() {
    if (!hasFalseBlock())
      return nullref;
    return this->operand(getNumOperands() - 1)->as<BlockRef>();
  }
  SCFConstructRef getSCFConstruct() {
    return this->operand(0)->as<SCFConstructRef>();
  }
  // do not ref specific value vreg like Wire here
  FatDynObjRef<> getYieldValue(uint n = 0) {
    assert(n < getNumYieldValues());
    return this->operand(1 + n)->as<FatDynObjRef<>>();
  }
};

// defs: (scfconstr, vreg...); uses: (cond_bl, body_bl, vreg...)
class WhileInstrRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  WhileInstrRef(const InstrRef &ref) : InstrRef(ref) {}

  uint getNumYieldValues() { return (getNumDefs() - 1); }

  BlockRef getCondBlock() {
    return this->operand(getNumDefs())->as<BlockRef>();
  }
  BlockRef getBodyBlock() {
    return this->operand(getNumDefs() + 1)->as<BlockRef>();
  }
  SCFConstructRef getSCFConstruct() {
    return this->operand(0)->as<SCFConstructRef>();
  }
  FatDynObjRef<> getYieldValue(uint n = 0) {
    assert(n < getNumYieldValues());
    return this->operand(1 + n)->as<FatDynObjRef<>>();
  }
  FatDynObjRef<> getInputValue(uint n = 0) {
    assert(n < getNumYieldValues());
    return this->operand(2 + getNumYieldValues() + n)->as<FatDynObjRef<>>();
  }
};

}; // namespace dyno
