#pragma once
#include "dyno/CFG.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/DefUseMixin.h"
#include "op/IDs.h"

namespace dyno {

// defs: (true_block, (false_block, vreg...)); uses: (cond_vreg)
class IfInstrRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  IfInstrRef(const InstrRef &ref) : InstrRef(ref) {}

  bool hasFalseBlock() { return getNumDefs() >= 2; }
  uint getNumYieldValues() { return getNumDefs() < 2 ? 0 : getNumDefs() - 2; }

  FatDynObjRef<> getCondValue() {
    return this->operand(getNumDefs())->as<FatDynObjRef<>>();
  }
  BlockRef getTrueBlock() { return this->operand(0)->as<BlockRef>(); }
  BlockRef getFalseBlock() {
    if (!hasFalseBlock())
      return nullref;
    return this->operand(1)->as<BlockRef>();
  }
  // do not ref specific value vreg like Wire here
  FatDynObjRef<> getYieldValue(uint n = 0) {
    assert(n < getNumYieldValues());
    return this->operand(2 + n)->as<FatDynObjRef<>>();
  }
};

// defs: (cond_bl, body_bl, vreg...); uses: (cond_vreg, vreg...)
class WhileInstrRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  WhileInstrRef(const InstrRef &ref) : InstrRef(ref) {}

  uint getNumYieldValues() { return (getNumDefs() - 2); }

  BlockRef getCondBlock() {
    return this->operand(0)->as<BlockRef>();
  }
  BlockRef getBodyBlock() {
    return this->operand(1)->as<BlockRef>();
  }
  FatDynObjRef<> getYieldValue(uint n = 0) {
    assert(n < getNumYieldValues());
    return this->operand(2 + n)->as<FatDynObjRef<>>();
  }
  FatDynObjRef<> getInputValue(uint n = 0) {
    assert(n < getNumYieldValues());
    return this->operand(getNumDefs() + n)->as<FatDynObjRef<>>();
  }
};

}; // namespace dyno
