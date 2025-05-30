#pragma once
#include "dyno/CFG.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/DefUseMixin.h"
#include "op/IDs.h"
#include "support/ArrayRef.h"

namespace dyno {

// defs: (true_block, (false_block, vreg...)); uses: (cond_vreg)
class IfInstrRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  IfInstrRef(const InstrRef &ref) : InstrRef(ref) {}

  bool hasFalseBlock() { return getNumDefs() >= 2; }
  uint getNumYieldValues() { return getNumDefs() < 2 ? 0 : getNumDefs() - 2; }

  OperandRef getCondValue() {
    return this->operand(getNumDefs());
  }
  BlockRef getTrueBlock() { return this->operand(0)->as<BlockRef>(); }
  BlockRef getFalseBlock() {
    if (!hasFalseBlock())
      return nullref;
    return this->operand(1)->as<BlockRef>();
  }
  // do not ref specific value vreg like Wire here
  OperandRef getYieldValue(uint n = 0) {
    assert(n < getNumYieldValues());
    return this->operand(2 + n);
  }
};

// defs block, wire... ; uses: cond
class SwitchInstrRef : public InstrRef {
public:
  using InstrRef::InstrRef;

  auto block() { return this->def(0)->as<BlockRef>(); }
  auto yieldValues() { return Range{this->def_begin() + 1, this->def_end()}; }
  auto cond() { return this->other(0); }
};

class CaseInstrRef : public InstrRef {
public:
  using InstrRef::InstrRef;

  auto block() { return this->def(0)->as<BlockRef>(); }
  auto labels() { return this->others(); }
  bool hasSingleLabel() { return this->getNumOthers() == 1; }
  bool isDefault() { return this->isOpc(OP_CASE_DEFAULT); }
};

// defs: (cond_bl, body_bl, vreg...); uses: (cond_vreg, vreg...)
class WhileInstrRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  WhileInstrRef(const InstrRef &ref) : InstrRef(ref) {}

  uint getNumYieldValues() { return (getNumDefs() - 2); }
  auto yieldValues() { return Range{this->def_begin() + 2, this->def_end()}; }

  BlockRef getCondBlock() { return this->operand(0)->as<BlockRef>(); }
  BlockRef getBodyBlock() { return this->operand(1)->as<BlockRef>(); }
  FatDynObjRef<> getYieldValue(uint n = 0) {
    assert(n < getNumYieldValues());
    return this->operand(2 + n)->as<FatDynObjRef<>>();
  }
  FatDynObjRef<> getInputValue(uint n = 0) {
    assert(n < getNumYieldValues());
    return this->operand(getNumDefs() + n)->as<FatDynObjRef<>>();
  }
};

class DoWhileInstrRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  DoWhileInstrRef(const InstrRef &ref) : InstrRef(ref) {}

  uint getNumYieldValues() { return (getNumDefs() - 1); }
  auto yieldValues() { return Range{this->def_begin() + 1, this->def_end()}; }

  BlockRef getBlock() { return this->operand(0)->as<BlockRef>(); }
  FatDynObjRef<> getYieldValue(uint n = 0) {
    assert(n < getNumYieldValues());
    return this->operand(1 + n)->as<FatDynObjRef<>>();
  }
  FatDynObjRef<> getInputValue(uint n = 0) {
    assert(n < getNumYieldValues());
    return this->operand(getNumDefs() + n)->as<FatDynObjRef<>>();
  }
};

class ForInstrRef : public InstrRef {
public:
  using InstrRef::InstrRef;

  uint getNumYieldValues() { return (getNumDefs() - 1); }
  BlockRef getBlock() { return this->operand(0)->as<BlockRef>(); }

  auto yieldValues() { return Range{this->def_begin() + 1, this->def_end()};}
  auto getLower() { return this->other(0); }
  auto getUpper() { return this->other(1); }
  auto getStep() { return this->other(2); }
};

}; // namespace dyno
