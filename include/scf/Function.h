#pragma once
#include "dyno/CFG.h"
#include "dyno/IDs.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/DefUseMixin.h"
#include "hw/IDs.h"
#include "scf/IDs.h"

namespace dyno {

class SCFFunc {
public:
  InstrDefUse defUse;

  // alternative to maintaining full copies here would be category-ordered
  // defUse.
  SmallVec<FatObjRef<Instr>, 4> params;
  SmallVec<FatObjRef<Instr>, 2> returns;

  SCFFunc(DynObjRef) {}
};

class SCFFuncRef : public FatObjRef<SCFFunc>,
                   public InstrDefUseMixin<SCFFuncRef> {
public:
  using FatObjRef<SCFFunc>::FatObjRef;
  SCFFuncRef(FatObjRef<SCFFunc> ref) : FatObjRef<SCFFunc>(ref) {}

  void addParam(InstrRef ref) {
    // todo: store arg index somewhere in instr.
    assert(ref.getDialect() == DIALECT_SCF && ref.getOpcode() == SCF_PARAM);
    ptr->params.emplace_back(ref);
  }
  void addReturn(InstrRef ref) {
    assert(ref.getDialect() == DIALECT_SCF && ref.getOpcode() == SCF_RETURN);
    ptr->returns.emplace_back(ref);
  }
};

template <> struct ObjTraits<SCFFunc> {
  static constexpr DialectID dialect{DIALECT_SCF};
  static constexpr TyID ty{SCF_FUNC};
  using FatRefT = SCFFunc;
};

class FuncInstrRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  FuncInstrRef(const InstrRef &ref) : InstrRef(ref) {}

  SCFFuncRef func() { return this->def()->as<SCFFuncRef>(); }
  uint getNumParams() { return func()->params.size(); }

  BlockRef getBlock() { return this->operand(2)->as<BlockRef>(); }

  auto blocks() {
    return func()->defUse.uses().filter([](OperandRef ref) {
      return ref.instr().getDialect() == DIALECT_RTL && ref.instr().getOpcode() == HW_BLOCK_INSTR;
    });
  }
};
}; // namespace dyno
