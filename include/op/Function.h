#pragma once
#include "dyno/CFG.h"
#include "dyno/DialectInfo.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/DefUseMixin.h"
#include "hw/IDs.h"
#include "op/IDs.h"

namespace dyno {

class Function {
public:
  InstrDefUse defUse;

  // alternative to maintaining full copies here would be category-ordered
  // defUse.
  SmallVec<FatObjRef<Instr>, 4> params;
  SmallVec<FatObjRef<Instr>, 2> returns;

  Function(DynObjRef) {}
};

class FunctionRef : public FatObjRef<Function>,
                   public InstrDefUseMixin<FunctionRef> {
public:
  using FatObjRef<Function>::FatObjRef;
  FunctionRef(FatObjRef<Function> ref) : FatObjRef<Function>(ref) {}

  void addParam(InstrRef ref) {
    // todo: store arg index somewhere in instr.
    assert(ref.getDialect() == DIALECT_OP && ref.getOpcode() == OP_PARAM);
    ptr->params.emplace_back(ref);
  }
  void addReturn(InstrRef ref) {
    assert(ref.getDialect() == DIALECT_OP && ref.getOpcode() == OP_RETURN);
    ptr->returns.emplace_back(ref);
  }
};

template <> struct ObjTraits<Function> {
  static constexpr DialectID dialect{DIALECT_OP};
  static constexpr TyID ty{SCF_FUNC};
  using FatRefT = Function;
};

class FuncInstrRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  FuncInstrRef(const InstrRef &ref) : InstrRef(ref) {}

  FunctionRef func() { return this->def()->as<FunctionRef>(); }
  uint getNumParams() { return func()->params.size(); }

  BlockRef getBlock() { return this->operand(1)->as<BlockRef>(); }

  auto blocks() {
    return func()->defUse.uses().filter([](OperandRef ref) {
      return ref.instr().getDialect() == DIALECT_HW &&
             ref.instr().getOpcode() == HW_BLOCK_INSTR;
    });
  }
};
}; // namespace dyno
