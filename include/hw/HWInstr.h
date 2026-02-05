#pragma once
#include "dyno/Context.h"
#include "dyno/RefUnion.h"
#include "hw/Module.h"
#include "hw/Process.h"
#include "op/Function.h"

namespace dyno {

class HWInstrRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  HWInstrRef(const InstrRef &ref) : InstrRef(ref) {}

  WireRef operandW(unsigned n) { return operand(n)->as<WireRef>(); }
  WireRef defW(unsigned n = 0) {
    assert(n < getNumDefs());
    return operandW(n);
  }
  // todo: get rid of ctx params via global directory.
  auto iter(Context &ctx) {
    return ctx.getCtx<CoreDialectContext>().cfg[this->as<ObjRef<Instr>>()];
  }
  BlockRef parentBlock(Context &ctx) {
    return ctx.getCtx<CoreDialectContext>().cfg.contains(
               this->as<ObjRef<Instr>>())
               ? iter(ctx).blockRef()
               : nullref;
  }
  FatRefUnion<ProcessIRef, FunctionIRef> parent(Context &ctx) {
    while (true) {
      auto block = parentBlock(ctx);
      assert(block);
      if (auto parent =
              block.defI().dyn_as<FatRefUnion<ProcessIRef, FunctionIRef>>())
        return parent;
      return HWInstrRef{block.defI()}.parent(ctx);
    }
  }
  ProcessIRef parentProc(Context &ctx) {
    auto rv = parent(ctx);
    assert(rv.is<ProcessIRef>() && "parent is not process");
    return rv.as<ProcessIRef>();
  }
  FunctionIRef parentFunc(Context &ctx) {
    auto rv = parent(ctx);
    assert(rv.is<FunctionIRef>() && "parent is not function");
    return rv.as<FunctionIRef>();
  }
  ModuleIRef parentMod(Context &ctx) {
    while (true) {
      auto block = parentBlock(ctx);
      if (auto mod = block.defI().dyn_as<ModuleIRef>())
        return mod;
      return HWInstrRef{block.defI()}.parentMod(ctx);
    }
  }
  bool isDescendantOf(BlockRef block, Context &ctx) {
    HWInstrRef instr = *this;
    while (ctx.getCtx<CoreDialectContext>().cfg.contains(instr)) {
      auto parent = ctx.getCtx<CoreDialectContext>().cfg[instr];
      if (parent.blockRef() == block)
        return true;
      instr = parent.blockRef().defI();
    }
    return false;
  }
};

}; // namespace dyno
