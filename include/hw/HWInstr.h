#pragma once
#include "dyno/RefUnion.h"
#include "hw/HWContext.h"

namespace dyno {

class HWInstrRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  HWInstrRef(const InstrRef &ref) : InstrRef(ref) {}

  WireRef operandW(uint n) { return operand(n)->as<WireRef>(); }
  WireRef defW(uint n = 0) {
    assert(n < getNumDefs());
    return operandW(n);
  }
  // todo: get rid of ctx params via global directory.
  auto iter(HWContext &ctx) { return ctx.getCFG()[this->as<ObjRef<Instr>>()]; }
  BlockRef parentBlock(HWContext &ctx) { return iter(ctx).blockRef(); }
  FatRefUnion<ProcessIRef, FunctionIRef> parent(HWContext &ctx) {
    while (true) {
      auto block = parentBlock(ctx);
      if (auto parent =
              block.defI().dyn_as<FatRefUnion<ProcessIRef, FunctionIRef>>())
        return parent;
      return HWInstrRef{block.defI()}.parent(ctx);
    }
  }
  ProcessIRef parentProc(HWContext &ctx) {
    auto rv = parent(ctx);
    assert(rv.is<ProcessIRef>() && "parent is not process");
    return rv.as<ProcessIRef>();
  }
  FunctionIRef parentFunc(HWContext &ctx) {
    auto rv = parent(ctx);
    assert(rv.is<FunctionIRef>() && "parent is not function");
    return rv.as<FunctionIRef>();
  }
  ModuleIRef parentMod(HWContext &ctx) {
    while (true) {
      auto block = parentBlock(ctx);
      if (auto mod = block.defI().dyn_as<ModuleIRef>())
        return mod;
      return HWInstrRef{block.defI()}.parentMod(ctx);
    }
  }
};

}; // namespace dyno
