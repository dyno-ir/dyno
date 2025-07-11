#pragma once

#include "aig/AIG.h"
#include "aig/IDs.h"
#include "dyno/IDImpl.h"
#include "dyno/Instr.h"
#include "dyno/NewDeleteObjStore.h"
#include "dyno/Obj.h"
#include "dyno/ObjMap.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWValue.h"
#include "hw/LoadStore.h"
#include "hw/Process.h"
#include "support/ArrayRef.h"
#include "support/Bits.h"
#include <array>
#include <limits>

namespace dyno {

class AIGBuilder {
  HWContext &ctx;
  ObjMapVec<Wire, ThinArrayRef<AIGNodeTRef>> wireToAIGNode;
  std::vector<AIGNodeTRef> wireToAIGNodeStorage;

public:
  AIG &aig;
  AIGBuilder(HWContext &ctx, AIG &aig) : ctx(ctx), aig(aig) {
    wireToAIGNode.resize(ctx.getWires().numIDs());
  }
  ArrayRef<AIGNodeTRef> resolveWire(WireRef wire) {
    return wireToAIGNode[wire].resolve(
        ArrayRef{wireToAIGNodeStorage.begin().base(),
                 wireToAIGNodeStorage.end().base()});
  }

  void buildAnd(WireRef out, HWValue lhs, HWValue rhs) {
    if (lhs.is<WireRef>() && rhs.is<WireRef>()) {
      uint32_t pos = wireToAIGNodeStorage.size();
      auto numBits = *lhs.as<WireRef>().getNumBits();
      for (uint i = 0; i < numBits; i++) {
        auto node = aig.createNode(resolveWire(lhs.as<WireRef>())[i],
                                   resolveWire(rhs.as<WireRef>())[i]);
        wireToAIGNodeStorage.emplace_back(node);
      }
      wireToAIGNode[out] = ThinArrayRef<AIGNodeTRef>{pos, numBits};
      return;
    }
    assert(0 && "todo");
  }

  MutArrayRef<AIGNodeTRef> buildInput(WireRef wire) {
    auto numBits = *wire.getNumBits();
    auto arr = ThinArrayRef<AIGNodeTRef>{(uint32_t)wireToAIGNodeStorage.size(),
                                         numBits};
    for (uint i = 0; i < numBits; i++) {
      auto node = aig.createInput();
      wireToAIGNodeStorage.emplace_back(node.as<AIGNodeRef>());
    }
    wireToAIGNode[wire] = arr;
    return arr.resolve(MutArrayRef{wireToAIGNodeStorage.begin().base(),
                                   wireToAIGNodeStorage.end().base()});
  }
  auto buildOutput(HWValue wire) {
    auto numBits = *wire.getNumBits();
    SmallVec<AIGNodeTRef, 4> arr;
    auto nodes = resolveWire(wire.as<WireRef>());
    for (uint i = 0; i < numBits; i++) {
      auto node = aig.createOutput(nodes[i]);
      arr.emplace_back(node.as<AIGNodeRef>());
    }
    return arr;
  }
};

class AIGConstructPass {
  HWContext &ctx;
  HWInstrBuilder build;

  void handleInstr(InstrRef instr, AIGBuilder &abuild) {
    auto &aig = abuild.aig;
    switch (*instr.getDialectOpcode()) {
    // probably have to rework I/O. ideally have I/O agnostic of node-fatness.
    // i.e. AIG_IN/AIG_OUT can just reference random nodes.
    case *HW_LOAD: {
      build.setInsertPoint(instr);
      auto arr = abuild.buildInput(instr.as<LoadIRef>().value());
      auto ibuild = build.buildInstrRaw(AIG_INPUT, arr.size() + 1);
      for (auto ref : arr)
        ibuild.addRef(aig.store.resolve(ref).as<FatAIGNodeRef>());
      ibuild.other();
      ibuild.addRef(instr.as<LoadIRef>().value());
      break;
    }
    case *HW_STORE: {
      if (!instr.as<StoreIRef>().value().is<WireRef>())
        break;
      build.setInsertPoint(instr);
      auto arr = abuild.buildOutput(instr.as<StoreIRef>().value());
      auto ibuild = build.buildInstrRaw(AIG_OUTPUT, arr.size() + 1);
      ibuild.addRef(instr.as<StoreIRef>().value().as<WireRef>());
      for (auto ref : arr)
        ibuild.addRef(aig.store.resolve(ref).as<FatAIGNodeRef>());
      break;
    }

    case *OP_AND: {
      assert(instr.getNumOperands() == 3);
      abuild.buildAnd(instr.def(0)->as<WireRef>(),
                      instr.other(0)->as<HWValue>(),
                      instr.other(1)->as<HWValue>());
      break;
    }
    }
  }

  void runOnProc(ProcessIRef proc) {
    auto aig = ctx.getAIGs().create();

    AIGBuilder abuild{ctx, aig->aig};
    build.setInsertPoint(proc.block().begin());
    build.buildInstrRaw(AIG_INSTR, 1).addRef(aig);

    for (auto instr : proc.block()) {
      handleInstr(instr, abuild);
    }
  }

  void runOnModule(ModuleIRef module) {
    for (auto proc : module.procs()) {
      runOnProc(proc);
    }
  }

public:
  void run() {
    for (auto mod : ctx.getModules()) {
      runOnModule(mod.iref());
    }
  }

  explicit AIGConstructPass(HWContext &ctx) : ctx(ctx), build(ctx) {}
};

}; // namespace dyno
