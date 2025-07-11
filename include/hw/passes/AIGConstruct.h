#pragma once

#include "aig/AIG.h"
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
  AIG &aig;
  ObjMapVec<Wire, ThinArrayRef<AIGNodeTRef>> wireToAIGNode;
  std::vector<AIGNodeTRef> wireToAIGNodeStorage;

public:
  AIGBuilder(AIG &aig) : aig(aig) {}
  ArrayRef<AIGNodeTRef> resolveWire(WireRef wire) {
    return wireToAIGNode[wire].resolve(ArrayRef{wireToAIGNodeStorage});
  }

  void buildAnd(WireRef out, HWValue lhs, HWValue rhs) {
    if (lhs.is<WireRef>() && rhs.is<WireRef>()) {
      auto lhsNodes = resolveWire(lhs.as<WireRef>());
      auto rhsNodes = resolveWire(rhs.as<WireRef>());

      uint32_t pos = wireToAIGNodeStorage.size();
      auto numBits = *lhs.as<WireRef>().getNumBits();
      for (uint i = 0; i < numBits; i++) {
        auto node = aig.createNode(lhsNodes[i], rhsNodes[i]);
        wireToAIGNodeStorage.emplace_back(node);
      }
      wireToAIGNode[out] = ThinArrayRef<AIGNodeTRef>{pos, numBits};
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
    return arr.resolve(MutArrayRef{wireToAIGNodeStorage});
  }
  auto buildOutput(WireRef wire) {
    auto numBits = *wire.getNumBits();
    SmallVec<AIGNodeTRef, 4> arr;
    auto nodes = resolveWire(wire);
    for (uint i = 0; i < numBits; i++) {
      auto node = aig.createOutput(nodes[i]);
      arr.emplace_back(node);
    }
    return arr;
  }
};

class AIGConstructPass {
  HWContext &ctx;

  AIG aig;
  AIGBuilder abuild;
  HWInstrBuilder build;

  void handleInstr(InstrRef instr) {
    switch (*instr.getDialectOpcode()) {

    // probably have to rework I/O. ideally have I/O agnostic of node-fatness.
    // i.e. AIG_IN/AIG_OUT can just reference random nodes.
    case *HW_LOAD: {
      auto arr = abuild.buildInput(instr.as<LoadIRef>().value());
      auto ibuild = build.buildInstrRaw(HW_AIG_IN, arr.size() + 1);
      for (auto ref : arr)
        ibuild.addRef(aig.store.resolve(ref).as<FatAIGNodeRef>());
      break;
    }
    case *HW_STORE: {

      // AIG output
      break;
    }

    case *OP_AND: {
      break;
    }
    }
  }

  void runOnProc(ProcessIRef proc) {
    for (auto instr : proc.block()) {
      handleInstr(instr);
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
    }
  }

  explicit AIGConstructPass(HWContext &ctx)
      : ctx(ctx), abuild(aig), build(ctx) {}
};

}; // namespace dyno
