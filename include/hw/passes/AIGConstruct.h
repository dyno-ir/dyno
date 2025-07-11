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
#include "op/IDs.h"
#include "support/ArrayRef.h"
#include "support/Bits.h"
#include "support/Utility.h"
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

  AIGNodeTRef resolveBit(HWValue val, uint bit) {
    if (auto asWire = val.dyn_as<WireRef>())
      return resolveWire(asWire)[bit];
    else if (auto asConst = val.dyn_as<ConstantRef>())
      // todo: what do we want to do with unknown values?
      return asConst.getBit(bit).val ? aig.getOne() : aig.getZero();
    dyno_unreachable("unknown type");
  }

  void buildNode(WireRef out, HWValue lhs, HWValue rhs, bool invertOut,
                 bool invertLHS, bool invertRHS) {
    uint32_t pos = wireToAIGNodeStorage.size();
    auto numBits = *lhs.getNumBits();
    for (uint i = 0; i < numBits; i++) {
      AIGNodeTRef lhsNode = resolveBit(lhs, i);
      AIGNodeTRef rhsNode = resolveBit(rhs, i);

      lhsNode = invertLHS ? lhsNode.inverted() : lhsNode;
      rhsNode = invertRHS ? rhsNode.inverted() : rhsNode;

      auto node = aig.createNode(lhsNode, rhsNode);

      node = invertOut ? node.inverted() : node;
      wireToAIGNodeStorage.emplace_back(node);
    }
    wireToAIGNode[out] = ThinArrayRef<AIGNodeTRef>{pos, numBits};
  }

  void buildAnd(WireRef out, HWValue lhs, HWValue rhs) {
    buildNode(out, lhs, rhs, false, false, false);
  }
  void buildOr(WireRef out, HWValue lhs, HWValue rhs) {
    buildNode(out, lhs, rhs, true, true, true);
  }

  void buildExtTrunc(WireRef out, WireRef in, bool sign = false) {
    auto inNumBits = *in.getNumBits();
    auto outNumBits = *out.getNumBits();
    wireToAIGNode[out] = ThinArrayRef<AIGNodeTRef>{
        (uint32_t)wireToAIGNodeStorage.size(), outNumBits};
    for (uint i = 0; i < std::min(inNumBits, outNumBits); i++) {
      AIGNodeTRef node = resolveBit(in, i);
      wireToAIGNodeStorage.emplace_back(node);
    }
    AIGNodeTRef extBit = sign ? resolveBit(in, inNumBits - 1) : aig.getZero();
    for (uint i = inNumBits; i < outNumBits; i++)
      wireToAIGNodeStorage.emplace_back(extBit);
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
  AIGObjRef aigRef;

  void handleInstr(InstrRef instr, AIGBuilder &abuild) {
    auto &aig = abuild.aig;
    switch (*instr.getDialectOpcode()) {
    // probably have to rework I/O. ideally have I/O agnostic of node-fatness.
    // i.e. AIG_IN/AIG_OUT can just reference random nodes.
    case *HW_LOAD: {
      build.setInsertPoint(instr);
      auto arr = abuild.buildInput(instr.as<LoadIRef>().value());
      auto ibuild = build.buildInstrRaw(AIG_INPUT, arr.size() + 2);
      for (auto ref : arr)
        ibuild.addRef(aig.store.resolve(ref).as<FatAIGNodeRef>());
      ibuild.other();
      ibuild.addRef(instr.as<LoadIRef>().value());
      ibuild.addRef(aigRef);
      break;
    }
    case *HW_STORE: {
      if (!instr.as<StoreIRef>().value().is<WireRef>())
        break;
      build.setInsertPoint(instr);
      auto arr = abuild.buildOutput(instr.as<StoreIRef>().value());
      auto ibuild = build.buildInstrRaw(AIG_OUTPUT, arr.size() + 2);
      ibuild.addRef(instr.as<StoreIRef>().value().as<WireRef>());
      for (auto ref : arr)
        ibuild.addRef(aig.store.resolve(ref).as<FatAIGNodeRef>());
      ibuild.other();
      ibuild.addRef(aigRef);
      break;
    }

    case *OP_AND: {
      assert(instr.getNumOperands() == 3);
      abuild.buildAnd(instr.def(0)->as<WireRef>(),
                      instr.other(0)->as<HWValue>(),
                      instr.other(1)->as<HWValue>());
      break;
    }
    case *OP_OR: {
      assert(instr.getNumOperands() == 3);
      abuild.buildOr(instr.def(0)->as<WireRef>(), instr.other(0)->as<HWValue>(),
                     instr.other(1)->as<HWValue>());
      break;
    }

    case *OP_TRUNC:
    case *OP_SEXT:
    case *OP_ZEXT: {
      abuild.buildExtTrunc(instr.def(0)->as<WireRef>(),
                           instr.other(0)->as<WireRef>(), instr.isOpc(OP_SEXT));
      break;
    }
    }
  }

  void runOnProc(ProcessIRef proc) {
    aigRef = ctx.getAIGs().create();

    AIGBuilder abuild{ctx, aigRef->aig};
    build.setInsertPoint(proc.block().begin());
    build.buildInstrRaw(AIG_GRAPH, 1).addRef(aigRef);

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
