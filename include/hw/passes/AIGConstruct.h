#pragma once

#include "aig/AIG.h"
#include "aig/AIGContext.h"
#include "aig/IDs.h"
#include "dyno/Context.h"
#include "dyno/Instr.h"
#include "dyno/NewDeleteObjStore.h"
#include "dyno/Obj.h"
#include "dyno/ObjMap.h"
#include "dyno/Pass.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Process.h"
#include "op/IDs.h"
#include "support/ArrayRef.h"
#include "support/Debug.h"
#include "support/Utility.h"

namespace dyno {

class AIGBuilder {
  Context &ctx [[maybe_unused]];
  ObjMapVec<Wire, ThinArrayRef<AIGNodeTRef>> wireToAIGNode;
  std::vector<AIGNodeTRef> wireToAIGNodeStorage;

public:
  AIG &aig;
  AIGBuilder(Context &ctx, AIG &aig) : ctx(ctx), aig(aig) {
    wireToAIGNode.resize(ctx.getStore<Wire>().numIDs());
  }
  ArrayRef<AIGNodeTRef> resolveWire(WireRef wire) {
    if (wireToAIGNode[wire].size() == 0) {
      DYNO_DBG("AIGBuilder", {
        dbgs() << "undefined wire, def instr:\n";
        dumpInstr(wire.getDefI());
      })
      dyno_unreachable("");
    }
    return wireToAIGNode[wire].resolve(
        ArrayRef{wireToAIGNodeStorage.begin().base(),
                 wireToAIGNodeStorage.end().base()});
  }

  AIGNodeTRef resolveBit(HWValue val, unsigned bit) {
    if (auto asWire = val.dyn_as<WireRef>()) {
      auto resolved = resolveWire(asWire);
      assert(asWire.getNumBits() == resolved.size());
      return resolved[bit];
    } else if (auto asConst = val.dyn_as<ConstantRef>())
      // todo: what do we want to do with unknown values?
      return asConst.getBit(bit).val ? aig.getOne() : aig.getZero();
    dyno_unreachable("unknown type");
  }

  void buildBinop(WireRef out, HWValue lhs, HWValue rhs,
                  std::invocable<AIGNodeTRef, AIGNodeTRef> auto buildFunc) {
    uint32_t pos = wireToAIGNodeStorage.size();
    auto numBits = *lhs.getNumBits();
    for (unsigned i = 0; i < numBits; i++) {
      AIGNodeTRef lhsNode = resolveBit(lhs, i);
      AIGNodeTRef rhsNode = resolveBit(rhs, i);
      auto node = buildFunc(lhsNode, rhsNode);
      wireToAIGNodeStorage.emplace_back(node);
    }
    wireToAIGNode[out] = ThinArrayRef<AIGNodeTRef>{pos, numBits};
  }
  void buildNOT(WireRef out, HWValue val) {
    uint32_t pos = wireToAIGNodeStorage.size();
    auto numBits = *val.getNumBits();
    for (unsigned i = 0; i < numBits; i++) {
      AIGNodeTRef node = resolveBit(val, i).inverted();
      wireToAIGNodeStorage.emplace_back(node);
    }
    wireToAIGNode[out] = ThinArrayRef<AIGNodeTRef>{pos, numBits};
  }

  void buildAND(WireRef out, HWValue lhs, HWValue rhs) {
    buildBinop(out, lhs, rhs, [&](AIGNodeTRef lhs, AIGNodeTRef rhs) {
      return aig.createAND(lhs, rhs);
    });
  }
  void buildOR(WireRef out, HWValue lhs, HWValue rhs) {
    buildBinop(out, lhs, rhs, [&](AIGNodeTRef lhs, AIGNodeTRef rhs) {
      return aig.createOR(lhs, rhs);
    });
  }
  void buildXOR(WireRef out, HWValue lhs, HWValue rhs) {
    buildBinop(out, lhs, rhs, [&](AIGNodeTRef lhs, AIGNodeTRef rhs) {
      return aig.createXOR(lhs, rhs);
    });
  }
  void buildXNOR(WireRef out, HWValue lhs, HWValue rhs) {
    buildBinop(out, lhs, rhs, [&](AIGNodeTRef lhs, AIGNodeTRef rhs) {
      return aig.createXNOR(lhs, rhs);
    });
  }
  void buildMUX(WireRef out, HWValue sel, HWValue trueV, HWValue falseV) {
    auto selNode = resolveBit(sel, 0);
    buildBinop(out, trueV, falseV, [&](AIGNodeTRef lhs, AIGNodeTRef rhs) {
      return aig.createMUX(selNode, lhs, rhs);
    });
  }
  void buildSplice(SpliceIRef instr) {
    uint32_t pos = wireToAIGNodeStorage.size();
    assert(instr.isConstantOffs());

    auto val = instr.in()->as<HWValue>();
    auto low = instr.getBase();
    auto len = instr.getLen();
    for (unsigned j = 0; j < len; j++) {
      wireToAIGNodeStorage.emplace_back(resolveBit(val, low + j));
    }

    uint32_t numBits = wireToAIGNodeStorage.size() - pos;
    wireToAIGNode[instr.def(0)->as<WireRef>()] =
        ThinArrayRef<AIGNodeTRef>{pos, numBits};
  }
  void buildConcat(InstrRef instr) {
    uint32_t pos = wireToAIGNodeStorage.size();
    for (unsigned i = instr.getNumOthers(); i-- > 0;) {
      auto val = instr.other(i)->as<HWValue>();
      for (unsigned j = 0; j < *val.getNumBits(); j++) {
        wireToAIGNodeStorage.emplace_back(resolveBit(val, j));
      }
    }
    uint32_t numBits = wireToAIGNodeStorage.size() - pos;
    wireToAIGNode[instr.def(0)->as<WireRef>()] =
        ThinArrayRef<AIGNodeTRef>{pos, numBits};
  }
  void buildInsert(InsertIRef insert) {
    assert(insert.isConstantOffs());

    uint32_t pos = wireToAIGNodeStorage.size();
    for (unsigned i = 0; i < insert.getMemoryLen(); i++) {
      if (i >= insert.getBase() && i < insert.getBase() + insert.getLen()) {
        wireToAIGNodeStorage.emplace_back(
            resolveBit(insert.val()->as<HWValue>(), i - insert.getBase()));
        continue;
      }
      wireToAIGNodeStorage.emplace_back(
          resolveBit(insert.in()->as<HWValue>(), i));
    }
    uint32_t numBits = wireToAIGNodeStorage.size() - pos;
    assert(numBits == insert.getMemoryLen());
    wireToAIGNode[insert.out()->as<WireRef>()] =
        ThinArrayRef<AIGNodeTRef>{pos, numBits};
  }
  void buildRepeat(InstrRef instr) {
    uint32_t pos = wireToAIGNodeStorage.size();
    auto val = instr.other(0)->as<HWValue>();
    assert(*instr.def(0)->as<WireRef>().getNumBits() % *val.getNumBits() == 0);
    auto count = *instr.def(0)->as<WireRef>().getNumBits() / *val.getNumBits();
    for (unsigned i = 0; i < count; i++) {
      for (unsigned j = 0; j < *val.getNumBits(); j++) {
        wireToAIGNodeStorage.emplace_back(resolveBit(val, j));
      }
    }
    uint32_t numBits = wireToAIGNodeStorage.size() - pos;
    wireToAIGNode[instr.def(0)->as<WireRef>()] =
        ThinArrayRef<AIGNodeTRef>{pos, numBits};
  }

  void buildNOP(WireRef out, HWValue in) {
    uint32_t pos = wireToAIGNodeStorage.size();
    auto val = in;
    for (unsigned i = 0; i < *val.getNumBits(); i++) {
      wireToAIGNodeStorage.emplace_back(resolveBit(val, i));
    }
    uint32_t numBits = wireToAIGNodeStorage.size() - pos;
    wireToAIGNode[out] = ThinArrayRef<AIGNodeTRef>{pos, numBits};
  }

  void buildExtTrunc(WireRef out, WireRef in, bool sign = false) {
    auto inNumBits = *in.getNumBits();
    auto outNumBits = *out.getNumBits();
    wireToAIGNode[out] = ThinArrayRef<AIGNodeTRef>{
        (uint32_t)wireToAIGNodeStorage.size(), outNumBits};
    for (unsigned i = 0; i < std::min(inNumBits, outNumBits); i++) {
      AIGNodeTRef node = resolveBit(in, i);
      wireToAIGNodeStorage.emplace_back(node);
    }
    AIGNodeTRef extBit = sign ? resolveBit(in, inNumBits - 1) : aig.getZero();
    for (unsigned i = inNumBits; i < outNumBits; i++)
      wireToAIGNodeStorage.emplace_back(extBit);
  }

  MutArrayRef<AIGNodeTRef> buildInput(WireRef wire) {
    auto numBits = *wire.getNumBits();
    auto arr = ThinArrayRef<AIGNodeTRef>{(uint32_t)wireToAIGNodeStorage.size(),
                                         numBits};
    for (unsigned i = 0; i < numBits; i++) {
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
    for (unsigned i = 0; i < numBits; i++) {
      auto node = aig.createOutput(nodes[i]);
      arr.emplace_back(node.as<AIGNodeRef>());
    }
    return arr;
  }
};

class AIGConstructPass : public Pass<AIGConstructPass> {
  Context &ctx;
  HWInstrBuilder build;
  AIGObjRef aigRef;
  ObjMapVec<Instr, bool> destroyMap;

  void buildAIGInputInstr(WireRef wire, AIGBuilder &abuild) {
    auto &aig = abuild.aig;
    auto arr = abuild.buildInput(wire);
    auto ibuild = build.buildInstrRaw(AIG_INPUT, arr.size() + 2);
    for (auto ref : arr)
      ibuild.addRef(aig.store.resolve(ref).as<FatAIGNodeRef>());
    ibuild.other();
    ibuild.addRef(wire);
    ibuild.addRef(aigRef);
  }
  void buildAIGOutputInstr(WireRef wire, AIGBuilder &abuild) {
    for (auto def : wire.defs())
      if (def.instr().isOpc(AIG_OUTPUT))
        return;
    for (auto use : wire.uses())
      if (use.instr().isOpc(AIG_INPUT))
        return;

    // auto nodes = abuild.resolveWire(wire);
    // if (std::all_of(nodes.begin(), nodes.end(),
    //                 [](AIGNodeTRef ref) { return ref.isSpecial(); })) {
    //   auto ibuild = build.buildInstrRaw(HW_CONCAT, nodes.size() + 1);
    //   build.setInsertPoint(ibuild.instr());
    //   ibuild.addRef(wire).other();
    //   for (auto node : Range{nodes}.reverse()) {
    //     auto fat = aigRef->aig.store.resolve(node).as<FatAIGNodeRef>();
    //     auto def = *fat->defUse.getSingleDef();
    //     auto instr = def.instr();
    //     assert(instr.isOpc(AIG_INPUT));
    //     auto val = build.buildSplice(instr.other(0)->as<HWValue>(), 1,
    //                                  def - *instr.def_begin());
    //     ibuild.addRef(val);
    //   }
    //   return;
    // }

    auto &aig = abuild.aig;
    auto arr = abuild.buildOutput(wire);
    auto ibuild = build.buildInstrRaw(AIG_OUTPUT, arr.size() + 2);
    ibuild.addRef(wire);
    for (auto ref : arr)
      ibuild.addRef(aig.store.resolve(ref).as<FatAIGNodeRef>());
    ibuild.other();
    ibuild.addRef(aigRef);
  }

  void handleInstr(InstrRef instr, AIGBuilder &abuild) {
    auto &aig = abuild.aig;
    switch (*instr.getDialectOpcode()) {
    // case *HW_LOAD: {
    //   build.setInsertPoint(instr);
    //   build.setInsertPoint(build.insert.succ());
    //   auto arr = abuild.buildInput(instr.as<LoadIRef>().value());
    //   auto ibuild = build.buildInstrRaw(AIG_INPUT, arr.size() + 2);
    //   for (auto ref : arr)
    //     ibuild.addRef(aig.store.resolve(ref).as<FatAIGNodeRef>());
    //   ibuild.other();
    //   ibuild.addRef(instr.as<LoadIRef>().value());
    //   ibuild.addRef(aigRef);
    //   return;
    // }
    // case *HW_STORE_DEFER:
    // case *HW_STORE: {
    //   if (!instr.as<StoreIRef>().value().is<WireRef>())
    //     return;
    //   build.setInsertPoint(instr);
    //   auto arr = abuild.buildOutput(instr.as<StoreIRef>().value());
    //   auto ibuild = build.buildInstrRaw(AIG_OUTPUT, arr.size() + 2);
    //   ibuild.addRef(instr.as<StoreIRef>().value().as<WireRef>());
    //   for (auto ref : arr)
    //     ibuild.addRef(aig.store.resolve(ref).as<FatAIGNodeRef>());
    //   ibuild.other();
    //   ibuild.addRef(aigRef);
    //   return;
    // }

    // Keep all unmatched instructions, just pipe their defs/uses in/out of the
    // AIG as IOs.
    case *AIG_INPUT:
    case *AIG_OUTPUT:
      return;

    default: {
      build.setInsertPoint(instr);
      for (auto use : instr.others()) {
        if (auto asWire = use->dyn_as<WireRef>())
          buildAIGOutputInstr(asWire, abuild);
      }

      build.insert = std::next(build.insert);

      for (auto def : instr.defs()) {
        if (auto asWire = def->dyn_as<WireRef>())
          buildAIGInputInstr(asWire, abuild);
      }
      return;
    }

    case *OP_AND: {
      assert(instr.getNumOperands() == 3);
      abuild.buildAND(instr.def(0)->as<WireRef>(),
                      instr.other(0)->as<HWValue>(),
                      instr.other(1)->as<HWValue>());
      break;
    }
    case *OP_OR: {
      assert(instr.getNumOperands() == 3);
      abuild.buildOR(instr.def(0)->as<WireRef>(), instr.other(0)->as<HWValue>(),
                     instr.other(1)->as<HWValue>());
      break;
    }
    case *OP_XOR: {
      assert(instr.getNumOperands() == 3);
      abuild.buildXOR(instr.def(0)->as<WireRef>(),
                      instr.other(0)->as<HWValue>(),
                      instr.other(1)->as<HWValue>());
      break;
    }
    case *OP_NOT: {
      abuild.buildNOT(instr.def(0)->as<WireRef>(),
                      instr.other(0)->as<HWValue>());
      break;
    }
    case *HW_MUX: {
      assert(instr.getNumOperands() == 4);
      // if (instr.other(1)->is<ConstantRef>() &&
      //     instr.other(1)->as<ConstantRef>().allBitsUndef()) {
      //   abuild.buildNOP(instr.def(0)->as<WireRef>(),
      //                   instr.other(2)->as<HWValue>());
      //   break;
      // }
      // if (instr.other(2)->is<ConstantRef>() &&
      //     instr.other(2)->as<ConstantRef>().allBitsUndef()) {
      //   abuild.buildNOP(instr.def(0)->as<WireRef>(),
      //                   instr.other(1)->as<HWValue>());
      //   break;
      // }

      abuild.buildMUX(
          instr.def(0)->as<WireRef>(), instr.other(0)->as<HWValue>(),
          instr.other(1)->as<HWValue>(), instr.other(2)->as<HWValue>());
      break;
    }

    case *HW_SPLICE:
      abuild.buildSplice(instr);
      break;
    case *HW_REPEAT:
      abuild.buildRepeat(instr);
      break;
    case *HW_CONCAT:
      abuild.buildConcat(instr);
      break;
    case *HW_INSERT:
      abuild.buildInsert(instr);
      break;

    case *OP_TRUNC:
    case *OP_ANYEXT:
    case *OP_SEXT:
    case *OP_ZEXT: {
      abuild.buildExtTrunc(instr.def(0)->as<WireRef>(),
                           instr.other(0)->as<WireRef>(), instr.isOpc(OP_SEXT));
      break;
    }
      // default:
      //   return;
    }
    instr.def(0).replace(FatDynObjRef<>{nullref});
    destroyMap[instr] = 1;
  }

  void runOnProc(ProcessIRef proc) {
    aigRef = ctx.getStore<AIGObj>().create();

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
  void runWrapper(auto &&runFunc) {
    destroyMap.clear();
    destroyMap.resize(ctx.getStore<Instr>().numIDs());
    ctx.getStore<Instr>().createHooks.emplace_back(
        [&](InstrRef ref) { destroyMap.get_ensure(ref) = 0; });

    runFunc();

    ctx.getStore<Instr>().createHooks.pop_back();
    for (auto [obj, destroy] : destroyMap) {
      if (!destroy || !ctx.getStore<Instr>().exists(obj))
        continue;
      build.destroyInstr(ctx.getStore<Instr>().resolve(obj));
    }
  }
  void run() {
    runWrapper([&] {
      for (auto mod : ctx.getCtx<HWDialectContext>().activeModules()) {
        runOnModule(mod.iref());
      }
    });
  }
  void runModule(ModuleIRef mod) {
    runWrapper([&] { runOnModule(mod); });
  }

  static constexpr auto runFuncs =
      std::make_tuple(&AIGConstructPass::runModule, &AIGConstructPass::run);

  auto make(Context &ctx) { return AIGConstructPass(ctx); }
  explicit AIGConstructPass(Context &ctx) : ctx(ctx), build(ctx) {}
};

}; // namespace dyno
