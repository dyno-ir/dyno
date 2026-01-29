#pragma once
#include "dyno/Context.h"
#include "dyno/Pass.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWValue.h"
#include "hw/analysis/KnownBits.h"
namespace dyno {
class MuxTreeFlattenPass : public Pass<MuxTreeFlattenPass> {
  Context &ctx;
  KnownBitsAnalysis knownBits;
  ObjMapVec<Instr, bool> visitedMap;

  HWValue convertTreeToOneHot(InstrRef root,
                              std::invocable<InstrRef> auto visitedCallback,
                              bool matchMultiUse = false) {
    SmallVec<std::tuple<HWValue, uint32_t>, 32> worklist{
        {root.def(0)->as<WireRef>(), 1}};

    SmallVec<HWValue, 4> prefixes;
    SmallVec<std::pair<HWValue, HWValue>, 4> cases;
    HWInstrBuilder build{ctx, root};

    while (!worklist.empty()) {
      auto [val, idx] = worklist.back();
      if (val.is<ConstantRef>() ||
          (val.is<WireRef>() &&
           (!val.as<WireRef>().getDefI().isOpc(HW_MUX) ||
            (!matchMultiUse && val.as<WireRef>().getDefI() != root &&
             !val.as<WireRef>().hasSingleUse())))) {
        SmallVec<HWValue, 4> copy;
        copy.reserve(cases.size());
        copy.push_back_range(Range{prefixes});
        for (auto [val, wire] : Range{copy}.zip(prefixes)) {
          val.setCustom(0);
          if (wire.getCustom())
            val = build.buildNot(val);
        }
        cases.emplace_back(build.buildAnd(copy), val);
        worklist.pop_back();
        continue;
      }
      auto asWire = val.as<WireRef>();
      auto outOperand = *asWire.getSingleDef();
      auto instr = outOperand.instr();
      auto operand = *(instr.other_begin() + idx);
      if (operand == instr.other_end()) {
        worklist.pop_back();
        prefixes.pop_back();
        continue;
      }

      // on first touch push prefix and do callback
      if (operand == instr.other(1)) {
        visitedCallback(instr);
        assert(instr.other(0)->as<HWValue>().getNumBits() == 1);
        prefixes.emplace_back(instr.other(0)->as<HWValue>());
      }

      switch (*instr.getDialectOpcode()) {
      case *HW_MUX: {
        std::get<1>(worklist.back()) += 1;
        worklist.emplace_back(operand->as<HWValue>(), 1);
        if (operand != instr.other(1)) {
          prefixes.back().setCustom(1);
        }
        break;
      }
      default:
        dyno_unreachable("invalid instr");
      }
    }
    return build.buildOneHotMux(MutArrayRef{cases});
  }

  void runOnProcess(ProcessIRef proc) {
    for (InstrRef instr :
         Range{proc.block().begin(), proc.block().end()}.reverse()) {
      if (visitedMap[instr])
        continue;

      switch (*instr.getDialectOpcode()) {
      case *HW_MUX:
        visitedMap[instr] = 1;
        auto newVal = convertTreeToOneHot(
            instr, [&](InstrRef ref) { visitedMap[ref] = 1; }, false);
        instr.def(0)->as<WireRef>().replaceAllUsesWith(newVal);
        break;
      }
    }
  }

  void runOnModule(ModuleIRef mod) {
    for (auto proc : mod.procs())
      runOnProcess(proc);
  }

public:
  void run() {
    visitedMap.clear();
    visitedMap.resize(ctx.getStore<Instr>().numIDs());
    ctx.getStore<Instr>().createHooks.emplace_back(
        [&](InstrRef ref) { visitedMap.get_ensure(ref) = 1; });
    for (auto mod : ctx.getCtx<HWDialectContext>().activeModules())
      runOnModule(mod.iref());
  }
  explicit MuxTreeFlattenPass(Context &ctx) : ctx(ctx) {}
  MuxTreeFlattenPass make(Context &ctx) { return MuxTreeFlattenPass{ctx}; }
};
}; // namespace dyno
