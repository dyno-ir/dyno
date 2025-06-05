#pragma once
#include "dyno/CFG.h"
#include "dyno/Obj.h"
#include "dyno/ObjMap.h"
#include "hw/DeepCopy.h"
#include "hw/HWAbstraction.h"
#include "hw/HWPrinter.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Process.h"
#include "hw/analysis/RegisterValue.h"
#include "support/Debug.h"
#include "support/DenseMap.h"
#include "support/DenseSet.h"
#include "support/DynBitSet.h"
#include "support/Utility.h"
#include <algorithm>
#include <iterator>

namespace dyno {

class ProcessLinearizePass {

  using BitSet = UnsizedBitSet<SmallVec<uint64_t, 2>>;

  struct Custom {
    enum BitField : uint8_t { VISITED = 1, PRE_VISITED = 2 };

    static constexpr uint32_t LOOPID_NONE = 0;
    static constexpr uint32_t LOOPID_MULTIPLE = ~uint32_t(0);

    uint8_t bitField = 0;
    uint32_t loopID = LOOPID_NONE;
    SmallDenseSet<ProcessIRef, 1> predsSet;
    // todo: currently we only use bool value of this, replace if not required.
    BitSet dependingOutputs;
    BitSet dependingInputs;
  };

  ObjMapVec<Process, Custom> map;

  struct Config {
    bool retainIODeps = true;
    bool retainInnerDeps = false;
  };
  Config config;

  HWContext &ctx;

public:
  explicit ProcessLinearizePass(HWContext &ctx) : ctx(ctx), copier(ctx) {}
  std::vector<ProcessIRef> procs;
  DeepCopier copier;

  void findDeps(ModuleIRef module) {

    uint inputIdxCnt = 0;
    uint outputIdxCnt = 0;

    for (auto reg : module.regs()) {
      bool regIsAnyInput =
          reg.isOpc(HW_INPUT_REGISTER_INSTR, HW_INOUT_REGISTER_INSTR,
                    HW_REF_REGISTER_INSTR);

      bool regIsAnyOutput =
          reg.isOpc(HW_OUTPUT_REGISTER_INSTR, HW_INOUT_REGISTER_INSTR,
                    HW_REF_REGISTER_INSTR);

      RegisterRegions writeRegions{reg.getNumBits()};

      for (auto access : reg.oref().uses()) {
        auto instr = HWInstrRef{access.instr()};
        switch (instr.getDialectOpcode().raw()) {

        case HW_STORE.raw(): {
          auto asStore = instr.as<StoreIRef>();
          auto parentProc = instr.parentProc(ctx);
          auto [addr, len] = asStore.getConstAccessRange();
          writeRegions.addRegion(parentProc.proc().getObjID(), addr, len);
          break;
        }

        case HW_INSTANCE.raw(): {
          auto other = instr.other(0)->as<ModuleRef>();
          bool isInputFromInst = other->ports[access.getNum() - 1].portType.is(
              HW_INOUT_REGISTER_INSTR, HW_OUTPUT_REGISTER_INSTR,
              HW_REF_REGISTER_INSTR);
          bool isOutputToInst = other->ports[access.getNum() - 1].portType.is(
              HW_INOUT_REGISTER_INSTR, HW_INPUT_REGISTER_INSTR,
              HW_REF_REGISTER_INSTR);

          regIsAnyInput |= isInputFromInst;
          regIsAnyOutput |= isOutputToInst;
          break;
        }

        case HW_LOAD.raw():
          break;

        case HW_STORE_DEFER.raw():
          break;

        case HW_TRIGGER_INSTR.raw():
          break;

        default:
          dyno_unreachable("register ref'd by unexpected instr");
        }
      }

      RegisterRegions readRegions{reg.getNumBits()};
      if (regIsAnyInput && config.retainIODeps) {
        for (auto access : reg.oref().uses()) {
          auto instr = HWInstrRef{access.instr()};

          switch (instr.getDialectOpcode().raw()) {
          case HW_LOAD.raw(): {
            auto procI = instr.parentProc(ctx);
            auto [addr, len] = instr.as<LoadIRef>().getConstAccessRange();
            readRegions.addRegion(procI.proc().getObjID(), addr, len);
            break;
          }
          }
        }
      }

      for (auto access : reg.oref().uses()) {
        auto instr = HWInstrRef{access.instr()};

        switch (instr.getDialectOpcode().raw()) {
        case HW_LOAD.raw(): {
          auto asLoad = instr.as<LoadIRef>();
          auto procI = instr.parentProc(ctx);
          auto [addr, len] = asLoad.getConstAccessRange();
          auto writers = writeRegions.getAccessors(addr, len);

          map[procI.proc()].predsSet.findOrInsert(
              Range{writers}
                  .transform([&](size_t, uint32_t val) {
                    if (val == UINT32_MAX)
                      return std::optional<ProcessIRef>{};
                    return std::make_optional(
                        ProcessIRef{ctx.getProcs()
                                        .resolve(ObjRef<Process>{ObjID{val}})
                                        ->defUse.getSingleDef()
                                        ->instr()});
                  })
                  .discard_optional());

          if (regIsAnyInput && config.retainIODeps) {
            readRegions.getAccessors(addr, len);
            auto lowIt = readRegions.getInsertIt(addr);
            uint bitsI = inputIdxCnt + (lowIt - readRegions.frags.begin());
            uint bitsLen =
                (readRegions.getInsertIt(addr + len - 1) + 1) - lowIt;
            map[procI.proc()].dependingInputs.setRangeDyn(bitsI, bitsLen);
          }
          break;
        }
        case HW_STORE.raw(): {
          if (regIsAnyOutput) {
            auto procI = instr.parentProc(ctx);
            map[procI.proc()].dependingOutputs.setDyn(outputIdxCnt);
          }
          break;
        }
        default:
          break;
        }
      }

      if (regIsAnyOutput)
        outputIdxCnt++;
      if (regIsAnyInput)
        inputIdxCnt += readRegions.frags.size();
    }
  }

  void visit2(SmallVecImpl<ProcessIRef> &ordered, ProcessIRef root) {
    using Iterator = decltype(Custom::predsSet)::iterator;
    struct Frame {
      ProcessRef proc;
      Iterator it;
    };

    SmallVec<Frame, 128> stack;
    stack.emplace_back(root.proc(), map[root.proc()].predsSet.end());

    uint32_t loopIDCnt = 1;

    while (!stack.empty()) {
      auto &entry = stack.back();
      auto &custom = map[entry.proc];

      if (entry.proc.getCustom() == 0) {
        entry.proc.setCustom(1);
        if ((custom.bitField & Custom::VISITED)) {
          stack.pop_back();
          continue;
        }
        if ((custom.bitField & Custom::PRE_VISITED)) {
          auto it =
              std::find_if(std::make_reverse_iterator(stack.end() - 1),
                           std::make_reverse_iterator(stack.begin()),
                           [&](auto &val) { return val.proc == entry.proc; });

          assert(it != std::make_reverse_iterator(stack.begin()));

          for (auto it2 = it.base(); it2 != stack.end(); ++it2) {
            // giving up on merging if part of multiple logic loops. we might
            // want to revisit this at some point.
            bool noLoopID = map[it2->proc].loopID == Custom::LOOPID_NONE;
            map[it2->proc].loopID =
                noLoopID ? loopIDCnt : Custom::LOOPID_MULTIPLE;
          }
          loopIDCnt++;

          DEBUG("ProcessLinearize", {
            size_t length = stack.end() - it.base();
            dbgs() << "Found process loop of length " << length << ":\n";
            size_t i = 0;
            for (auto it2 = stack.end() - 1; it2 >= it.base(); --it2, ++i) {
              dbgs() << "Process " << (i + 1) << " of " << length << "\n";
              dumpInstr(it2->proc.iref());
              dbgs() << "\n";
            }
          });

          stack.pop_back();
          continue;
        }
        custom.bitField |= Custom::PRE_VISITED;
        entry.it = custom.predsSet.begin();
      }

      if (entry.it != custom.predsSet.end()) {
        stack.emplace_back(entry.it.key().proc(), Iterator{});
        stack.back().proc.setCustom(0);
        ++entry.it;
        continue;
      }

      custom.bitField |= Custom::VISITED;
      ordered.emplace_back(entry.proc.iref());
      stack.pop_back();
    }
  }

  void linearize(ModuleIRef module) {
    SmallVec<ProcessIRef, 16> ordered;
    for (auto proc : module.procs()) {
      visit2(ordered, proc);
    }

    if (config.retainIODeps) {
      // propagate depending outputs.
      for (auto proc : Range{ordered}.reverse()) {
        auto &custom = map[proc.proc()];
        for (auto pred : Range{custom.predsSet}) {
          custom.dependingOutputs |= map[pred.proc()].dependingOutputs;
        }
      }
      // propagate depending inputs.
      for (auto proc : Range{ordered}) {
        auto &custom = map[proc.proc()];
        for (auto pred : Range{custom.predsSet}) {
          custom.dependingInputs |= map[pred.proc()].dependingInputs;
        }
      }

      // DEBUG(
      //     "ProcessLinearize", std::cerr << "ordered:\n";
      //     for (auto proc : ordered) { dumpInstr(proc); } dbgs() << "\n\n\n";)

      size_t cnt = ordered.size();
      SmallVec<uint32_t, 16> toMerge;
      while (cnt != 0) {
        toMerge.clear();

        bool foundInitial = false;

        uint32_t loopID;
        BitSet mergedIns;
        bool hasOutputsDeps = false;
        bool skippedAny = false;

        for (auto [i, proc] : Range{ordered}.enumerate()) {
          if (!proc)
            continue;

          auto &custom = map[proc.proc()];
          auto &otherIns = custom.dependingInputs;
          bool otherHasOutputDeps = custom.dependingOutputs.count() != 0;

          if (!foundInitial) {
            loopID = custom.loopID;
            mergedIns = otherIns;
            hasOutputsDeps = otherHasOutputDeps;
            toMerge.emplace_back(i);
            foundInitial = true;
            if (loopID == Custom::LOOPID_MULTIPLE)
              break;
            continue;
          }

          bool loopOK =
              custom.loopID == loopID && loopID != Custom::LOOPID_MULTIPLE;

          bool innerMerge = (!config.retainInnerDeps && !skippedAny &&
                             !hasOutputsDeps && !otherHasOutputDeps);

          bool outerMerge = otherIns == mergedIns;

          if ((innerMerge || outerMerge) && loopOK) {
            toMerge.emplace_back(i);
          } else
            skippedAny = true;
        }

        auto proc = ordered[toMerge[0]];
        ordered[toMerge[0]] = nullref;

        for (auto idx : Range{toMerge}.drop_front()) {
          // todo: move
          copier.deepCopyInstrs(ordered[idx].block().begin(),
                                proc.block().end().pred());
          HWInstrBuilder{ctx}.destroyInstr(ordered[idx]);
          ordered[idx] = nullref;
        }

        cnt -= toMerge.size();
      }
    } else {
      if (ordered.empty())
        return;

      auto proc = ordered.front();
      for (auto mergeProc : Range{ordered}.drop_front()) {
        // todo: move
        copier.deepCopyInstrs(mergeProc.block().begin(),
                              proc.block().end().pred());
        HWInstrBuilder{ctx}.destroyInstr(mergeProc);
      }
    }
  }

  void runOnModule(ModuleIRef module) {
    findDeps(module);
    linearize(module);
  }

  void run() {
    map.resize(ctx.getProcs().numIDs());
    for (auto mod : ctx.getModules()) {
      runOnModule(ModuleRef{mod}.iref());
    }
  }
};
}; // namespace dyno
