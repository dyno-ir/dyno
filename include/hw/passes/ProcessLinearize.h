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

namespace dyno {

struct RegisterRegionsFragment {
  SmallVec<uint32_t, 1> writerIDs;
  uint32_t dstAddr;
  uint32_t len;
};

struct RegisterRegions : public RegisterFrags<RegisterRegionsFragment> {
  using Fragment = RegisterRegionsFragment;

  void addRegion(uint32_t writerID, uint32_t dstAddr, uint32_t len) {

    auto it = getInsertIt(dstAddr);

    auto isContained = [](RegisterRegionsFragment *it, uint32_t id) {
      return std::find(it->writerIDs.begin(), it->writerIDs.end(), id) !=
             it->writerIDs.end();
    };

    auto addIfNotContained = [](RegisterRegionsFragment *it, uint32_t id) {
      if (std::find(it->writerIDs.begin(), it->writerIDs.end(), id) ==
          it->writerIDs.end()) {
        it->writerIDs.emplace_back(id);
      }
    };

    // it starts earlier
    if (it->dstAddr < dstAddr) {
      if (isContained(it, writerID)) {

        if (it->dstAddr + it->len >= dstAddr + len)
          return;
        dstAddr = it->dstAddr + it->len;
        len -= it->len;

        ++it;
      } else {
        auto pieceLen = dstAddr - it->dstAddr;
        Fragment frag{it->writerIDs, it->dstAddr, pieceLen};
        it = frags.insert(it, frag) + 1;

        it->len -= pieceLen;
        it->dstAddr += pieceLen;
      }
    }

    while (len != 0) {

      uint32_t end = dstAddr + len;
      uint32_t itEnd = it->dstAddr + it->len;

      // it ends later
      if (itEnd > end) {
        if (isContained(it, writerID))
          break;
        Fragment frag{it->writerIDs, it->dstAddr, len};
        it->len = itEnd - end;
        it->dstAddr += len;
        it = frags.insert(it, frag);
        addIfNotContained(it, writerID);
        break;
      }

      addIfNotContained(it, writerID);

      len -= it->len;
      dstAddr += it->len;
      ++it;
    }
  }

  auto getAccessors(uint32_t addr, uint32_t len) {
    SmallDenseSet<uint32_t, 1> writers;
    uint32_t end = addr + len;

    auto it = getInsertIt(addr);
    while (it != frags.end() && len != 0) {
      uint32_t itEnd = it->dstAddr + it->len;

      if (addr < itEnd && it->dstAddr < end) {
        uint32_t pieceLen = std::min(end, itEnd) - std::max(addr, it->dstAddr);

        writers.findOrInsert(Range{it->writerIDs});

        addr += pieceLen;
        len -= pieceLen;
        it++;
      } else
        break;
    }

    return writers;
  }

  explicit RegisterRegions(uint32_t len) : RegisterFrags({{{}, 0, len}}) {}
};

class ProcessLinearizePass {

  using BitSet = UnsizedBitSet<SmallVec<uint64_t, 2>>;

  struct Custom {
    enum BitField : uint8_t { VISITED = 1, PRE_VISITED = 2 };
    uint8_t bitField = 0;
    SmallDenseSet<ProcessIRef, 1> predsSet;
    BitSet dependingOutputs;
    BitSet dependingInputs;
  };

  ObjMapVec<Process, Custom> map;

  struct Config {
    bool retainIODeps = true;
    bool retainInnerDeps = false;
  };
  Config config;

  // using CustInstrRef = CustomInstrRef<InstrRef, Custom *>;
  // using CustProcIRef = CustomInstrRef<ProcessIRef, Custom *>;
  HWContext &ctx;

  static constexpr ProcessIRef blackBoxWrite(uint16_t id) {
    assert(id < 0xFFFF);
    auto rv = ProcessIRef{ObjID::invalid(), (Instr *)nullptr, uint16_t(1 + id)};
    return rv;
  }
  static constexpr bool isBlackBoxWrite(ProcessIRef proc) {
    return proc.getObjID() == ObjID::invalid() && proc.getCustom() >= 1;
  }

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
            for (auto it = readRegions.getInsertIt(addr);
                 it != readRegions.getInsertIt(addr + len - 1) + 1; it++) {
              map[procI.proc()].dependingInputs.setDyn(
                  inputIdxCnt + (it - readRegions.frags.begin()));
            }
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

  void visit(SmallVecImpl<ProcessIRef> &ordered, ProcessIRef proc) {
    auto &custom = map[proc.proc()];

    if ((custom.bitField & Custom::VISITED))
      return;
    if ((custom.bitField & Custom::PRE_VISITED)) {
      std::cerr << "cyclic:\n";
      dumpInstr(proc);
      abort();
    }

    custom.bitField |= Custom::PRE_VISITED;

    for (auto depend : Range{custom.predsSet}) {
      if (isBlackBoxWrite(depend))
        continue;
      if (depend == proc)
        continue;
      visit(ordered, depend);
    }

    custom.bitField |= Custom::VISITED;
    ordered.emplace_back(proc);
  }

  void linearize(ModuleIRef module) {
    SmallVec<ProcessIRef, 16> ordered;
    for (auto proc : module.procs()) {
      visit(ordered, proc);
    }

    if (config.retainIODeps) {
      // propagate depending outputs.
      for (auto proc : Range{ordered}.reverse()) {
        auto &custom = map[proc.proc()];
        for (auto pred : Range{custom.predsSet}) {
          if (isBlackBoxWrite(pred))
            continue;
          custom.dependingOutputs |= custom.dependingOutputs;
        }
      }
      // propagate depending inputs.
      for (auto proc : Range{ordered}) {
        auto &custom = map[proc.proc()];
        for (auto pred : Range{custom.predsSet}) {
          if (isBlackBoxWrite(pred))
            continue;
          custom.dependingInputs |= custom.dependingInputs;
        }
      }

      DEBUG(
          "ProcessLinearize", std::cerr << "ordered:\n";
          for (auto proc : ordered) { dumpInstr(proc); } dbgs() << "\n\n\n";)

      size_t cnt = ordered.size();
      SmallVec<uint32_t, 16> toMerge;
      while (cnt != 0) {
        toMerge.clear();
        std::optional<BitSet> mergedIns = std::nullopt;
        bool hasOutputsDeps = false;
        bool skippedAny = false;

        for (auto [i, proc] : Range{ordered}.enumerate()) {
          if (!proc)
            continue;

          auto &otherIns = map[proc.proc()].dependingInputs;
          bool otherHasOutputDeps =
              map[proc.proc()].dependingOutputs.count() != 0;

          if (!mergedIns) {
            mergedIns = otherIns;
            hasOutputsDeps = otherHasOutputDeps;
            toMerge.emplace_back(i);
            continue;
          }

          if (otherIns == *mergedIns ||
              (!skippedAny && !config.retainInnerDeps && !hasOutputsDeps &&
               !otherHasOutputDeps)) {
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
