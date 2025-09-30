#pragma once

#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/analysis/RegisterValue.h"
#include "support/Utility.h"
namespace dyno {

// fuse loads in processes. should only be run after SSA construct
class LoadCoalescePass {
  HWContext &ctx;

  struct LoadCoalesceFragment {
    uint32_t dstAddr;
    uint32_t len;
    bool active = false;

    bool intersects(LoadCoalesceFragment &other) { return false; }
    bool fuses(LoadCoalesceFragment &other) { return other.active; }
    bool overwrites(LoadCoalesceFragment &other) { return !other.active; }

    LoadCoalesceFragment intersect(LoadCoalesceFragment &other) {
      dyno_unreachable("bad");
    }

    bool abstractEquals(LoadCoalesceFragment &other) {
      return active == other.active;
    }
  };

  auto findLoadsAndRegions(RegisterIRef reg, ProcessIRef proc) {
    GenericPartitions<LoadCoalesceFragment> regions(*reg.getNumBits());
    SmallVec<LoadIRef, 16> loads;
    for (auto use : reg.oref().uses()) {
      if (use.instr().isOpc(HW_LOAD) &&
          HWInstrRef{use.instr()}.parentProc(ctx) == proc) {
        auto load = use.instr().as<LoadIRef>();
        auto [addr, len] = load.getConstAccessRange();
        regions.writeSingle(addr, len, true);
        loads.emplace_back(load);
      }
    }
    return std::make_pair(loads, regions);
  }

  void runOnProcess(ModuleIRef mod, ProcessIRef proc) {
    for (auto reg : mod.regs()) {
      auto [loads, regions] = findLoadsAndRegions(reg, proc);

      bool lastActive = false;
      unsigned activeCnt = 0;
      for (auto frag : regions.frags) {
        if (frag.active && !lastActive)
          activeCnt++;
        lastActive = frag.active;
      }

      assert(activeCnt <= loads.size());
      if (activeCnt == loads.size())
        continue;

      HWInstrBuilder build{ctx, proc.block().begin()};
      for (auto frag : Range{regions.frags}.filter(
               [](auto &frag) { return frag.active; })) {
        auto ldVal = build.buildLoad(reg.oref(), frag.len, frag.dstAddr);
        for (auto oldLoad : loads) {
          auto [oldAddr, oldLen] = oldLoad.getConstAccessRange();
          if (std::max(oldAddr, frag.dstAddr) <
              std::min(oldAddr + oldLen, frag.dstAddr + frag.len)) {
            assert(oldAddr >= frag.dstAddr &&
                   oldAddr + oldLen <= frag.dstAddr + frag.len);
            auto value = build.buildSplice(ldVal, oldLoad.getLen(),
                                           oldLoad.getBase() - frag.dstAddr,
                                           oldLoad.terms());
            assert(value.getNumBits() == oldLoad.getLen());
            oldLoad.value().replaceAllUsesWith(value);
          }
        }
      }
    }
  }

  void runOnModule(ModuleIRef mod) {
    for (auto proc : mod.procs()) {
      runOnProcess(mod, proc);
    }
  }

public:
  void run() {
    for (auto mod : ctx.activeModules()) {
      runOnModule(mod.iref());
    }
  }
  explicit LoadCoalescePass(HWContext &ctx) : ctx(ctx) {}
};

}; // namespace dyno
