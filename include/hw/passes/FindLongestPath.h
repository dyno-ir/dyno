#pragma once

#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/IDs.h"
#include "support/Debug.h"
namespace dyno {

class FindLongestPathPass {
  HWContext &ctx;

  struct Path {
    Optional<uint32_t> length = nullopt;
    Optional<uint32_t> useIdx;
  };
  ObjMapVec<Instr, Path> map;

  InstrRef longestPathInstr = nullref;

  bool circularDepsIgnored = false;

  void runOnProcess(ProcessIRef proc) {
    for (auto instr : proc.block()) {
      uint32_t maxLength = 0;
      Optional<uint32_t> useIdx;
      for (auto use : instr.others()) {
        // todo: registers
        if (auto asWire = use->dyn_as<WireRef>()) {
          auto &entry = map[asWire.getDefI()];
          if (!entry.length) {
            circularDepsIgnored = true;
            continue;
          }
          if (*entry.length > maxLength) {
            useIdx = use - instr.other_begin();
            maxLength = *entry.length;
          }
        }
      }

      auto &entry = map[instr];
      entry.length = maxLength + instr.isOpc(HW_STDCELL_INSTANCE);
      entry.useIdx = useIdx;

      if (!longestPathInstr || *entry.length >= *map[longestPathInstr].length)
        longestPathInstr = instr;
    }
  }

  void runOnModule(ModuleIRef mod) {
    for (auto proc : mod.procs()) {
      runOnProcess(proc);
    }

    auto instr = longestPathInstr;
    if (!instr)
      return;
    SmallVec<InstrRef, 16> path;
    path.reserve(*map[instr].length);
    while (1) {
      path.emplace_back(instr);
      auto &entry = map[instr];
      if (!entry.useIdx)
        break;
      instr = instr.other(*entry.useIdx)
                  ->as<FatDynObjRef<InstrDefUse>>()
                  ->getDef()
                  .instr();
    }
    std::reverse(path.begin(), path.end());

    DEBUG("FindLongestPath", {
      dbgs() << "longest path:\n";
      HWPrinter print{dbgs()};
      for (auto instr : path)
        print.printInstr(instr, ctx);
      dbgs() << "\n\n";
    });
  }

public:
  void run() {
    map.clear();
    map.resize(ctx.getInstrs().numIDs());
    for (auto mod : ctx.activeModules()) {
      runOnModule(mod.iref());
    }
  }

  explicit FindLongestPathPass(HWContext &ctx) : ctx(ctx) {}
};

}; // namespace dyno
