#pragma once

#include "dyno/Context.h"
#include "dyno/Pass.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/IDs.h"
#include "support/Debug.h"
namespace dyno {

class FindLongestPathPass : public Pass<FindLongestPathPass> {
  Context &ctx;

  struct Path {
    Optional<uint32_t> length = nullopt;
    Optional<uint32_t> useIdx;
  };
  ObjMapVec<Instr, Path> map;

  InstrRef longestPathInstr = nullref;
  bool circularDepsIgnored = false;
  double area = 0.0;

  void reset() {
    map.clear();
    map.resize(ctx.getStore<Instr>().numIDs());
    longestPathInstr = nullref;
    circularDepsIgnored = false;
    area = 0;
  }

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
      entry.useIdx = useIdx;
      entry.length = maxLength;

      if (instr.isOpc(HW_STDCELL_INSTANCE)) {
        auto stdcell = instr.other(0)->as<ModuleRef>().iref();
        auto info = stdcell.def(2)->as<StdCellInfoRef>();
        if (info->area)
          area += *info->area;

        if (info->isFlipFlop)
          entry.length = 0;
        else
          *entry.length += 1;
      }

      if (!longestPathInstr || *entry.length > *map[longestPathInstr].length)
        longestPathInstr = instr;
    }
  }

  void runOnModule(ModuleIRef mod) {
    reset();
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

    // DEBUG("FindLongestPath", {
    dbgs() << "longest path:\n";
    HWPrinter print{dbgs()};
    for (auto instr : path)
      print.printInstr(instr, ctx);
    //});
    dbgs() << "area: " << area << "\n";
    dbgs() << "\n\n";
  }

public:
  void run() {
    for (auto mod : ctx.getCtx<HWDialectContext>().activeModules()) {
      runOnModule(mod.iref());
    }
  }
  void runModule(ModuleIRef mod) { runOnModule(mod); }

  static constexpr auto runFuncs = std::make_tuple(
      &FindLongestPathPass::runModule, &FindLongestPathPass::run);

  auto make(Context &ctx) { return FindLongestPathPass(ctx); }
  explicit FindLongestPathPass(Context &ctx) : ctx(ctx) {}
};

}; // namespace dyno
