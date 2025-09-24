#pragma once

#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
namespace dyno {

class RemoveInitProcsPass {
  HWContext &ctx;

  void runOnModule(ModuleIRef mod) {
    SmallVec<InstrRef, 32> destroyList;
    for (auto proc : mod.procs()) {
      if (proc.isOpc(HW_INIT_PROCESS_DEF, HW_FINAL_PROCESS_DEF))
        destroyList.emplace_back(proc);
    }

    HWInstrBuilder build{ctx};
    for (auto instr : destroyList)
      build.destroyInstr(instr);
  }

public:
  explicit RemoveInitProcsPass(HWContext &ctx) : ctx(ctx) {}
  void run() {
    for (auto mod : ctx.activeModules()) {
      runOnModule(mod.iref());
    }
  }
};

}; // namespace dyno