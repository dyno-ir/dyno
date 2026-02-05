#pragma once

#include "dyno/Context.h"
#include "dyno/Pass.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/Module.h"
namespace dyno {

class RemoveInitProcsPass : public Pass<RemoveInitProcsPass> {
  Context &ctx;

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
  auto make(Context &ctx) { return RemoveInitProcsPass(ctx); }
  explicit RemoveInitProcsPass(Context &ctx) : ctx(ctx) {}
  void run() {
    for (auto mod : ctx.getCtx<HWDialectContext>().activeModules()) {
      runOnModule(mod.iref());
    }
  }
  void runModule(ModuleIRef mod) { runOnModule(mod); }

  static constexpr auto runFuncs = std::make_tuple(
      &RemoveInitProcsPass::runModule, &RemoveInitProcsPass::run);
};

}; // namespace dyno
