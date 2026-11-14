#pragma once

#include "dyno/Context.h"
#include "dyno/Instr.h"
#include "dyno/MutInstr.h"
#include "dyno/Pass.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/Module.h"
#include "hw/StdCellInfo.h"

namespace dyno {

class ModuleToStdCellPass : public Pass<ModuleToStdCellPass> {
  Context &ctx;

  void runOnModule(ModuleIRef mod) {
    if (!mod.isOpc(HW_MODULE_DEF))
      return;

    auto moduleRef = mod.def(0)->as<ModuleRef>();
    auto blockRef = mod.def(1)->as<BlockRef>();

    mod.def(0).replace(FatDynObjRef<>{nullref});
    mod.def(1).replace(FatDynObjRef<>{nullref});

    auto info = ctx.getStore<StdCellInfo>().create();

    MutInstr<FatDynObjRef<>> newInstr{ctx, HW_STDCELL_DEF, 3, 0};
    newInstr.def(0) = moduleRef;
    newInstr.def(1) = blockRef;
    newInstr.def(2) = info;
    auto newInstrRef = newInstr.build();

    auto &cfg = ctx.getCFG();
    if (cfg.contains(mod))
      cfg[mod].replace(newInstrRef);

    HWInstrBuilder{ctx}.destroyInstr(mod);
  }

public:
  void runWrapper(auto &&runFunc) { runFunc(); }

  void runModule(ModuleIRef mod) {
    runWrapper([&] { runOnModule(mod); });
  }

  void run() {
    runWrapper([&] {
      for (auto mod : ctx.getCtx<HWDialectContext>().activeModules())
        runOnModule(mod.iref());
    });
  }

  static constexpr auto runFuncs =
      mk_tuple(&ModuleToStdCellPass::run, &ModuleToStdCellPass::runModule);

  explicit ModuleToStdCellPass(Context &ctx) : ctx(ctx) {}
  auto make(Context &ctx) { return ModuleToStdCellPass{ctx}; }
};
}; // namespace dyno
