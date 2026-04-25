#pragma once

#include "dyno/Context.h"
#include "dyno/Obj.h"
#include "dyno/Pass.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Module.h"
namespace dyno {

// removes loopback registers, just directly forward reference wires
class ProcToNetlistPass : public Pass<ProcToNetlistPass> {
  Context &ctx;

#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM) FIELD(bool, keepRegs, false)
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
  Config config;

  bool runOnRegister(RegisterIRef reg) {
    // ignore I/Os
    if (!reg.isOpc(HW_REGISTER_DEF))
      return false;
    auto store = StoreIRef{reg.getSingleStore()};
    if (!store || !store.isFullReg())
      return false;
    auto load = LoadIRef{reg.getSingleLoad()};
    if (!load || !load.isFullReg())
      return false;

    auto proc = HWInstrRef{store}.parentProc(ctx);
    if (proc != HWInstrRef{load}.parentProc(ctx))
      return false;

    load.value().replaceAllUsesWith(store.value());

    HWInstrBuilder build{ctx};
    build.destroyInstr(load);
    if (!config.keepRegs) {
      build.destroyInstr(store);
      build.destroyInstr(reg);
    }

    build.changeProcessType(HW_NETLIST_PROCESS_DEF, proc);
    return true;
  }

  void runOnModule(ModuleIRef mod) {
    for (auto reg : mod.regs().earlyincr()) {
      runOnRegister(reg);
    }
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
      mk_tuple(&ProcToNetlistPass::run, &ProcToNetlistPass::runModule);

  explicit ProcToNetlistPass(Context &ctx) : ctx(ctx) {}
  auto make(Context &ctx) { return ProcToNetlistPass{ctx}; }
};
}; // namespace dyno
