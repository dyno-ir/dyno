#pragma once

#include "dyno/Context.h"
#include "dyno/Pass.h"
#include "hw/HWContext.h"
#include "hw/IDs.h"
#include "hw/Module.h"
#include "hw/run/HWInterpreter.h"
#include "support/Debug.h"
#include <sstream>
namespace dyno {

class EvalInitProcsPass : public Pass<EvalInitProcsPass> {
  Context &ctx;

  void runOnModule(ModuleIRef mod) {
    auto initProcsRng = mod.procs().filter(
        [](auto proc) { return proc.isOpc(HW_INIT_PROCESS_DEF); });
    if (initProcsRng.empty())
      return;

    DYNO_DBG(std::print(dbgs(), "running initial proc eval for {}\n",
                        mod.mod()->name));

    std::ostringstream combinedOS;
    HWInterpreter interp{ctx, mod, combinedOS, combinedOS};
    interp.setup();
    interp.trace = 0;
    // todo: timeout
    interp.initialEval();

    auto &regResetValue = ctx.getCtx<HWDialectContext>().regResetValue;

    DYNO_DBG(std::print(dbgs(), "stdout/stderr:```\n{}```\n",
                        std::move(combinedOS).str()));

    for (auto reg : mod.regs()) {
      auto &val = interp.getReg(reg);
      if (val.allBitsUndef())
        continue;
      assert(val.getNumBits() == reg.getNumBits());
      regResetValue.get_ensure(reg.oref()) =
          ctx.getStore<Constant>().findOrInsert(val);
    }
  }

public:
  auto make(Context &ctx) { return EvalInitProcsPass(ctx); }
  explicit EvalInitProcsPass(Context &ctx) : ctx(ctx) {}
  void run() {
    for (auto mod : ctx.getCtx<HWDialectContext>().activeModules()) {
      runOnModule(mod.iref());
    }
  }
  void runModule(ModuleIRef mod) { runOnModule(mod); }

  static constexpr auto runFuncs =
      mk_tuple(&EvalInitProcsPass::runModule, &EvalInitProcsPass::run);
};

}; // namespace dyno
