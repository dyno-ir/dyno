#pragma once
#include "dyno/Context.h"
#include "dyno/Pass.h"
#include "hw/AutoDebugInfo.h"
#include "hw/FlipFlop.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/IDs.h"
#include "hw/Module.h"

namespace dyno {

class LiftLatchesPass : public Pass<LiftLatchesPass> {
  Context &ctx;
  HWInstrBuilder build{ctx};

  void runOnInstance(LatchIRef instr) {
    HWInstrBuilder pbuild{ctx};
    AutoCopyDebugInfoStack autoDbgInfo{ctx};
    autoDbgInfo.pushDebugInfo(instr);

    pbuild.setInsertPoint(instr);

    auto state = build.buildRegister(instr.q().getNumBits());
    if (auto init = instr.initValue()) {
      if (auto asConst = init.dyn_as<ConstantRef>())
        ctx.getCtx<HWDialectContext>().regResetValue.get_ensure(state) =
            asConst;
      else
        report_fatal_error(ctx, instr, "latch init val is not a constant");
    }
    auto stateVal = pbuild.buildLoad(state);
    instr.q().replaceAllUsesWith(stateVal);

    HWValue dVal = instr.d();
    dVal = pbuild.buildMux(instr.en(), dVal, stateVal);
    pbuild.buildStore(state, dVal);

    build.destroyInstr(instr);
  }

  void runOnModule(ModuleIRef mod) {
    build.setInsertPoint(mod.regs_end());
    for (auto proc : mod.procs())
      for (auto instr : Range{proc.block()}.earlyincr())
        if (instr.isOpc(HW_LATCH))
          runOnInstance(instr);
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
      mk_tuple(&LiftLatchesPass::run, &LiftLatchesPass::runModule);

  explicit LiftLatchesPass(Context &ctx) : ctx(ctx) {}
  auto make(Context &ctx) { return LiftLatchesPass{ctx}; }
};
}; // namespace dyno
