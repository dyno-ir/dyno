#pragma once
#include "dyno/Context.h"
#include "dyno/Pass.h"
#include "hw/FlipFlop.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/IDs.h"
#include "hw/Module.h"
#include "hw/SensList.h"

namespace dyno {

class LiftFlipFlopsPass : public Pass<LiftFlipFlopsPass> {
  Context &ctx;
  HWInstrBuilder build{ctx};

  void runOnInstance(FlipFlopIRef instr) {
    HWInstrBuilder pbuild{ctx};

    pbuild.setInsertPoint(instr);

    auto state = build.buildRegister(instr.q().getNumBits());
    auto stateVal = pbuild.buildLoad(state);
    instr.q().replaceAllUsesWith(stateVal);

    HWValue dVal = instr.d();
    if (instr.hasClkEn()) {
      if (instr.clkEnPolarity() == 1)
        dVal = pbuild.buildMux(instr.clkEn(), dVal, stateVal);
      else
        dVal = pbuild.buildMux(instr.clkEn(), stateVal, dVal);
    }

    auto clkReg = build.buildRegister(1);
    pbuild.buildStore(clkReg, instr.clk());
    SensList sens;
    sens.signals.emplace_back(clkReg, instr.clkPol() ? SensMode::POSEDGE
                                                     : SensMode::NEGEDGE);
    for (auto rstIdx : IntRange{instr.numRsts()}) {
      auto rstReg = build.buildRegister(1);
      pbuild.buildStore(rstReg, instr.rst(rstIdx));

      if (instr.rstPol(rstIdx) == 1)
        dVal = pbuild.buildMux(instr.rst(rstIdx), instr.rstVal(rstIdx), dVal);
      else
        dVal = pbuild.buildMux(instr.rst(rstIdx), dVal, instr.rstVal(rstIdx));

      sens.signals.emplace_back(
          rstReg, instr.rstPol(rstIdx) ? SensMode::POSEDGE : SensMode::NEGEDGE);
    }

    auto trig = build.buildTrigger(sens);
    build.setInsertPoint(trig);

    pbuild.buildStore(state, dVal, true, trig);
    build.destroyInstr(instr);
  }

  void runOnModule(ModuleIRef mod) {
    build.setInsertPoint(mod.regs_end());
    for (auto proc : mod.procs())
      for (auto instr : Range{proc.block()}.earlyincr())
        if (instr.isOpc(HW_FLIP_FLOP))
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
      mk_tuple(&LiftFlipFlopsPass::run, &LiftFlipFlopsPass::runModule);

  explicit LiftFlipFlopsPass(Context &ctx) : ctx(ctx) {}
  auto make(Context &ctx) { return LiftFlipFlopsPass{ctx}; }
};
}; // namespace dyno
