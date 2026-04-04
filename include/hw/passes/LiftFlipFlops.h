#pragma once
#include "dyno/CFG.h"
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

    auto proc = build.buildProcess(HW_COMB_PROCESS_DEF);
    build.setInsertPoint(proc);
    pbuild.setInsertPoint(proc.block().end());

    auto state = build.buildRegister(instr.q().getNumBits());
    auto stateVal = pbuild.buildLoad(state);
    pbuild.buildStore(instr.q(), stateVal);

    HWValue dVal = pbuild.buildLoad(instr.d());
    if (instr.hasClkEn()) {
      if (instr.clkEnPolarity() == 1)
        dVal = pbuild.buildMux(pbuild.buildLoad(instr.clkEn()), dVal, stateVal);
      else
        dVal = pbuild.buildMux(pbuild.buildLoad(instr.clkEn()), stateVal, dVal);
    }

    SensList sens;
    sens.signals.emplace_back(instr.clk(), instr.clkPolarity()
                                               ? SensMode::POSEDGE
                                               : SensMode::NEGEDGE);
    for (auto rstIdx : IntRange{instr.numRsts()}) {
      if (instr.rstPolarity(rstIdx) == 1)
        dVal = pbuild.buildMux(pbuild.buildLoad(instr.rst(rstIdx)),
                               instr.rstVal(rstIdx), dVal);
      else
        dVal = pbuild.buildMux(pbuild.buildLoad(instr.rst(rstIdx)), dVal,
                               instr.rstVal(rstIdx));

      sens.signals.emplace_back(instr.rst(rstIdx), instr.rstPolarity(rstIdx)
                                                       ? SensMode::POSEDGE
                                                       : SensMode::NEGEDGE);
    }

    auto trig = build.buildTrigger(sens);
    build.setInsertPoint(trig);

    pbuild.buildStore(state, dVal, true, trig);
    build.destroyInstr(instr);
  }

  void runOnModule(ModuleIRef mod) {
    build.setInsertPoint(mod.regs_end());
    for (auto instr : Range{mod.block()}.earlyincr())
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
