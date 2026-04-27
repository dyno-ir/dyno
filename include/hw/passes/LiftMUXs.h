#pragma once

#include "hw/IDs.h"
#include "hw/passes/InstCombine.h"

namespace dyno {

class LiftMuxPass : public Pass<LiftMuxPass> {
  Context &ctx;
  InstCombinePass instCombine;

  void runOnProcess(ProcessIRef proc) {
    auto range = HierBlockRange{proc.block()};
    Vec<InstrRef> worklist(Range{range}.filter(
        [](auto instr) { return instr.isOpc(HW_ONEHOT_MUX); }));
    instCombine.runOnly(HW_ONEHOT_MUX, std::move(worklist));
  }
  void runOnModule(ModuleIRef mod) {
    for (auto proc : mod.procs())
      runOnProcess(proc);
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
      mk_tuple(&LiftMuxPass::run, &LiftMuxPass::runModule);

  explicit LiftMuxPass(Context &ctx) : ctx(ctx), instCombine(ctx) {
    instCombine.config.liftMUX = true;
  }
  auto make(Context &ctx) { return LiftMuxPass{ctx}; }
};
}; // namespace dyno
