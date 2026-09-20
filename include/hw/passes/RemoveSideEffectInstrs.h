#pragma once

#include "dyno/Context.h"
#include "dyno/HierBlockIterator.h"
#include "dyno/Pass.h"
#include "hw/HWContext.h"
#include "hw/Module.h"
#include "op/IDs.h"
namespace dyno {

class RemoveSideEffectInstrsPass : public Pass<RemoveSideEffectInstrsPass> {
  Context &ctx;

  static bool isSideEffectInstr(InstrRef instr) {
    return instr.isOpc(HW_PRINT, HW_PRINT_DEFER, OP_ASSERT, HW_ASSERT_DEFER);
  }

public:
  void runOnProcess(ProcessIRef proc) {
    SmallVec<ObjRef<Instr>, 64> deleteInstrs;
    for (auto instr : HierBlockRange{proc.block()}) {
      if (!isSideEffectInstr(instr))
        continue;
      deleteInstrs.emplace_back(instr);
    }
    for (auto instr : Range{deleteInstrs}.resolve(ctx))
      ctx.destroyInstr(instr);
  }

  void runOnModule(ModuleIRef mod) {
    for (auto proc : mod.procs())
      runOnProcess(proc);
  }

  void run() {
    for (auto mod : ctx.getCtx<HWDialectContext>().activeModules())
      runOnModule(mod.iref());
  }

  static constexpr auto runFuncs =
      mk_tuple(&RemoveSideEffectInstrsPass::run,
               &RemoveSideEffectInstrsPass::runOnModule,
               &RemoveSideEffectInstrsPass::runOnProcess);

  explicit RemoveSideEffectInstrsPass(Context &ctx) : ctx(ctx) {}
  auto make(Context &ctx) { return RemoveSideEffectInstrsPass{ctx}; }
};
}; // namespace dyno
