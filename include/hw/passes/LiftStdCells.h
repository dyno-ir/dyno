#pragma once

#include "dyno/Context.h"
#include "dyno/MutInstr.h"
#include "dyno/Obj.h"
#include "dyno/Pass.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/IDs.h"
#include "hw/Module.h"
#include "support/ErrorRecovery.h"
namespace dyno {

// Convert std cell instances to regular module instances (useful for sim).
class LiftStdCellsPass : public Pass<LiftStdCellsPass> {
  Context &ctx;
  HWInstrBuilder build{ctx};

  void runOnInstance(InstrRef instr) {
    auto cell = instr.other(0)->as<ModuleRef>().iref();
    MutInstr<FatDynObjRef<>> modInst{ctx, HW_INSTANCE,
                                     1 + cell.mod()->ports.size()};
    modInst.emplace_back(cell.mod());

    auto inputs = instr.other_begin() + 1;
    auto outputs = instr.def_begin();

    HWInstrBuilder lbuild{ctx, instr};

    for (auto port : cell.ports()) {
      auto reg = build.buildRegister(port.getNumBits());
      if (port.isOpc(HW_INPUT_REGISTER_DEF)) {
        modInst.emplace_back(reg);
        lbuild.buildStore(reg, inputs->as<HWValue>());
        ++inputs;
      } else if (port.isOpc(HW_OUTPUT_REGISTER_DEF)) {
        modInst.emplace_back(reg);
        auto newV = lbuild.buildLoad(reg);
        outputs->as<WireRef>().replaceAllUsesWith(newV);
        ++outputs;
      } else
        report_fatal_error("invalid std cell port dir");
    }
    auto newInstr = modInst.build();
    build.insertInstr(newInstr);
    build.setInsertPoint(newInstr);
    build.destroyInstr(instr);
  }

  void runOnProcess(ProcessIRef proc) {
    for (auto instr : Range{proc.block()}.earlyincr()) {
      if (instr.isOpc(HW_STDCELL_INSTANCE))
        runOnInstance(instr);
    }
  }

  void runOnModule(ModuleIRef mod) {
    build.setInsertPoint(mod.regs_end());
    for (auto proc : mod.procs())
      runOnProcess(proc);
  }

public:
  void runWrapper(auto &&runFunc) { runFunc(); }

  void runModule(ModuleIRef mod) {
    runWrapper([&] { runOnModule(mod); });
  }
  void runProcess(ProcessIRef proc) {
    runWrapper([&] { runOnProcess(proc); });
  }

  void run() {
    runWrapper([&] {
      for (auto mod : ctx.getCtx<HWDialectContext>().activeModules())
        runOnModule(mod.iref());
    });
  }

  static constexpr auto runFuncs =
      mk_tuple(&LiftStdCellsPass::run, &LiftStdCellsPass::runModule,
               &LiftStdCellsPass::runProcess);

  explicit LiftStdCellsPass(Context &ctx) : ctx(ctx) {}
  auto make(Context &ctx) { return LiftStdCellsPass{ctx}; }
};
}; // namespace dyno
