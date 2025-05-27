#pragma once
#include "hw/HWAbstraction.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/Register.h"

namespace dyno {

class SeqToCombPass {
  HWContext &ctx;

public:
  explicit SeqToCombPass(HWContext &ctx) : ctx(ctx) {}

  void runOnReg(ModuleIRef module, RegisterIRef reg) {
    HWInstrBuilderStack build{ctx};

    SmallVec<InstrRef, 4> storeDefers;
    for (auto use : reg.oref().uses()) {
      auto instr = use.instr();
      if (instr.isOpc(HW_STORE_DEFER))
        storeDefers.emplace_back(instr);
    }
    if (storeDefers.empty())
      return;

    for (auto &store : storeDefers) {
      auto proc = HWInstrRef{store}.parentProc(ctx);
      build.setInsertPoint(HWInstrRef{store}.iter(ctx));

      auto range = BitRange::full();
      if (store.getNumOperands() > 2)
        range = BitRangeOperand{store.operand(2)};

      build.buildStore(store.operand(1)->as<RegisterRef>(),
                       store.operand(0)->as<HWValue>(), range, true,
                       proc.other(0)->as<TriggerRef>().iref());

      build.destroyInstr(store);
    }
  }

  void runOnModule(ModuleIRef module) {
    // move triggers into *_defer instructions
    for (auto reg : module.regs())
      runOnReg(module, reg);

    for (auto proc : module.procs()) {
      if (proc.isOpc(HW_SEQ_PROCESS_INSTR)) {
        proc.mutateOpcode(HW_COMB_PROCESS_INSTR);
        proc.downsizeOperands(2);
      }
    }
  }

  void run() {
    for (auto mod : Range{ctx.getModules()}.as<ModuleRef>()) {
      runOnModule(mod.iref());
    }
  }
};

}; // namespace dyno
