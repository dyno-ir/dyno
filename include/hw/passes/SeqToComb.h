#pragma once
#include "dyno/CFG.h"
#include "dyno/CustomInstr.h"
#include "dyno/HierBlockIterator.h"
#include "dyno/Instr.h"
#include "hw/HWAbstraction.h"
#include "hw/HWInstr.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/Register.h"

namespace dyno {

class SeqToCombPass {
  HWContext &ctx;

public:
  explicit SeqToCombPass(HWContext &ctx) : ctx(ctx) {}

  using TaggedRegRef = CustomInstrRef<RegisterIRef, uint64_t>;

  void runOnProc(ModuleIRef mod, ProcessIRef proc) {
    if (!proc.isOpc(HW_SEQ_PROCESS_INSTR))
      return;

    auto trigger = proc.other(0)->as<TriggerRef>().iref();

    HWInstrBuilder build{ctx};
    std::optional<BlockRef_iterator<true>> regs_end;

    SmallVec<InstrRef, 16> destroyList;
    auto range = HierBlockRange{proc.block()};
    for (auto instr : range) {
      switch (*instr.getDialectOpcode()) {
      case *HW_STORE: {
        // for all regs that are written to by regular STORE in seq process:
        // add a last value loopback FF (i.e. LOAD at front, STORE_DEFER at
        // end of proc)
        auto reg = instr.operand(1)->as<RegisterRef>();
        // check if already handled
        if (TaggedRegRef{reg.iref()}.get())
          continue;
        TaggedRegRef{reg.iref()}.get() = 1;

        if (!regs_end)
          regs_end = mod.regs_end();

        build.setInsertPoint(*regs_end);
        auto qReg = build.buildRegister(reg->numBits);

        // at start of proc put qReg value into reg as default (stateful) value.
        build.setInsertPoint(proc.block().begin());
        build.buildStore(reg, build.buildLoad(qReg));

        // at end of proc, store defer reg value into qreg
        build.setInsertPoint(proc.block().end());
        auto finalV = build.buildLoad(reg);
        build.buildStore(qReg, finalV, BitRange::full(), true, trigger);
        break;
      }
      case *HW_STORE_DEFER: {
        build.setInsertPoint(HWInstrRef{instr}.iter(ctx));

        auto range = BitRange::full();
        if (instr.getNumOperands() > 3) {
          range = BitRangeOperand{instr.operand(instr.getNumOperands() - 2)};
        }

        build.buildStore(instr.operand(1)->as<RegisterRef>(),
                         instr.operand(0)->as<HWValue>(), range, true, trigger);
        destroyList.emplace_back(instr);
        break;
      }
      case *OP_ASSERT: {
        build.setInsertPoint(HWInstrRef{instr}.iter(ctx));
        build.buildAssert(instr.operand(0)->as<HWValue>(), trigger);
        destroyList.emplace_back(instr);
        break;
      }
      default:
        break;
      }
    }

    for (auto instr : Range{destroyList}.reverse())
      build.destroyInstr(instr);
  }

  void runOnModule(ModuleIRef module) {
    for (auto proc : module.procs()) {
      runOnProc(module, proc);
    }

    HWInstrBuilder build{ctx};

    SmallVec<ProcessIRef, 32> destroyList;
    for (auto proc : module.procs()) {
      if (proc.isOpc(HW_SEQ_PROCESS_INSTR)) {
        auto newProc = ctx.getInstrs().create(2, HW_COMB_PROCESS_INSTR);
        InstrBuilder ibuild{newProc};
        ibuild.addRef(proc.operand(0)->fat());
        proc.operand(0).replace(FatDynObjRef<>{nullref});

        ibuild.addRef(proc.operand(1)->fat());
        proc.operand(1).replace(FatDynObjRef<>{nullref});

        build.setInsertPoint(ctx.getCFG()[proc]);
        build.insertInstr(newProc);

        destroyList.emplace_back(proc);
      }
    }

    for (auto proc : destroyList)
      build.destroyInstr(proc);
  }

  void run() {
    for (auto mod : Range{ctx.getModules()}.as<ModuleRef>()) {
      runOnModule(mod.iref());
    }
  }
};

}; // namespace dyno
