#pragma once
#include "dyno/CFG.h"
#include "dyno/CustomInstr.h"
#include "dyno/HierBlockIterator.h"
#include "dyno/Instr.h"
#include "hw/AutoDebugInfo.h"
#include "hw/HWAbstraction.h"
#include "hw/HWInstr.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/Register.h"

namespace dyno {

class SeqToCombPass {
  HWContext &ctx;
  AutoCopyDebugInfoStack autoDbgInfo;

public:
  explicit SeqToCombPass(HWContext &ctx) : ctx(ctx), autoDbgInfo(ctx) {}

  using TaggedRegRef = CustomInstrRef<RegisterIRef, uint64_t>;

  void runOnProc(ModuleIRef mod, ProcessIRef proc) {
    if (!proc.isOpc(HW_SEQ_PROCESS_DEF))
      return;

    auto trigger = proc.other(0)->as<TriggerRef>().iref();
    ObjMapVec<Instr, bool> handled;
    handled.resize(ctx.getInstrs().numIDs());
    HWInstrBuilder build{ctx};
    std::optional<BlockRef_iterator<true>> regs_end;

    SmallVec<InstrRef, 16> destroyList;
    auto range = HierBlockRange{proc.block()};
    for (auto instr : range) {
      switch (*instr.getDialectOpcode()) {
      case *HW_STORE: {
        auto tok = autoDbgInfo.addWithToken(instr);
        // for all regs that are written to by regular STORE in seq process:
        // add a last value loopback FF (i.e. LOAD at front, STORE_DEFER at
        // end of proc)
        auto store = instr.as<StoreIRef>();
        auto reg = instr.operand(1)->as<RegisterRef>();
        // check if already handled
        if (handled[instr])
          continue;
        handled[instr] = 1;

        if (!regs_end)
          regs_end = mod.regs_end();

        auto [accessAddr, accessLen] = store.getConstAccessRange();

        build.setInsertPoint(*regs_end);
        auto qReg = build.buildRegister(accessLen);

        // at start of proc put qReg value into reg as default (stateful) value.
        build.setInsertPoint(proc.block().begin());
        build.buildStore(reg, build.buildLoad(qReg), false, nullref,
                         accessAddr);

        // at end of proc, store defer reg value into qreg
        build.setInsertPoint(proc.block().end());
        auto finalV = build.buildLoad(reg, accessLen, accessAddr);
        build.buildStore(qReg, finalV, true, trigger);
        break;
      }
      case *HW_STORE_DEFER: {
        auto tok = autoDbgInfo.addWithToken(instr);
        auto store = instr.as<StoreIRef>();
        build.setInsertPoint(HWInstrRef{instr}.iter(ctx));

        build.buildStore(store.reg(), store.value(), true, trigger,
                         store.getBase(), store.terms());
        destroyList.emplace_back(instr);
        break;
      }
      case *OP_ASSERT: {
        auto tok = autoDbgInfo.addWithToken(instr);
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
      if (proc.isOpc(HW_SEQ_PROCESS_DEF)) {
        auto newProc = ctx.getInstrs().create(2, HW_COMB_PROCESS_DEF);
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
    for (auto mod : ctx.activeModules()) {
      runOnModule(mod.iref());
    }
  }
};

}; // namespace dyno
