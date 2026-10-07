#pragma once
#include "dyno/CFG.h"
#include "dyno/CustomInstr.h"
#include "dyno/HierBlockIterator.h"
#include "dyno/Instr.h"
#include "dyno/Pass.h"
#include "hw/AutoDebugInfo.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/Register.h"
#include "hw/analysis/RegisterValue.h"
#include "support/ErrorRecovery.h"
#include "support/TempBind.h"

namespace dyno {

class SeqToCombPass : public Pass<SeqToCombPass> {
  Context &ctx;
  TempBindVal<AutoCopyDebugInfoStack> autoDbgInfo;

public:
  auto make(Context &ctx) { return SeqToCombPass(ctx); }
  explicit SeqToCombPass(Context &ctx) : ctx(ctx) {}

  using TaggedRegRef = CustomInstrRef<RegisterIRef, uint64_t>;

  auto findAccessedFrags(ProcessIRef storeProc, RegisterIRef reg) {
    GenericPartitions<BoolFragment, 4> frags(*reg.getNumBits(), false);
    SmallVec<InstrRef, 16> users;
    for (auto &use : reg.oref().uses()) {
      auto proc = HWInstrRef{use.instr()}.parentProc(ctx);
      if (storeProc != proc)
        continue;
      auto instr = use.instr();
      if (auto asStore = instr.dyn_as<StoreIRef>()) {
        auto [addr, len] = asStore.getConstAccessRange();
        frags.writeSingle(addr, len, true);
      }
      users.emplace_back(instr);
    }
    frags.defragment();
    return std::make_pair(frags, users);
  }

  void
  handleNonBlockingStores(ProcessIRef storeProc, RegisterRef stateReg,
                          RegisterRef combReg, ArrayRef<InstrRef> procAccesses,
                          GenericPartitions<BoolFragment, 4> &accessFrags) {
    HWInstrBuilder build{ctx, storeProc.block().begin()};
    for (auto frag : Range{accessFrags.frags}.filter([](auto &f) { return f; }))
      build.buildStore(combReg,
                       build.buildLoad(stateReg, frag.len, frag.dstAddr), false,
                       nullref, frag.dstAddr);

    for (auto access : procAccesses) {
      if (access.isOpc(HW_STORE_DEFER))
        report_fatal_error(
            ctx, stateReg.iref(),
            "blocking and non-blocking stores to same register ranges");
      assert(access.isOpc(HW_LOAD, HW_STORE));
      access.operand(1).replace(combReg);
    }

    build.setInsertPoint(storeProc.block().end());
    for (auto frag : Range{accessFrags.frags}.filter([](auto &f) { return f; }))
      build.buildStore(stateReg,
                       build.buildLoad(combReg, frag.len, frag.dstAddr), true,
                       nullref, frag.dstAddr);
  }

  void runOnProc(ModuleIRef mod, ProcessIRef proc) {
    if (!proc.isOpc(HW_SEQ_PROCESS_DEF))
      return;

    auto trigger = proc.other(0)->as<TriggerRef>().iref();
    ObjMapVec<Register, bool> handled;
    handled.resize(ctx.getStore<Instr>().numIDs());
    HWInstrBuilder build{ctx};
    std::optional<BlockRef_iterator<true>> regs_end;

    SmallVec<InstrRef, 16> destroyList;
    auto range = HierBlockRange{proc.block()};
    for (auto instr : range) {
      switch (*instr.getDialectOpcode()) {
      case *HW_STORE: {
        auto tok = autoDbgInfo->addWithToken(instr);
        // for all regs that are written to by regular STORE in seq process:
        // add a last value loopback FF (i.e. LOAD at front, STORE_DEFER at
        // end of proc)
        auto store = instr.as<StoreIRef>();
        auto reg = store.reg();

        // check if already handled
        if (handled[reg])
          continue;
        handled[reg] = 1;

        if (!regs_end)
          regs_end = mod.regs_end();

        build.setInsertPoint(*regs_end);
        auto combReg = build.buildRegister(reg.getNumBits());
        ctx.getCtx<HWDialectContext>().regTypeInfo.copyType(reg, combReg);
        for (auto nm : ctx.getCtx<HWDialectContext>().regNameInfo.getNames(reg))
          ctx.getCtx<HWDialectContext>().regNameInfo.addName(
              combReg, nm + std::string("__s2c_comb"));

        auto [frags, users] = findAccessedFrags(proc, reg.iref());
        handleNonBlockingStores(proc, reg, combReg, users, frags);
        break;
      }
      case *HW_STORE_DEFER: {
        auto store = instr.as<StoreIRef>();
        if (store.hasTrigger()) {
          assert(store.trigger() == trigger &&
                 "store already has different trigger than proc it's in?");
          break;
        }
        auto tok = autoDbgInfo->addWithToken(instr);
        build.setInsertPoint(instr);

        build.buildStore(store.reg(), store.value(), true, trigger,
                         store.getBase(), store.terms());
        destroyList.emplace_back(instr);
        break;
      }
      case *OP_ASSERT: {
        auto tok = autoDbgInfo->addWithToken(instr);
        build.setInsertPoint(instr);
        build.buildAssert(instr.operand(0)->as<HWValue>(), trigger);
        destroyList.emplace_back(instr);
        break;
      }
      case *HW_PRINT: {
        auto tok = autoDbgInfo->addWithToken(instr);
        build.setInsertPoint(instr);
        build.buildPrint(instr.operand(0)->as<StringObjRef>()->data,
                         instr.others().as<HWValue>(), trigger);
        destroyList.emplace_back(instr);
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
        auto newProc = ctx.getStore<Instr>().create(2, HW_COMB_PROCESS_DEF);
        InstrBuilder ibuild{newProc};
        ibuild.addRef(proc.operand(0)->fat());
        proc.operand(0).replace(FatDynObjRef<>{nullref});

        ibuild.addRef(proc.operand(1)->fat());
        proc.operand(1).replace(FatDynObjRef<>{nullref});

        build.setInsertPoint(ctx.getCtx<CoreDialectContext>().cfg[proc]);
        build.insertInstr(newProc);

        destroyList.emplace_back(proc);
      }
    }

    for (auto proc : destroyList)
      build.destroyInstr(proc);
  }

  void run() {
    auto tok = autoDbgInfo.emplace(ctx);
    for (auto mod : ctx.getCtx<HWDialectContext>().activeModules()) {
      runOnModule(mod.iref());
    }
  }
  void runModule(ModuleIRef mod) {
    auto tok = autoDbgInfo.emplace(ctx);
    runOnModule(mod);
  }
  static constexpr auto runFuncs =
      mk_tuple(&SeqToCombPass::runModule, &SeqToCombPass::run);
};

}; // namespace dyno
