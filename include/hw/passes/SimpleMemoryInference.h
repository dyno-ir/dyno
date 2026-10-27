#pragma once

#include "dyno/Constant.h"
#include "dyno/Context.h"
#include "dyno/DeepCopy.h"
#include "dyno/FatContext.h"
#include "dyno/HierBlockIterator.h"
#include "dyno/Instr.h"
#include "dyno/MutInstr.h"
#include "dyno/Pass.h"
#include "hw/AutoDebugInfo.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Memory.h"
#include "hw/MemoryPort.h"
#include "hw/Register.h"
#include "hw/SensList.h"
#include "op/OpContext.h"
#include "support/Any.h"
#include "support/DynBitSet.h"
#include "support/ErrorRecovery.h"
#include "support/Format.h"
#include "support/SmallVec.h"
#include <cstdint>
namespace dyno {

class SimpleMemoryInferencePass : public Pass<SimpleMemoryInferencePass> {

  struct InputRegister {
    StoreIRef store;
    LoadIRef load;
  };

  Context &ctx;
  SmallDenseSet<ObjRef<Instr>, 2> coveredUses;

  struct WritePort {
    InstrRef instr; // insert/concat
    HWValue enables;
    uint32_t dly;
    TriggerIRef trigger;
  };

  struct ReadPort {
    InstrRef instr; // splice/trunc
    SmallVec<unsigned, 4> fwdWrites;
    uint32_t dly;
  };

  struct Memory {
    RegisterIRef reg;
    SmallVec<ReadPort, 2> reads;
    SmallVec<WritePort, 4> writes;
  };

  Result<Memory, Format> walkTree(LoadIRef load) {

    auto stores = load.reg().iref().storeOrStoreDefers();
    StoreIRef store = stores.empty() ? nullref : stores.front();
    if (!stores.empty() && std::next(stores.begin()) != stores.end())
      return Format{"multiple stores to memory:\n{}",
                    LazyFormat{[this, load](std::ostream &str) {
                      HWCtxPrinter print{ctx, str};
                      for (auto st : LoadIRef{load}.reg().iref().stores()) {
                        str << "  ";
                        print.printInstr(st, true, false);
                      }
                    }}};
    if (store && !store.isFullReg())
      return Format{"expected full reg store"};

    Memory memory;

    struct Frame {
      ObjRef<Wire> ref;
      uint32_t idx = 0;
      HWValue acc = ConstantRef::fromBool(false);
    };
    SmallVec<Frame, 32> stack{{load.value()}};

    HWValue rv;
    bool rvValid = false;

    // every MUX must be visited exactly twice (via both inputs)
    SmallDenseSet<ObjRef<Instr>> coveredMUXs;
    auto flipMux = [&coveredMUXs](InstrRef instr) {
      if (auto [found, it] = coveredMUXs.findOrInsert(instr); found)
        coveredMUXs.erase(it);
    };

    int unaccountedInserts = 0;
    bool sawStore = false;

    SmallDenseMap<ObjRef<Instr>, HWValue> muxMemo;
    BlockRef scratchBlock = ctx.getCFG().createBlock();

    Defer destroyScratchBlock([&]() {
      for (auto instr : scratchBlock.unordered().reverse())
        ctx.destroyInstr(instr);
      ctx.destroy(scratchBlock);
    });

    HWInstrBuilder scratchBuild{ctx, scratchBlock.end()};

    while (!stack.empty()) {
      auto wire = ctx.resolve(stack.back().ref);
      if (stack.back().idx == wire.getNumUses()) {

        // combine final retval with acc
        if (rvValid) {
          stack.back().acc = scratchBuild.buildOr(stack.back().acc, rv);
        }
        rvValid = true;
        rv = stack.pop_back_val().acc;

        if (wire.getDefI().isOpc(HW_INSERT)) {
          unaccountedInserts--;
          memory.writes.emplace_back(
              WritePort{wire.getDefI(), rv, store && store.trigger(),
                        store ? store.trigger() : nullref});
        } else if (wire.getDefI().isOpc(HW_MUX)) {
          muxMemo.findOrInsert(wire.getDefI(), rv);
          auto use = ctx.resolve(stack.back().ref).uses()[stack.back().idx - 1];
          assert(use.instr() == wire.getDefI());
          bool polarity = use.getNum() == 2;
          auto sel = wire.getDefI().other(0)->as<HWValue>();
          rv = scratchBuild.buildAnd(rv, polarity ? sel
                                                  : scratchBuild.buildNot(sel));
        }

        continue;
      } else if (stack.back().idx != 0 && rvValid) {
        // or RV into expression
        stack.back().acc = scratchBuild.buildOr(stack.back().acc, rv);
        rvValid = false;
      }

      auto use = wire.uses()[stack.back().idx++];
      auto instr = use.instr();

      switch (*instr.getDialectOpcode()) {

      case *HW_STORE_DEFER:
      case *HW_STORE: {
        auto asStore = instr.as<StoreIRef>();
        if (asStore != store)
          return Format{"saw store to wrong register"};

        rv = ConstantRef::fromBool(true);
        rvValid = true;
        sawStore = true;
        break;
      }

      case *HW_SPLICE: {
        auto asSplice = instr.as<SpliceIRef>();
        if (use != asSplice.in())
          return Format{"expected splice in"};
        if (stack.size() != 1)
          return Format{"not implemented: store forwarding load (todo)"};
        memory.reads.emplace_back(
            ReadPort{instr,
                     {}, //{Range{memory.writes}.transform([](size_t
                         // i, auto) { return i; })},
                     0});
        break;
      }
      case *HW_ASSUME: {
        if (use != instr.other(0))
          return Format{"expected assume in"};
        stack.emplace_back(instr.def()->as<WireRef>());
        break;
      }
      // case *OP_TRUNC: {
      //   if (stack.size() != 1)
      //     return Format{"not implemented: store forwarding load (todo)"};
      //   memory.reads.emplace_back(ReadPort2{
      //       instr,
      //       {Range{memory.writes}.transform([](size_t i, auto) { return i;
      //       })}, 0});
      //   break;
      // }
      case *HW_INSERT: {
        unaccountedInserts++;
        auto asInsert = instr.as<InsertIRef>();
        if (use != asInsert.in())
          return Format{"expected insert in"};

        auto nextVal = asInsert.out().as<WireRef>();
        stack.emplace_back(nextVal);

        // actual port creation happens later during backtrace
        // (once we know MUX path to enable this insert)
        break;
      }
      case *HW_MUX: {
        flipMux(instr);
        // MUXs have two inputs, so we encounter them twice (exactly twice in
        // fact, otherwise we're muxing in some unrelated value -> fail). Memo
        // to avoid evaluating twice.
        if (auto it = muxMemo.find(instr); it != muxMemo.end()) {
          rv = it.val();
          bool polarity = use.getNum() == 2;
          auto sel = instr.other(0)->as<HWValue>();
          rv = scratchBuild.buildAnd(rv, polarity ? sel
                                                  : scratchBuild.buildNot(sel));
          rvValid = true;
          break;
        }
        stack.emplace_back(instr.def()->as<WireRef>());
        break;
      }
      default: {
        return Format{"unexpected instr: {}",
                      LazyFormat{[&, instr](std::ostream &str) {
                        HWCtxPrinter print{ctx, str};
                        print.printInstr(instr, false, false);
                      }}};
      }
      }
    }

    if (coveredMUXs.size() != 0)
      return Format{"Invalid MUXs. Can only MUX in memory value (optionally "
                    "routed through inserts)\n{}",
                    LazyFormat{[&, coveredMUXs](std::ostream &str) {
                      HWCtxPrinter print{ctx, str};
                      for (auto [back, mux] :
                           Range{coveredMUXs}.resolve(ctx).mark_back()) {
                        str << "  ";
                        print.printInstr(mux, !back);
                      }
                    }}};
    if (unaccountedInserts != 0)
      return Format{"unaccounted for inserts"};
    if (store && !sawStore)
      return Format{"DAG walk did not find store"};
    // move exprs out of scratch block s.t. they're not deleted
    DeepCopier copier{ctx};
    copier.moveInstrs(scratchBlock.begin(), load.iter(ctx));
    return memory;
  }

  // maybe actually make this an analysis
  std::optional<InputRegister> mergeInputRegister(HWValue value) {
    auto wire = value.dyn_as<WireRef>();
    if (!wire)
      return std::nullopt;
    auto load = wire.getDefI().dyn_as<LoadIRef>();
    if (!load || !load.isConstantOffs())
      return std::nullopt;
    auto store = load.reg().iref().getSingleStore();
    if (!store || !store.isOpc(HW_STORE_DEFER) ||
        !store.as<StoreIRef>().isConstantOffs())
      return std::nullopt;

    return InputRegister{store, load};
  }

  void buildMemory(ModuleIRef mod, RegisterIRef reg, Memory &&memory) {
    HWInstrBuilder build{ctx, mod.regs_end()};

    build.destroyInstr(reg.getSingleLoad());
    build.destroyInstr(reg.getSingleStore());

    for (auto &wr : memory.writes) {
      build.setInsertPoint(wr.instr);
      AutoCopyDebugInfoStack autoDbgInfo{ctx};
      auto tok = autoDbgInfo.addWithToken(wr.instr);

      auto asInsert = wr.instr.as<InsertIRef>();
      HWValue en = wr.enables;
      auto ref = build.buildMemStore(
          reg.oref(), asInsert.val()->as<HWValue>(), en, wr.dly, wr.trigger,
          build.buildGEP(asInsert.getBase(), asInsert.terms()));
      wr.instr = ref;
    }

    for (auto &rd : memory.reads) {
      build.setInsertPoint(rd.instr);
      if (auto asSplice = rd.instr.as<SpliceIRef>()) {
        AutoCopyDebugInfoStack autoDbgInfo{ctx};
        auto tok = autoDbgInfo.addWithToken(rd.instr);

        auto val = build.buildMemLoad(
            reg.oref(), asSplice.getLen(), nullref, 0, nullref,
            build.buildGEP(asSplice.getBase(), asSplice.terms()),
            Range{rd.fwdWrites}.transform([&](size_t, unsigned i) {
              return MemoryWriteForward{
                  memory.writes[i].instr.as<MemStoreIRef>().port(), {0, 0}};
            }));
        asSplice.def()->as<WireRef>().replaceAllUsesWith(val);
      } else
        assert(0);
    }
  }

  void runOnRegister(RegisterIRef reg) {
    if (*reg.getNumBits() < 16)
      return;

    auto ld = reg.getSingleLoad();
    if (!ld)
      return;

    auto res = walkTree(ld);

    DYNO_DBG({
      dbgs() << "memory ";
      dumpInstr(reg, ctx);

      if (!res) {
        dbgs() << "failed mapping: " << res.error() << "\n\n";
      } else {

        dbgs() << "write ports:\n";
        for (auto &port : res->writes) {
          dumpInstr(port.instr, ctx);
        }

        dbgs() << "read ports:\n";
        for (auto &port : res->reads) {
          dumpInstr(port.instr, ctx);
        }

        dbgs() << "\n";
      }
    })

    if (res)
      buildMemory(HWInstrRef{reg}.parentMod(ctx), reg, std::move(*res));
  }

  void runOnModule(ModuleIRef mod) {
    SmallVec<RegisterIRef, 16> regs(mod.regs());
    for (auto reg : regs) {
      runOnRegister(reg);
    }
  }

public:
  auto make(Context &ctx) { return SimpleMemoryInferencePass(ctx); }
  explicit SimpleMemoryInferencePass(Context &ctx) : ctx(ctx) {}
  void run() {
    for (auto mod : ctx.getCtx<HWDialectContext>().activeModules()) {
      runOnModule(mod.iref());
    }
  }

  void runModule(ModuleIRef mod) { runOnModule(mod); }
  void runRegister(RegisterIRef reg) { runOnRegister(reg); }

  static constexpr auto runFuncs = mk_tuple(
      &SimpleMemoryInferencePass::runRegister,
      &SimpleMemoryInferencePass::runModule, &SimpleMemoryInferencePass::run);
};

}; // namespace dyno
