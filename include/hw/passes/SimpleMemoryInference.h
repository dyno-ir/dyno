#pragma once

#include "dyno/Constant.h"
#include "dyno/Context.h"
#include "dyno/Instr.h"
#include "dyno/Pass.h"
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
#include "support/Any.h"
#include "support/BoolExpr.h"
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
  struct ReadPort {
    InstrRef splice;
    std::optional<InputRegister> addrReg;
    std::optional<InputRegister> dataReg;
  };
  struct WritePort {
    InsertIRef insert;
    HWValue enable;
    bool enablePolarity;
  };

  Context &ctx;
  SmallDenseSet<ObjRef<Instr>, 2> coveredUses;

  bool findReadPorts(SmallVecImpl<ReadPort> &out, RegisterIRef reg) {
    for (auto use : reg.oref().uses()) {
      auto instr = use.instr();
      if (instr.isOpc(HW_STORE_DEFER))
        continue;

      // previously checked in findWritePorts
      assert(!instr.isOpc(HW_STORE, HW_TRIGGER_DEF, HW_INSTANCE));

      if (!walkReadTree(out, instr.as<LoadIRef>()))
        return false;
    }
    return true;
  }

  bool findWritePorts(SmallVecImpl<WritePort> &out, RegisterIRef reg) {
    for (auto use : reg.oref().uses()) {
      auto instr = use.instr();
      if (instr.isOpc(HW_LOAD))
        continue;

      // todo: support non-deferred (ie async) writes
      if (instr.isOpc(HW_STORE))
        return false;

      // don't infer memories for these
      if (instr.isOpc(HW_TRIGGER_DEF, HW_INSTANCE))
        return false;

      assert(instr.isOpc(HW_STORE_DEFER));

      if (!walkWriteTree2(out, instr.as<StoreIRef>()))
        return false;
    }

    return true;
  }

  bool walkReadTree(SmallVecImpl<ReadPort> &out, LoadIRef load) {
    for (auto use : load.value().uses()) {
      auto instr = use.instr();
      if (coveredUses.contains(instr))
        continue;
      if (!instr.isOpc(HW_SPLICE, OP_TRUNC))
        return false;

      out.emplace_back(instr);
    }

    return true;
  }

  // Walk MUX/INSERT network to find all write ports and their associated
  // enables.
  // bool walkWriteTree(SmallVecImpl<WritePort> &out, StoreIRef store) {
  //   struct Frame {
  //     WireRef wire;
  //     unsigned idx;
  //   };
  //   if (!store.value().is<WireRef>())
  //     return false;
  //   SmallVec<Frame, 4> stack{Frame{store.value().as<WireRef>(), 0}};
  //   WritePort prefix;

  //   while (!stack.empty()) {
  //     auto &frame = stack.back();
  //     auto instr = frame.wire.getDefI();
  //     coveredUses.findOrInsert(instr);

  //     switch (*instr.getDialectOpcode()) {
  //     case *HW_MUX: {
  //       auto sel = instr.other(0)->as<WireRef>();
  //       if (frame.idx == 0) {
  //         prefix.enablesValues.emplace_back(sel.as<WireRef>());
  //         prefix.enablePolarities.push_back(1);
  //       } else if (frame.idx == 1) {
  //         prefix.enablePolarities.back() = 0;
  //       } else if (frame.idx == 2) {
  //         prefix.enablesValues.pop_back();
  //         prefix.enablePolarities.pop_back();
  //         stack.pop_back();
  //         break;
  //       }
  //       auto val = instr.other(frame.idx + 1)->dyn_as<WireRef>();
  //       if (!val)
  //         return false;
  //       frame.idx++;
  //       stack.emplace_back(val, 0);
  //       break;
  //     }
  //     case *HW_INSERT: {
  //       auto asInsert = instr.as<InsertIRef>();
  //       auto &copy = out.emplace_back(prefix);
  //       copy.insert = asInsert;

  //       auto pad = asInsert.in()->dyn_as<WireRef>();
  //       if (!pad)
  //         return false;

  //       stack.pop_back();
  //       stack.emplace_back(pad, 0);
  //       break;
  //     }

  //     case *HW_LOAD: {
  //       auto asLoad = instr.as<LoadIRef>();
  //       // todo: support partial loads.
  //       if (!asLoad.isFullReg())
  //         return false;
  //       if (asLoad.reg() != store.reg())
  //         return false;

  //       stack.pop_back();
  //       break;
  //     }

  //     default:
  //       return false;
  //     }
  //   }

  //   return true;
  // }

  bool walkWriteTree2(SmallVecImpl<WritePort> &out, StoreIRef store) {
    HWValue cur = store.value();

    while (true) {
      auto wire = cur.dyn_as<WireRef>();
      if (!wire)
        return false;
      auto instr = wire.getDefI();

      HWValue enable = ConstantRef::fromBool(false);
      bool enablePolarity = false;

      if (instr.isOpc(HW_MUX)) {
        enable = instr.other(0)->as<WireRef>();
        auto trueV = instr.other(1)->dyn_as<WireRef>();
        auto falseV = instr.other(2)->dyn_as<WireRef>();
        if (!trueV || !falseV)
          return false;

        coveredUses.insert(instr);

        if (auto ins = trueV.getDefI().dyn_as<InsertIRef>();
            ins && ins.in()->as<HWValue>() == falseV) {
          cur = trueV;
          wire = trueV;
          instr = wire.getDefI();
          enablePolarity = true;
        } else if (auto ins = falseV.getDefI().dyn_as<InsertIRef>();
                   ins && ins.in()->as<HWValue>() == trueV) {
          cur = falseV;
          wire = falseV;
          instr = wire.getDefI();
          enablePolarity = false;
        }
      } else if (instr.isOpc(HW_LOAD)) {
        auto asLoad = instr.as<LoadIRef>();
        if (asLoad.reg() != store.reg())
          return false;
        if (!asLoad.isFullReg())
          return false;
        return true;
      } else
        return false;

      if (instr.isOpc(HW_INSERT)) {
        auto insert = instr.as<InsertIRef>();
        out.emplace_back(insert, enable, enablePolarity);
        cur = insert.in()->as<HWValue>();
        coveredUses.insert(insert);
      } else
        return false;
    }
  }

  //   HWValue cur = store.value();
  // Memory memory;

  // while (true) {
  //   auto wire = cur.dyn_as<WireRef>();
  //   if (!wire)
  //     return std::unexpected("non-wire value");
  //   auto instr = wire.getDefI();

  //   HWValue enable = ConstantRef::fromBool(false);
  //   bool enablePolarity = false;

  //   if (instr.isOpc(HW_MUX)) {
  //     enable = instr.other(0)->as<WireRef>();
  //     auto trueV = instr.other(1)->dyn_as<WireRef>();
  //     auto falseV = instr.other(2)->dyn_as<WireRef>();
  //     if (!trueV || !falseV)
  //       return std::unexpected("mux on non-wire value");

  //     coveredUses.insert(instr);

  //     if (auto ins = trueV.getDefI().dyn_as<InsertIRef>();
  //         ins && ins.in()->as<HWValue>() == falseV) {
  //       cur = trueV;
  //       wire = trueV;
  //       instr = wire.getDefI();
  //       enablePolarity = true;
  //     } else if (auto ins = falseV.getDefI().dyn_as<InsertIRef>();
  //                ins && ins.in()->as<HWValue>() == trueV) {
  //       cur = falseV;
  //       wire = falseV;
  //       instr = wire.getDefI();
  //       enablePolarity = false;
  //     }
  //   } else if (instr.isOpc(HW_LOAD)) {
  //     auto asLoad = instr.as<LoadIRef>();
  //     if (asLoad.reg() != store.reg())
  //       return std::unexpected("load reg different");
  //     if (!asLoad.isFullReg())
  //       return std::unexpected("load not full reg");
  //     return memory;
  //   } else
  //     return std::unexpected("expected mux or load instr");

  //   if (instr.isOpc(HW_INSERT)) {
  //     auto insert = instr.as<InsertIRef>();
  //     memory.writes.emplace_back(
  //         insert,
  //         SmallVec<std::pair<HWValue, bool>, 2>{{enable, enablePolarity}});
  //     cur = insert.in()->as<HWValue>();
  //     coveredUses.insert(insert);
  //   } else
  //     return std::unexpected("expected insert");
  // }
  using BoolExpr =
      TypedSmallBoolExprDNF<HWValue>;

  struct WritePort2 {
    InstrRef instr; // insert/concat
    BoolExpr enables;
    uint32_t dly;
    TriggerIRef trigger;
  };

  struct ReadPort2 {
    InstrRef instr; // splice/trunc
    SmallVec<unsigned, 4> fwdWrites;
    uint32_t dly;
  };

  struct Memory {
    RegisterIRef reg;
    SmallVec<ReadPort2, 2> reads;
    SmallVec<WritePort2, 4> writes;
  };

  Result<Memory, Format> walkTree3(LoadIRef load) {

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
      BoolExpr acc = {};
    };
    SmallVec<Frame, 32> stack{{load.value()}};

    BoolExpr rv;
    bool rvValid = false;

    // every MUX must be visited exactly twice (via both inputs)
    SmallDenseSet<ObjRef<Instr>> coveredMUXs;
    auto flipMux = [&coveredMUXs](InstrRef instr) {
      if (auto [found, it] = coveredMUXs.findOrInsert(instr); found)
        coveredMUXs.erase(it);
    };

    int unaccountedInserts = 0;
    bool sawStore = false;

    SmallDenseMap<ObjRef<Instr>, BoolExpr> muxMemo;

    while (!stack.empty()) {
      auto wire = ctx.resolve(stack.back().ref);
      if (stack.back().idx == wire.getNumUses()) {

        // combine final retval with acc
        if (rvValid) {
          stack.back().acc.addTerms(rv);
        }
        rvValid = true;
        rv = std::move(stack.pop_back_val().acc);

        if (wire.getDefI().isOpc(HW_INSERT)) {
          unaccountedInserts--;
          rv.simplify();
          memory.writes.emplace_back(
              WritePort2{wire.getDefI(), rv, store && store.trigger(),
                         store ? store.trigger() : nullref});
        } else if (wire.getDefI().isOpc(HW_MUX)) {
          muxMemo.findOrInsert(wire.getDefI(), rv);
          auto use = ctx.resolve(stack.back().ref).uses()[stack.back().idx - 1];
          assert(use.instr() == wire.getDefI());
          bool polarity = use.getNum() == 2;
          rv.addAND(wire.getDefI().other(0)->as<HWValue>(), polarity);

          std::cout << rv.toString() << "\n";
          rv.simplify();
          std::cout << rv.toString() << "\n\n";
        }

        continue;
      } else if (stack.back().idx != 0 && rvValid) {
        // or RV into expression
        stack.back().acc.addTerms(rv);
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

        rv = BoolExpr::trueExpr();
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
            ReadPort2{instr,
                      {}, //{Range{memory.writes}.transform([](size_t
                          // i, auto) { return i; })},
                      0});
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
          rv.addAND(instr.other(0)->as<HWValue>(), polarity);
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

  template <typename T> void buildPort(T &rd) {}

  void buildMemory(RegisterIRef reg, Memory &&memory) {
    auto mod = HWInstrRef{reg}.parentMod(ctx);
    HWInstrBuilder rbuild{ctx, mod.regs_end()};

    auto block = ctx.getCFG().blocks.create(ctx.getCFG());
    auto memI = InstrBuilder{ctx.getStore<Instr>().create(2, HW_MEMORY_DEF)}
                    .addRef(block)
                    .other()
                    .addRef(ConstantRef::fromU32(*reg.getNumBits()))
                    .instr();

    auto mem = MemoryInstrRef{memI};

    for (auto &rd : memory.reads) {
      auto oldDef = rd.instr.def()->as<WireRef>();
      MemoryInstrRef::Port port;

      // not set for read, inferred later
      port.en = nullref;
      port.enPol = 0;

      port.delay = 0;
      port.clkReg = nullref;
      port.clkPol = 0;

      port.data = rbuild.buildRegister(oldDef.getNumBits());
      HWInstrBuilder build{ctx, rd.instr};

      if (auto asSplice = rd.instr.as<SpliceIRef>(); asSplice) {
        assert(asSplice.getNumTerms() == 1);

        auto terms = asSplice.terms().transform([&](size_t, auto term) {
          auto reg = rbuild.buildRegister(32);
          build.buildStore(reg, term.getIdx());
          return MemoryInstrRef::Port::Term{
              .addr = reg,
              .fact = term.getFact(),
              .max = term.getMax() ? *term.getMax() : ~0U};
        });
        port.terms.push_back_range(terms);
        port.base = asSplice.getBase();
      } else {
        assert(rd.instr.isOpc(OP_TRUNC));
        port.base = 0;
      }

      oldDef.replaceAllUsesWith(build.buildLoad(port.data));

      mem.appendPort(ctx, port, HW_READ_PORT_DEF);
    }

    for (auto &wr : memory.writes) {
      auto asInsert = wr.instr.as<InsertIRef>();
      MemoryInstrRef::Port port;
      HWInstrBuilder build{ctx, wr.instr};

      if (!wr.enables.empty()) {
        port.en = rbuild.buildRegister(1);
        port.enPol = 1;
        auto enW = build.buildOr(
            Range{wr.enables}.transform([&build](size_t, auto term) {
              return build.buildAnd(
                  Range{term}.transform([&build](size_t, auto pair) {
                    if (pair.second)
                      return pair.first;
                    return build.buildNot(pair.first);
                  }));
            }));
        build.buildStore(port.en, enW);
      }

      if (wr.dly == 0) {
        port.delay = 0;
        port.clkReg = nullref;
        port.clkPol = 0;
      } else {
        port.delay = wr.dly;
        assert(wr.trigger.others().size() == 1 &&
               (wr.trigger.oref()->getMode(0) ==
                Any{SensMode::POSEDGE, SensMode::NEGEDGE}));
        port.clkReg = wr.trigger.other(0)->as<RegisterRef>();
        port.clkPol = wr.trigger.oref()->getMode(0) == SensMode::POSEDGE;
      }

      port.data = rbuild.buildRegister(asInsert.getLen());
      build.buildStore(port.data, asInsert.val()->as<HWValue>());

      assert(asInsert.getNumTerms() == 1);

      auto terms = asInsert.terms().transform([&](size_t, auto term) {
        auto reg = rbuild.buildRegister(32);
        build.buildStore(reg, term.getIdx());
        return MemoryInstrRef::Port::Term{.addr = reg,
                                          .fact = term.getFact(),
                                          .max = term.getMax() ? *term.getMax()
                                                               : ~0U};
      });
      port.terms.push_back_range(terms);
      port.base = asInsert.getBase();

      mem.appendPort(ctx, port, HW_WRITE_PORT_DEF);
    }

    rbuild.insertInstr(memI);
  }

  void buildMemory2(ModuleIRef mod, RegisterIRef reg, Memory &&memory) {
    HWInstrBuilder build{ctx, mod.regs_end()};
    auto newReg = build.buildRegister(reg.getNumBits());
    ctx.getCtx<HWDialectContext>().copyRegisterInfo(reg.oref(), newReg);

    for (auto &wr : memory.writes) {
      build.setInsertPoint(wr.instr);
      auto asInsert = wr.instr.as<InsertIRef>();
      HWValue en = nullref;
      if (!wr.enables.empty()) {
        en = build.buildOr(
            Range{wr.enables}.transform([&build](size_t, auto term) {
              return build.buildAnd(
                  Range{term}.transform([&build](size_t, auto pair) {
                    if (pair.second)
                      return pair.first;
                    return build.buildNot(pair.first);
                  }));
            }));
      }

      auto ref = build.buildMemStore(
          newReg, asInsert.val()->as<HWValue>(), en, wr.dly, wr.trigger,
          build.buildGEP(asInsert.getBase(), asInsert.terms()));
      wr.instr = ref;
    }

    for (auto &rd : memory.reads) {
      build.setInsertPoint(rd.instr);
      if (auto asSplice = rd.instr.as<SpliceIRef>()) {
        auto val = build.buildMemLoad(
            newReg, asSplice.getLen(), nullref, 0, nullref,
            build.buildGEP(asSplice.getBase(), asSplice.terms()),
            Range{rd.fwdWrites}.transform([&](size_t, unsigned i) {
              return MemoryWriteForward{
                  memory.writes[i].instr.as<MemStoreIRef>().port(), 0, 0};
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

    auto res = walkTree3(ld);

    DYNO_DBG({
      dbgs() << "memory ";
      dumpInstr(reg, ctx);

      if (!res) {
        dbgs() << "failed mapping: " << res.error() << "\n\n";
      } else {

        dbgs() << "write ports:\n";
        for (auto &port : res->writes) {
          dbgs() << "en{";
          for (auto term : port.enables) {
            dbgs() << "(";
            for (auto [ref, inv] : term) {
              if (inv)
                dbgs() << "!";
              dumpObj(ref);
              dbgs() << "&";
            }
            dbgs() << ") |";
          }
          dbgs() << "}:";
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
      buildMemory2(HWInstrRef{reg}.parentMod(ctx), reg, std::move(*res));
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
