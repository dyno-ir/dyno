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

  struct WritePort2 {
    InstrRef instr; // insert/concat
    SmallVec<std::pair<HWValue, bool>, 2> enables;
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

  std::expected<Memory, std::string> walkTree(LoadIRef load) {
    Memory memory;
    auto val = load.value().as<WireRef>();
    WireRef nextVal = nullref;
    bool end = false;

    while (true) {
      SmallDenseSet<ObjRef<Instr>, 8> coveredMUXs;
      auto flipMux = [&coveredMUXs](InstrRef instr) {
        if (auto [found, it] = coveredMUXs.findOrInsert(instr); found)
          coveredMUXs.erase(it);
      };

      for (auto op : val.uses()) {
        auto instr = op.instr();
        switch (*instr.getDialectOpcode()) {

        case *HW_STORE_DEFER:
          for (auto &st : memory.writes) {
            st.dly = 1;
            st.trigger = instr.as<StoreIRef>().trigger();
          }
          [[fallthrough]];
        case *HW_STORE: {
          auto asStore = instr.as<StoreIRef>();
          if (!asStore.isFullReg())
            return std::unexpected("expected full reg store");

          end = true;
          break;
        }

        case *HW_SPLICE: {
          auto asSplice = instr.as<SpliceIRef>();
          if (op != asSplice.in())
            return std::unexpected("expected splice in");
          memory.reads.emplace_back(
              ReadPort2{instr,
                        {Range{memory.writes}.transform(
                            [](size_t i, auto) { return i; })},
                        0});
          break;
        }

        case *OP_TRUNC: {
          memory.reads.emplace_back(
              ReadPort2{instr,
                        {Range{memory.writes}.transform(
                            [](size_t i, auto) { return i; })},
                        0});
          break;
        }

        case *HW_INSERT: {
          auto asInsert = instr.as<InsertIRef>();
          if (nextVal)
            return std::unexpected("parallel inserts, not implemented");
          nextVal = asInsert.out().as<WireRef>();

          if (op != asInsert.in())
            return std::unexpected("expected insert in");

          if (auto use = nextVal.getSingleUse();
              use && use->instr().isOpc(HW_MUX)) {
            auto mux = use->instr();
            if (*use == mux.other(0))
              return std::unexpected("expected mux val not select");
            auto otherUse = mux.operand(use->getNum() == 2 ? 3 : 2);

            if (otherUse->as<HWValue>() != val)
              return std::unexpected("expected bypass mux");

            nextVal = mux.def(0)->as<WireRef>();
            auto sel = mux.other(0)->as<HWValue>();
            bool selPol = use->getNum() == 2;

            memory.writes.emplace_back(
                WritePort2{instr, {{sel, selPol}}, 0, nullref});
            flipMux(mux);
          } else {
            nextVal = asInsert.out()->as<WireRef>();
            memory.writes.emplace_back(WritePort2{instr, {}, 0, nullref});
          }
          break;
        }

        case *HW_MUX: {
          flipMux(instr);
          break;
        }

        default:
          return std::unexpected(
              "unexpected instr: " +
              ContextPrinterWrapper<>{ctx}.toString(instr.getDialectOpcode()));
        }
      }

      if (coveredMUXs.size() != 0)
        return std::unexpected("incorrect MUX covering");

      if (end != !nextVal)
        return std::unexpected("end/next val mismatch");

      if (end)
        break;

      val = std::exchange(nextVal, nullref);
    }

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
        auto enW = build.buildAnd(
            Range{wr.enables}.transform([&build](size_t, auto pair) {
              if (pair.second)
                return pair.first;
              return build.buildNot(pair.first);
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

    for (auto &wr : memory.writes) {
      build.setInsertPoint(wr.instr);
      auto asInsert = wr.instr.as<InsertIRef>();
      HWValue en = nullref;
      if (!wr.enables.empty()) {
        en = build.buildAnd(
            Range{wr.enables}.transform([&build](size_t, auto pair) {
              if (pair.second)
                return pair.first;
              return build.buildNot(pair.first);
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

    auto res = walkTree(ld);

    DYNO_DBG("SimpleMemoryMappingPass", {
      dbgs() << "memory ";
      dumpInstr(reg, ctx);

      if (!res) {
        dbgs() << "failed mapping: " << res.error() << "\n";
      } else {

        dbgs() << "write ports:\n";
        for (auto &port : res->writes) {
          dbgs() << "en{";
          for (auto &en : port.enables) {
            dumpObj(en.first);
            dbgs() << ":" << en.second << ",";
          }
          dbgs() << "}:";
          dumpInstr(port.instr);
        }

        dbgs() << "read ports:\n";
        for (auto &port : res->reads) {
          dumpInstr(port.instr);
        }
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
