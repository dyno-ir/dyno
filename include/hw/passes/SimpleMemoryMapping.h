#pragma once

#include "dyno/Context.h"
#include "dyno/Pass.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include <cstdint>
namespace dyno {

class SimpleMemoryMappingPass : public Pass<SimpleMemoryMappingPass> {

  struct AbstractMemory {
    uint32_t bitsPerWord;
    uint32_t addressBits;

    bool hasByteWE;
    uint32_t byteSize;

    uint32_t readDelay;
    uint32_t writeDelay;

    uint32_t numReadPorts;
    uint32_t numWritePorts;
    uint32_t numRWPorts;

    enum class UnselectedReadVal { LAST, INVALID };
  };
  struct InputRegister {
    StoreIRef store;
    LoadIRef load;
  };
  struct ReadPort {
    SpliceIRef splice;
    std::optional<InputRegister> addrReg;
    std::optional<InputRegister> dataReg;
  };
  struct WritePort {
    InsertIRef insert;
    WireRef enable;
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
      if (!instr.isOpc(HW_SPLICE))
        return false;

      out.emplace_back(instr.as<SpliceIRef>());
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

      WireRef enable = nullref;
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
      }

      if (instr.isOpc(HW_INSERT)) {
        auto insert = instr.as<InsertIRef>();
        out.emplace_back(insert, enable, enablePolarity);
        cur = insert.in()->as<HWValue>();
        coveredUses.insert(insert);
      } else
        return false;
    }
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

  void runOnRegister(RegisterIRef reg) {
    if (*reg.getNumBits() < 128)
      return;
    SmallVec<WritePort, 4> writePorts;
    SmallVec<ReadPort, 4> readPorts;
    coveredUses.clear();

    if (!findWritePorts(writePorts, reg))
      return;
    if (!findReadPorts(readPorts, reg))
      return;

    DYNO_DBG("SimpleMemoryMappingPass", {
      dbgs() << "for memory ";
      dumpInstr(reg, ctx);

      dbgs() << "write ports:\n";
      for (auto &port : writePorts) {
        dbgs() << "en";
        dumpObj(port.enable);
        dbgs() << " = " << port.enablePolarity << ": ";
        dumpInstr(port.insert);
      }

      dbgs() << "read ports:\n";
      for (auto &port : readPorts) {
        dumpInstr(port.splice);
      }
    })
  }

  void runOnModule(ModuleIRef mod) {
    for (auto reg : mod.regs()) {
      runOnRegister(reg);
    }
  }

public:
  auto make(Context &ctx) { return SimpleMemoryMappingPass(ctx); }
  explicit SimpleMemoryMappingPass(Context &ctx) : ctx(ctx) {}
  void run() {
    for (auto mod : ctx.getCtx<HWDialectContext>().activeModules()) {
      runOnModule(mod.iref());
    }
  }
};

}; // namespace dyno
