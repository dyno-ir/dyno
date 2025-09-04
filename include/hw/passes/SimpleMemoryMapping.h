#pragma once

#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "support/DynBitSet.h"
#include <cstdint>
namespace dyno {

class SimpleMemoryMappingPass {

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
  struct ReadPort {
    SpliceIRef splice;
  };
  struct WritePort {
    InsertIRef insert;
    SmallVec<WireRef, 4> enablesValues;
    DynSymbSet<SmallVec<uint64_t, 1>, 1> enablePolarities;
    auto enables() { return Range{enablesValues}.zip(enablePolarities); }
  };

  HWContext &ctx;
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

      if (!walkWriteTree(out, instr.as<StoreIRef>()))
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
  bool walkWriteTree(SmallVecImpl<WritePort> &out, StoreIRef store) {
    struct Frame {
      WireRef wire;
      uint idx;
    };
    if (!store.value().is<WireRef>())
      return false;
    SmallVec<Frame, 4> stack{Frame{store.value().as<WireRef>(), 0}};
    WritePort prefix;

    while (!stack.empty()) {
      auto &frame = stack.back();
      auto instr = frame.wire.getDefI();
      coveredUses.findOrInsert(instr);

      switch (*instr.getDialectOpcode()) {
      case *HW_MUX: {
        auto sel = instr.other(0)->as<WireRef>();
        if (frame.idx == 0) {
          prefix.enablesValues.emplace_back(sel.as<WireRef>());
          prefix.enablePolarities.push_back(1);
        } else if (frame.idx == 1) {
          prefix.enablePolarities.back() = 0;
        } else if (frame.idx == 2) {
          prefix.enablesValues.pop_back();
          prefix.enablePolarities.pop_back();
          stack.pop_back();
          break;
        }
        auto val = instr.other(frame.idx + 1)->dyn_as<WireRef>();
        if (!val)
          return false;
        frame.idx++;
        stack.emplace_back(val, 0);
        break;
      }
      case *HW_INSERT: {
        auto asInsert = instr.as<InsertIRef>();
        auto &copy = out.emplace_back(prefix);
        copy.insert = asInsert;

        auto pad = asInsert.in()->dyn_as<WireRef>();
        if (!pad)
          return false;

        stack.pop_back();
        stack.emplace_back(pad, 0);
        break;
      }

      case *HW_LOAD: {
        auto asLoad = instr.as<LoadIRef>();
        // todo: support partial loads.
        if (!asLoad.isFullReg())
          return false;
        if (asLoad.reg() != store.reg())
          return false;

        stack.pop_back();
        break;
      }

      default:
        return false;
      }
    }

    return true;
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

    DEBUG("SimpleMemoryMappingPass", {
      dbgs() << "for memory ";
      dumpInstr(reg, ctx);

      dbgs() << "write ports:\n";
      for (auto &port : writePorts) {
        dbgs() << "en {";
        for (auto [val, pol] : port.enables()) {
          if (pol == 0)
            dbgs() << "!";
          dumpObj(val);
          dbgs() << ", ";
        }
        dbgs() << "}: ";
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
  SimpleMemoryMappingPass(HWContext &ctx) : ctx(ctx) {}
  void run() {
    for (auto mod : ctx.activeModules()) {
      runOnModule(mod.iref());
    }
  }
};

}; // namespace dyno
