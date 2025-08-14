#pragma once

#include "dyno/HierBlockIterator.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/HWPrinter.h"
#include "hw/IDs.h"
#include "support/Debug.h"
namespace dyno {

class CheckPass {
  HWContext &ctx;
  bool hasError;

public:
  struct Config {
    bool dominance = true;
    bool operandsDefined = true;
  };

  Config config;

  template <typename... Ts> void error(InstrRef instr, Ts... ts) {
    dumpInstr(HWInstrRef{instr}.parentBlock(ctx).defI(), ctx);
    dumpInstr(instr, ctx);
    dbgs() << "error: ";
    ((dbgs() << ts), ...);
    dbgs() << "\n";
    hasError = true;
  }

  void checkOperands(ModuleIRef mod) {
    Range range{HierBlockRangeIter{mod.block().begin()},
                HierBlockRangeIter{mod.block().end()}};
    for (auto instr : range) {
      for (auto op : instr)
        if (!op->fat()) {
          error(instr, "undefined operand");
        }
    }
  }

  void checkWireDominance(ProcessIRef proc) {
    if (proc.isOpc(HW_NETLIST_PROCESS_DEF))
      return;
    ObjMapVec<Wire, bool> seen;
    seen.resize(ctx.getWires().numIDs());

    Range range{HierBlockRangeIter{proc.block().begin()},
                HierBlockRangeIter{proc.block().end()}};

    for (auto instr : range) {
      for (auto use : instr.others()) {
        if (!use->is<WireRef>())
          continue;
        auto wire = use->as<WireRef>();
        if (!seen[wire])
          error(instr, "wire does not respect dominance");
      }

      for (auto def : instr.defs())
        if (auto asWire = def->dyn_as<WireRef>())
          seen[asWire] = 1;
    }
  }

  void runOnModule(ModuleIRef mod) {
    if (config.operandsDefined)
      checkOperands(mod);
    if (config.dominance)
      for (auto proc : mod.procs())
        checkWireDominance(proc);
  }

public:
  CheckPass(HWContext &ctx) : ctx(ctx) {}
  void run() {
    for (auto mod : ctx.activeModules()) {
      runOnModule(mod.iref());
    }
    if (hasError)
      exit(-1);
  }
};
}; // namespace dyno
