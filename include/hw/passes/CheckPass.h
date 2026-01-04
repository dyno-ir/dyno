#pragma once

#include "dyno/HierBlockIterator.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/HWPrinter.h"
#include "hw/IDs.h"
#include "op/IDs.h"
#include "support/Debug.h"
#include "dyno/Pass.h"
namespace dyno {

class CheckPass : public Pass<CheckPass> {
  HWContext &ctx;
  bool hasError = false;

public:
  struct Config {
    bool dominance = true;
    bool operandsDefined = true;
    bool danglingBlocks = true;
    bool noLoops = false;
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

  template <typename... Ts> void error(DynObjRef ref, Ts... ts) {
    dumpObj(ctx.resolveObj(ref));
    dbgs() << "\nerror: ";
    ((dbgs() << ts), ...);
    dbgs() << "\n";
    hasError = true;
  }

  template <typename... Ts> void error(BlockRef block, Ts... ts) {
    dumpObj(block);
    dbgs() << ": {\n";
    for (auto instr : block)
      dumpInstr(instr, ctx);
    dbgs() << "}\n";
    dbgs() << "error: ";
    ((dbgs() << ts), ...);
    dbgs() << "\n";
    hasError = true;
  }

  void checkOperands(ModuleIRef mod) {
    Range range{HierBlockRangeIter{mod.block().begin()},
                HierBlockRangeIter{mod.block().end()}};
    for (auto instr : range) {
      for (auto op : instr) {
        if (!op->fat()) {
          error(instr, "undefined operand");
        }

        if (op.isDef() && op->is<WireRef>())
          if (op->as<WireRef>().getNumDefs() != 1)
            error(instr, "multi-def operand");

        switch (*instr.getDialectOpcode()) {
#define LAMBDA(opc, ib, cb, bi) case *opc:
          FOR_HW_SIMPLE_OPS(LAMBDA)
#undef LAMBDA
          {
            if (op.isDef())
              continue;
            auto bits = instr.def(0)->as<WireRef>().getNumBits();
            if (op->as<HWValue>().getNumBits() != bits)
              error(instr, "operand width mismatch");
          }

#define LAMBDA(opc, bi) case *opc:
          FOR_OP_ALL_COMPARE_OPS(LAMBDA)
#undef LAMBDA
          {
            if (op.isDef())
              continue;
            auto bits = instr.other(0)->as<HWValue>().getNumBits();
            if (op->as<HWValue>().getNumBits() != bits)
              error(instr, "operand width mismatch");
          }
        }
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

  void checkNoDanglingBlocks() {
    for (auto block : ctx.getCFG().blocks)
      if (block->defUse.getNumDefs() == 0)
        error(block, "dangling block");
  }

  void checkNoLoops() {
    for (auto instr : ctx.getInstrs())
      if (instr.isOpc(OP_FOR, OP_WHILE, OP_DO_WHILE)) {
        error(instr, "illegal loop");
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
  auto make(HWContext &ctx) { return CheckPass(ctx); }
  explicit CheckPass(HWContext &ctx) : ctx(ctx) {}
  void run() {
    for (auto mod : ctx.activeModules()) {
      runOnModule(mod.iref());
    }
    if (config.danglingBlocks)
      checkNoDanglingBlocks();
    if (config.noLoops)
      checkNoLoops();
    if (hasError) {
      {
        std::ofstream str{"dump_error.dyno"};
        HWPrinter print{str};
        print.printCtx(ctx);
      }
      abort();
    }
  }
};
}; // namespace dyno
