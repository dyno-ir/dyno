#pragma once

#include "dyno/Context.h"
#include "dyno/HierBlockIterator.h"
#include "dyno/Pass.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/HWPrinter.h"
#include "hw/IDs.h"
#include "hw/analysis/RegisterValue.h"
#include "op/IDs.h"
#include "support/Debug.h"
#include "support/ErrorRecovery.h"
namespace dyno {

class CheckPass : public Pass<CheckPass> {
  Context &ctx;
  bool hasError = false;

public:
#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM)                                      \
  FIELD(bool, dominance, true)                                                 \
  FIELD(bool, operandsDefined, true)                                           \
  FIELD(bool, danglingBlocks, false)                                           \
  FIELD(bool, noLoops, false)                                                  \
  FIELD(bool, multiDriven, false)
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
  Config config;

  // bool optimizeOneHotMux(InstrRef instr) {
  //   // hash to find duplicates
  //   bool change = false;
  //   SmallDenseMap<DynObjRef, SmallVec<uint32_t, 2>, 16> map;
  //   for (auto [sel, val] : Range{instr.others()}.pairwise()) {
  //     auto [found, iter] = map.findOrInsert(sel->thin(), {});

  //     // if this is the first time looking at select, check if it's known
  //     if (!found) [[likely]] {
  //       auto known = knownBits.getKnownBits(sel->as<HWValue>());
  //       if (known.valueEquals(1)) {
  //         // found a one entry, remove the instr
  //         replaceUses(instr.def(0)->as<WireRef>(), val->as<HWValue>());
  //         deleteMatchedInstr(instr);
  //         return true;
  //       } else if (known.valueEquals(0)) {
  //         // drop zero entries
  //         change = true;
  //         continue;
  //       }
  //     } else
  //       change = true;
  //   }
  // }

  template <typename... Ts> void error(InstrRef instr, Ts... ts) {
    dumpInstr(HWInstrRef{instr}.parentBlock(ctx).defI(), ctx, true, false);
    dumpInstr(instr, ctx, true, false);
    dbgs() << "error: ";
    ((dbgs() << ts), ...);
    dbgs() << "\n\n";
    hasError = true;
  }

  template <typename... Ts> void error(DynObjRef ref, Ts... ts) {
    dumpObj(ctx.resolve(ref));
    dbgs() << "\nerror: ";
    ((dbgs() << ts), ...);
    dbgs() << "\n\n";
    hasError = true;
  }

  template <typename... Ts> void error(BlockRef block, Ts... ts) {
    dumpObj(block);
    dbgs() << ": {\n";
    for (auto instr : block)
      dumpInstr(instr, ctx, true, false);
    dbgs() << "}\n";
    dbgs() << "error: ";
    ((dbgs() << ts), ...);
    dbgs() << "\n\n";
    hasError = true;
  }

  struct MultiDrivenFragment : public GenericFragment {
    bool isWritten = false;
    bool *error = nullptr;

    MultiDrivenFragment() = default;
    MultiDrivenFragment(uint32_t dstAddr, uint32_t len, bool isWritten,
                        bool *error)
        : GenericFragment{dstAddr, len}, isWritten(isWritten), error(error) {}

    bool overwrites(MultiDrivenFragment &) { return true; }
    bool fuses(MultiDrivenFragment &) { return false; }
    bool intersects(MultiDrivenFragment &) { return true; }
    bool abstractEquals(const MultiDrivenFragment &) const { return false; }
    MultiDrivenFragment intersect(MultiDrivenFragment &other) {
      if (other.isWritten && error)
        *error = true;
      isWritten = true;
      return *this;
    }
  };

  void checkMultiDriven(ModuleIRef mod) {
    for (auto reg : mod.regs()) {
      auto numBits = reg.getNumBits();
      if (!numBits)
        continue;
      bool errorFlag = false;
      GenericPartitions<MultiDrivenFragment, 4> part{*numBits, false,
                                                     &errorFlag};
      for (auto use : reg.oref().uses()) {
        auto instr = use.instr();
        if (!instr.isOpc(HW_STORE, HW_STORE_DEFER))
          continue;
        auto store = instr.as<StoreIRef>();
        auto [addr, len] = store.getConstAccessRange();
        part.writeSingle(addr, len, true, &errorFlag);
        if (errorFlag) {
          error(reg, "register has overlapping stores (multi-driven)");
          break;
        }
      }
    }
  }

  void checkOperands(ModuleIRef mod) {
    Range range{HierBlockRangeIter{mod.block().begin()},
                HierBlockRangeIter{mod.block().end()}};
    for (auto instr : range) {
      for (auto op : instr) {
        if (!op->fat()) {
          error(instr, "undefined operand");
        }

        if (op->fat().getType() == Any{HW_WIRE, HW_POINTER}) {
          if (op->as<FatDynObjRef<InstrDefUse>>()->getNumDefs() > 1)
            error(instr, "multi-def operand");
          if (op->as<FatDynObjRef<InstrDefUse>>()->getNumDefs() == 0)
            error(instr, "zero-def operand");
        }

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
            break;
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
            break;
          }
        }
      }
    }
  }

  void checkWireDominance(ProcessIRef proc) {
    if (proc.isOpc(HW_NETLIST_PROCESS_DEF))
      return;
    ObjMapVec<Wire, bool> seen;
    seen.resize(ctx.getStore<Wire>().numIDs());

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
    for (auto block : ctx.getCtx<CoreDialectContext>().cfg.blocks)
      if (block->defUse.getNumDefs() == 0)
        error(block, "dangling block");
  }

  void checkNoLoops(ModuleIRef mod) {
    for (auto instr : HierBlockRange{mod.block()}) {
      if (instr.isOpc(OP_FOR, OP_WHILE, OP_DO_WHILE))
        error(instr, "illegal loop (failed to unroll)");
    }
  }

  void runOnModule(ModuleIRef mod) {
    if (config.operandsDefined)
      checkOperands(mod);
    if (config.dominance)
      for (auto proc : mod.procs())
        checkWireDominance(proc);
    if (config.multiDriven)
      checkMultiDriven(mod);
    if (config.noLoops)
      checkNoLoops(mod);
  }

public:
  auto make(Context &ctx) { return CheckPass(ctx); }
  explicit CheckPass(Context &ctx) : ctx(ctx) {}
  void run() {
    for (auto mod : ctx.getCtx<HWDialectContext>().activeModules()) {
      runOnModule(mod.iref());
    }
    if (config.danglingBlocks)
      checkNoDanglingBlocks();
    if (hasError) {
      {
        std::ofstream str{"dump_error.dyno"};
        HWPrinter print{str};
        print.printCtx(ctx);
      }
      report_fatal_error("check pass failed");
    }
  }
  void runModule(ModuleIRef mod) { runOnModule(mod); }
  static constexpr auto runFuncs =
      mk_tuple(&CheckPass::run, &CheckPass::runModule);
};
}; // namespace dyno
