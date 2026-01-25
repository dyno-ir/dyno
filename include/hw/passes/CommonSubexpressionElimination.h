#pragma once

#include "dyno/Context.h"
#include "dyno/DestroyMap.h"
#include "dyno/HierBlockIterator.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "dyno/Pass.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/HWPrinter.h"
#include "hw/IDs.h"
#include "hw/Register.h"
#include "hw/analysis/ControlFlow.h"
#include "hw/analysis/SCFTraversal.h"
#include "op/IDs.h"
#include "support/Bits.h"
#include "support/Debug.h"
#include "support/DenseMultimap.h"
namespace dyno {

class CSEDedupeMap {
  Context &ctx;
  DenseMultimap<uint32_t, ObjRef<Instr>> instrDedupeMap;

  uint32_t hashOperand(OperandRef operand) {
    return DenseMapInfo<DynObjRef>::getHashValue(operand->thin());
  }
  uint32_t hashInstr(InstrRef instr) {
    uint32_t acc = 0;
    acc = hash_combine(acc, hash_u32(instr.getDialectOpcode().raw()));
    for (auto op : instr.others())
      acc = hash_combine(acc, hashOperand(op));
    return acc;
  }

  static bool defIsCompatible(OperandRef lhs, OperandRef rhs) {
    if (lhs->is<WireRef>() && rhs->is<WireRef>()) {
      if (*lhs->as<WireRef>()->numBits != *rhs->as<WireRef>()->numBits)
        return false;
    }
    if (lhs->is<RegisterRef>() && rhs->is<RegisterRef>()) {
      if (*lhs->as<RegisterRef>()->numBits != *rhs->as<RegisterRef>()->numBits)
        return false;
    }
    return true;
  }

  static bool instrDeepEqual(InstrRef lhs, InstrRef rhs) {
    if (lhs.getNumOperands() != rhs.getNumOperands())
      return false;
    if (lhs.getNumDefs() != rhs.getNumDefs())
      return false;
    if (lhs.getDialectOpcode() != rhs.getDialectOpcode())
      return false;

    for (size_t i = 0; i < lhs.getNumOthers(); i++)
      if (lhs.other(i)->thin() != rhs.other(i)->thin())
        return false;

    // todo: incorporate this into the hash
    for (size_t i = 0; i < lhs.getNumDefs(); i++)
      if (!defIsCompatible(lhs.def(i), rhs.def(i)))
        return false;

    return true;
  }

public:
  InstrRef findOrInsert(InstrRef instr) {
    uint32_t hash = hashInstr(instr);
    auto it = instrDedupeMap.find(hash);
    for (; it != instrDedupeMap.end(); it = instrDedupeMap.find_next(it)) {
      auto otherI = ctx.getStore<Instr>().resolve(it.val());
      if (!instrDeepEqual(instr, otherI))
        continue;
      return otherI;
    }
    instrDedupeMap.insert(hash, instr);
    return nullref;
  }

  void clear() { instrDedupeMap.clear(); }

  CSEDedupeMap(Context &ctx) : ctx(ctx) {}
};

class CommonSubexpressionEliminationPass
    : public Pass<CommonSubexpressionEliminationPass> {
  Context &ctx;
  CSEDedupeMap map;
  DestroyMap<Instr> instrDestroy;
  ControlFlowAnalysis controlFlowAnalysis;

public:
#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM) FIELD(bool, differentBlocks, true)
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
  Config config;

private:
  static bool ignoreForCSE(InstrRef instr) {
    return instr.isOpc(
        HW_STORE, HW_STORE_DEFER, HW_ASSERT_DEFER, OP_ASSERT, HW_PRINT_DEFER,
        HW_PRINT, OP_IF, OP_WHILE, OP_DO_WHILE, OP_FOR, OP_CALL, OP_SWITCH,
        OP_YIELD, OP_UNYIELD, OP_CASE, OP_CASE_DEFAULT, HW_CASE_Z, HW_CASE_X,
        HW_COMB_PROCESS_DEF, HW_INIT_PROCESS_DEF, HW_FINAL_PROCESS_DEF,
        HW_SEQ_PROCESS_DEF, HW_LATCH_PROCESS_DEF, HW_NETLIST_PROCESS_DEF,
        AIG_OUTPUT, AIG_INPUT);
  }

  void runOnInstr(InstrRef instr) {
    auto other = map.findOrInsert(instr);
    if (!other)
      return;

    assert(other != instr);
    assert(HWInstrRef{other}.parentProc(ctx) ==
           HWInstrRef{instr}.parentProc(ctx));

    // assuming other comes first. this is gonna be tricky for worklist-based
    // approach, order might be flipped and we don't have a quick way to find
    // which is first.
    for (auto [i, def] : instr.defs().enumerate()) {
      assert(Operand::isDefUseOperand(def->thin()));
      def->as<FatDynObjRef<InstrDefUse>>()->replaceAllUsesWith(
          other.def(i)->as<FatDynObjRef<InstrDefUse>>());
    }
    instrDestroy.mark(instr);
    ctx.getCtx<CoreDialectContext>().instrSourceLocInfo.copyDebugInfo(instr,
                                                                      other);

    BlockRef otherBl = HWInstrRef{other}.parentBlock(ctx);
    BlockRef instrBl = HWInstrRef{instr}.parentBlock(ctx);
    if (otherBl == instrBl) {
      DYNO_DBG("CSE", {
        dbgs() << "merging in same block:\n";
        dumpInstr(instr, ctx);
        dumpInstr(other, ctx);
      })
      return;
    }

    auto block = controlFlowAnalysis.findSharedParentBlock(instrBl, otherBl);

    DYNO_DBG("CSE", {
      dbgs() << "merging in different blocks:\n";
      dumpInstr(instr, ctx);
      dumpInstr(other, ctx);
    })

    ctx.getCtx<CoreDialectContext>().cfg[other].erase();
    auto it = block.begin();
    it.insertPrev(other);

    // for (auto it = block.begin(); it != block.end(); ++it) {
    //   if (*it == otherPred || *it == instrPred) {
    //     if (*it == other)
    //       return;
    //     ctx.getCtx<CoreDialectContext>().cfg[other].erase();
    //     it.insertPrev(other);
    //     return;
    //   }
    // }
    // dyno_unreachable("")
  }

  void runOnProcess(ProcessIRef proc) {
    if (config.differentBlocks) {
      map.clear();
      Range range{StableHierBlockRangeIter{proc.block().begin()},
                  StableHierBlockRangeIter{proc.block().end()}};
      for (auto instr : range) {
        if (!ignoreForCSE(instr)) {
          runOnInstr(instr);
        }
      }
    } else {
      auto blocks = getSCFBlocksPreorder(proc.block());
      for (auto block : blocks) {
        map.clear();
        for (auto instr : block) {
          if (!ignoreForCSE(instr))
            runOnInstr(instr);
        }
      }
    }
  }

  void runOnModule(ModuleIRef mod) {
    for (auto proc : mod.procs())
      runOnProcess(proc);
  }

public:
  void run() {
    instrDestroy.resize(ctx.getStore<Instr>().numIDs());
    for (auto mod : ctx.getCtx<HWDialectContext>().activeModules()) {
      runOnModule(mod.iref());
    }
    HWInstrBuilder build{ctx};
    instrDestroy.apply(ctx.getStore<Instr>(),
                       [&](InstrRef ref) { build.destroyInstr(ref); });
    instrDestroy.clear();
  }
  explicit CommonSubexpressionEliminationPass(Context &ctx)
      : ctx(ctx), map(ctx), controlFlowAnalysis(ctx) {}
  static auto make(Context &ctx) {
    return CommonSubexpressionEliminationPass{ctx};
  }
};

}; // namespace dyno
