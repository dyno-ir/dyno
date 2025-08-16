#pragma once

#include "dyno/Constant.h"
#include "dyno/DestroyMap.h"
#include "dyno/HierBlockIterator.h"
#include "dyno/IDs.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/HWPrinter.h"
#include "hw/IDs.h"
#include "hw/Register.h"
#include "op/IDs.h"
#include "support/Bits.h"
#include "support/Debug.h"
#include "support/DenseMultimap.h"
#include "support/Utility.h"
namespace dyno {

class CSEDedupeMap {
  HWContext &ctx;
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
      auto otherI = ctx.getInstrs().resolve(it.val());
      if (!instrDeepEqual(instr, otherI))
        continue;
      return otherI;
    }
    instrDedupeMap.insert(hash, instr);
    return nullref;
  }

  void clear() { instrDedupeMap.clear(); }

  CSEDedupeMap(HWContext &ctx) : ctx(ctx) {}
};

class CommonSubexpressionEliminationPass {
  HWContext &ctx;
  CSEDedupeMap map;
  DestroyMap<Instr> instrDestroy;

  static bool ignoreForCSE(InstrRef instr) {
    return instr.isOpc(
        HW_STORE, HW_STORE_DEFER, HW_ASSERT_DEFER, OP_ASSERT, HW_PRINT_DEFER,
        HW_PRINT, OP_IF, OP_WHILE, OP_DO_WHILE, OP_FOR, OP_CALL, OP_SWITCH,
        OP_YIELD, OP_UNYIELD, OP_CASE, OP_CASE_DEFAULT, HW_COMB_PROCESS_DEF,
        HW_INIT_PROCESS_DEF, HW_FINAL_PROCESS_DEF, HW_SEQ_PROCESS_DEF,
        HW_LATCH_PROCESS_DEF, HW_NETLIST_PROCESS_DEF);
  }

  template <typename T> auto findFirstShared(ArrayRef<T> a, ArrayRef<T> b) {
    for (size_t i = 0; i < a.size(); i++)
      for (size_t j = 0; j < b.size(); j++)
        if (a[i] == b[j])
          return std::pair<size_t, size_t>(i, j);
    dyno_unreachable("no common element");
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

    BlockRef otherBl = HWInstrRef{other}.parentBlock(ctx);
    BlockRef instrBl = HWInstrRef{instr}.parentBlock(ctx);
    if (otherBl == instrBl) {
      DEBUG("CSE", {
        dbgs() << "merging in same block:\n";
        dumpInstr(instr);
        dumpInstr(other);
      })
      return;
    }

    auto buildStack = [&](BlockRef block, SmallVecImpl<OperandRef> &stack) {
      while (true) {
        auto instr = block.defI();
        // switch block is a special context, doesn't count here.
        // if (!instr.isOpc(OP_SWITCH))
        stack.emplace_back(*block.def());
        auto parent = HWInstrRef{instr}.parentBlock(ctx);
        if (!parent)
          break;
        block = parent;
      }
    };

    SmallVec<OperandRef, 4> otherStack{*other.begin()};
    buildStack(otherBl, otherStack);
    SmallVec<OperandRef, 4> instrStack{*instr.begin()};
    buildStack(instrBl, instrStack);
    auto [otherIdx, instrIdx] = findFirstShared(
        ArrayRef{otherStack}.drop_front(), ArrayRef{instrStack}.drop_front());
    InstrRef otherPred = otherStack[otherIdx].instr();
    InstrRef instrPred = instrStack[instrIdx].instr();
    auto block = HWInstrRef{otherPred}.parentBlock(ctx);
    assert(block == HWInstrRef{instrPred}.parentBlock(ctx));

    if (block.defI().isOpc(OP_SWITCH))
      block = HWInstrRef{block.defI()}.parentBlock(ctx);

    DEBUG("CSE", {
      dbgs() << "merging in different blocks:\n";
      dumpInstr(instr, ctx);
      dumpInstr(other, ctx);
      dumpInstr(HWInstrRef{otherPred}.parentBlock(ctx).defI(), ctx);
      dumpInstr(HWInstrRef{instrPred}.parentBlock(ctx).defI(), ctx);
    })

    ctx.getCFG()[other].erase();
    auto it = block.begin();
    it.insertPrev(other);

    // for (auto it = block.begin(); it != block.end(); ++it) {
    //   if (*it == otherPred || *it == instrPred) {
    //     if (*it == other)
    //       return;
    //     ctx.getCFG()[other].erase();
    //     it.insertPrev(other);
    //     return;
    //   }
    // }
    // dyno_unreachable("")
  }

  void runOnProcess(ProcessIRef proc) {
    map.clear();
    Range range{StableHierBlockRangeIter{proc.block().begin()},
                StableHierBlockRangeIter{proc.block().end()}};
    for (auto instr : range) {
      if (!ignoreForCSE(instr)) {
        runOnInstr(instr);
      }
    }
  }

  void runOnModule(ModuleIRef mod) {
    for (auto proc : mod.procs())
      runOnProcess(proc);
  }

public:
  void run() {
    instrDestroy.resize(ctx.getInstrs().numIDs());
    for (auto mod : ctx.activeModules()) {
      runOnModule(mod.iref());
    }
    HWInstrBuilder build{ctx};
    instrDestroy.apply(ctx.getInstrs(),
                       [&](InstrRef ref) { build.destroyInstr(ref); });
    instrDestroy.clear();
  }
  explicit CommonSubexpressionEliminationPass(HWContext &ctx)
      : ctx(ctx), map(ctx) {}
};

}; // namespace dyno
