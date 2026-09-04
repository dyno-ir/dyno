#pragma once
#include "dyno/Context.h"
#include "dyno/HierBlockIterator.h"
#include "dyno/Obj.h"
#include "dyno/Pass.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/HWPrinter.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Process.h"
#include "hw/Register.h"
#include "hw/analysis/ControlFlow.h"
#include "hw/analysis/SCFTraversal.h"
#include "op/IDs.h"
#include "op/StructuredControlFlow.h"
#include "support/Any.h"
#include "support/DynBitSet.h"
#include "support/SmallVec.h"

namespace dyno {

// fixme: still needs mode to respect existing partial order. currently does not
// respect order of side effect instrs.
class OrderInstrsPass : public Pass<OrderInstrsPass> {
  Context &ctx;
  ObjMap<Instr, DynSymbSet<Vec<uint64_t>, 2>> map;
  enum { PRE_MARK = 0, MARK = 1 };

public:
#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM)                                      \
  FIELD(bool, assertNoCircularDeps, true)                                      \
  /* requires fully SSA constructed load/store */                              \
  FIELD(bool, moveStoresBeforeLoads, false)
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
  Config config;
  bool storeOrderPass = false;

private:
  void handleUsesInSubBlock(BlockRef block, BlockRef sub,
                            SmallVecImpl<OperandRef> &uses) {
    if (!sub)
      return;
    for (auto instr : HierBlockRange{sub}) {
      for (auto use : instr.others()) {
        if (use->fat().getType() != Any{HW_WIRE, HW_POINTER})
          continue;
        HWInstrRef instr =
            use->as<FatDynObjRef<InstrDefUse>>()->getSingleDef()->instr();
        if (config.moveStoresBeforeLoads) {
          if (!instr.isDescendantOf(sub, ctx))
            handleUse(use, uses);
        } else {
          if (instr.parentBlock(ctx) == block)
            handleUse(use, uses);
        }
      }
    }
  }

  void handleSCFUses(InstrRef instr, BlockRef block,
                     SmallVecImpl<OperandRef> &uses) {
    switch (*instr.getDialectOpcode()) {
    case *OP_IF: {
      auto asIf = instr.as<IfInstrRef>();
      handleUsesInSubBlock(block, asIf.getTrueBlock(), uses);
      handleUsesInSubBlock(block, asIf.getFalseBlock(), uses);
      break;
    }
    case *OP_SWITCH: {
      auto asSwitch = instr.as<SwitchInstrRef>();
      for (auto caseInstr : asSwitch.block()) {
        for (auto use : caseInstr.others())
          handleUse(use, uses);
      }
      for (auto sub : asSwitch.caseBlocks()) {
        handleUsesInSubBlock(block, sub, uses);
      }
      break;
    }
    case *OP_FOR: {
      auto asFor = instr.as<ForInstrRef>();
      handleUsesInSubBlock(block, asFor.getBlock(), uses);
      break;
    }
    case *OP_WHILE: {
      auto asWhile = instr.as<WhileInstrRef>();
      handleUsesInSubBlock(block, asWhile.getCondBlock(), uses);
      handleUsesInSubBlock(block, asWhile.getBodyBlock(), uses);
      break;
    }
    case *OP_DO_WHILE: {
      auto asDoWhile = instr.as<DoWhileInstrRef>();
      handleUsesInSubBlock(block, asDoWhile.getBlock(), uses);
      break;
    }
    default:;
    }
  }

  void insertInLowestDominatingBlock(InstrRef ref) {
    SmallVec<ObjRef<Block>, 16> parentBlocks;
    ControlFlowAnalysis cfa{ctx};

    for (auto def : ref.defs()) {
      if (!Operand::isDefUseOperand(def->fat()))
        continue;
      for (auto use : def->fat().as<FatDynObjRef<InstrDefUse>>()->uses()) {
        auto parentBlockB = ctx.getCFG()[use.instr()].blockRef();
        if (!parentBlocks.empty() && parentBlockB == parentBlocks.back())
          continue;
        auto parentBlocksB = cfa.buildDepStack(parentBlockB);

        if (parentBlocks.empty()) {
          parentBlocks = Range{parentBlocksB}.reverse();
          continue;
        }

        auto [idx, _] = cfa.findFirstShared(Range{parentBlocks}.reverse(),
                                            Range{parentBlocksB});
        parentBlocks.resize(parentBlocks.size() - idx);
      }
    }

    // todo: try to hoist out of loops if not dependent
    if (parentBlocks.empty()) {
      ctx.destroyInstr(ref);
      return;
    }
    auto block = ctx.resolve(parentBlocks.back());
    // switch block only for cases, pick next higher
    if (block.defI().isOpc(OP_SWITCH))
      block = ctx.resolve(parentBlocks.end()[-2]);
    block.end().insertPrev(ref);
  }

  void handleUse(OperandRef use, SmallVecImpl<OperandRef> &uses) {
    if (use->fat().getType() == Any{HW_WIRE, HW_POINTER}) {
      auto instr =
          use->as<FatDynObjRef<InstrDefUse>>()->getSingleDef()->instr();
      if (map[instr].at(MARK))
        return;

      uses.emplace_back(instr);
    }
  }

  void handleDef(OperandRef def, SmallVecImpl<OperandRef> &uses) {
    if (def->fat().getType() == Any{HW_WIRE, HW_POINTER}) {
      for (auto use : def->as<FatDynObjRef<InstrDefUse>>()->uses()) {
        auto instr = use.instr();
        if (map[instr].at(MARK))
          continue;
        uses.emplace_back(instr);
      }
    }
  }

  void prioritzeUses(MutArrayRef<OperandRef> uses) {
    std::stable_sort(
        uses.begin(), uses.end(), [](OperandRef lhs, OperandRef rhs) {
          // todo: reg priority for storeOrderPass?
          return lhs->as<FatDynObjRef<InstrDefUse>>()->getNumUses() <
                 rhs->as<FatDynObjRef<InstrDefUse>>()->getNumUses();
        });
  }

  void visit(BlockRef block, InstrRef root,
             SmallVecImpl<ObjRef<Instr>> &ordered) {
    struct Frame {
      InstrRef instr;
    };
    SmallVec<Frame, 16> stack{Frame{root}};
    while (!stack.empty()) {
      auto &frame = stack.back();
      auto &instr = frame.instr;

      if (instr.getCustom()) {
        stack.pop_back();
        map[instr].at(MARK) = 1;
        if (!storeOrderPass || instr.isOpc(HW_STORE))
          ordered.emplace_back(instr);
        continue;
      }

      if (map[instr].at(MARK).get()) {
        stack.pop_back();
        continue;
      }
      auto pm = map[instr].at(PRE_MARK);
      if (config.assertNoCircularDeps && !storeOrderPass)
        assert(!pm);
      if (pm) {
        // circular dep
        stack.pop_back();
        continue;
      }

      pm = 1;
      instr.setCustom(1);

      SmallVec<OperandRef, 16> uses;
      if (storeOrderPass) {
        if (auto asLoad = instr.dyn_as<LoadIRef>()) {
          auto singleStore = asLoad.reg().iref().getSingleStore();
          if (singleStore && singleStore.isOpc(HW_STORE)) {
            uses.emplace_back(singleStore.as<StoreIRef>().operand(1));
          }
        }
      }
      for (auto use : instr.others())
        handleUse(use, uses);
      handleSCFUses(instr, block, uses);
      prioritzeUses(uses);
      for (auto use : uses) {
        assert(ctx.getCFG()[use.instr()].blockRef() == block);
        stack.emplace_back(use.instr());
      }
    }
  }

  void runOnBlock(BlockRef block) {
    SmallVec<ObjRef<Instr>, 32> ordered;
    ordered.reserve(block.size());

    if (config.moveStoresBeforeLoads) {
      auto mapCopy = map;
      storeOrderPass = true;
      for (auto instr : block) {
        if (instr.isOpc(HW_STORE))
          visit(block, instr, ordered);
      }
      storeOrderPass = false;
      map = std::move(mapCopy);

      auto copy = std::move(ordered);
      ordered = {};
      for (auto instr : Range{copy}.resolve(ctx)) {
        visit(block, instr, ordered);
      }
    }
    for (auto instr : block) {
      visit(block, instr, ordered);
    }

    block.clear();
    auto it = block.end();

    InstrRef unyieldInstr = nullref;

    for (auto instr : ordered) {
      auto ref = ctx.getStore<Instr>().resolve(instr);
      it.insertPrev(ref);
      if (ref.isOpc(OP_UNYIELD)) {
        assert(!unyieldInstr);
        unyieldInstr = ref;
      }
    }

    // make sure unyield is first instr again if it exists
    if (unyieldInstr) {
      ctx.getCtx<CoreDialectContext>().cfg[unyieldInstr].erase();
      block.begin().insertPrev(unyieldInstr);
    }
    assert(block.size() == ordered.size());
  }

  void coarseSortDataflowInstrs(ArrayRef<BlockRef> blocks) {
    // Drop dataflow instrs
    Vec<InstrRef> instrs;
    for (auto block : blocks) {
      auto range = Range<StableBlockIterator>{block};
      for (auto instr : range.earlyincr()) {
        if (instr.isOpc(OP_IF, OP_SWITCH, OP_FOR, OP_WHILE, OP_DO_WHILE,
                        OP_UNYIELD, OP_YIELD, HW_LOAD, HW_STORE, HW_STORE_DEFER,
                        HW_MEM_STORE, HW_MEM_LOAD, HW_ASSERT_DEFER,
                        HW_PRINT_DEFER, HW_PRINT, OP_ASSERT, OP_CASE,
                        OP_CASE_DEFAULT)) {
          map[instr].at(MARK) = 1;
          continue;
        }
        assert(!instr.getCustom());
        instrs.emplace_back(instr);
      }
    }
    for (auto instr : instrs)
      ctx.getCFG()[instr].erase();

    // Coarse sort into blocks
    // initial non-CFG aware visit
    auto &stack = instrs;
    while (!stack.empty()) {
      auto &instr = stack.back();

      if (instr.getCustom()) {
        stack.pop_back();
        map[instr].at(MARK) = 1;
        instr.clearCustom();
        insertInLowestDominatingBlock(instr);
        continue;
      }

      if (map[instr].at(MARK).get()) {
        stack.pop_back();
        continue;
      }
      auto pm = map[instr].at(PRE_MARK);
      if (config.assertNoCircularDeps)
        assert(!pm);
      if (pm) {
        // circular dep
        stack.pop_back();
        continue;
      }

      pm = 1;
      instr.setCustom(1);

      SmallVec<OperandRef, 16> defs;
      for (auto def : instr.defs())
        handleDef(def, defs);
      for (auto def : defs) {
        stack.emplace_back(def.instr());
      }
    }
  }

  void runOnProcess(ProcessIRef proc) {
    auto blocks = getSCFBlocksPreorder(proc.block());

    if (!config.moveStoresBeforeLoads && !proc.isOpc(HW_NETLIST_PROCESS_DEF)) {
      map.clear();
      map.resize(ctx.getStore<Instr>().numIDs());

      coarseSortDataflowInstrs(blocks);
    }

    map.clear();
    map.resize(ctx.getStore<Instr>().numIDs());

    for (auto block : blocks)
      runOnBlock(block);
  }

  void runOnModule(ModuleIRef mod) {
    for (auto proc : mod.procs()) {
      runOnProcess(proc);
    }
  }

public:
  void runWrapper(auto &&runFunc) { runFunc(); }
  void run() {
    runWrapper([&] {
      for (auto mod : ctx.getCtx<HWDialectContext>().activeModules()) {
        runOnModule(mod.iref());
      }
    });
  }
  void runModule(ModuleIRef mod) {
    runWrapper([&] { runOnModule(mod); });
  }
  void runProcess(ProcessIRef proc) {
    runWrapper([&] { runOnProcess(proc); });
  }

  static constexpr auto runFuncs =
      mk_tuple(&OrderInstrsPass::runProcess, &OrderInstrsPass::runModule,
               &OrderInstrsPass::run);

  auto make(Context &ctx) { return OrderInstrsPass(ctx); }
  explicit OrderInstrsPass(Context &ctx) : ctx(ctx) {}
};

}; // namespace dyno
