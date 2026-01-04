#pragma once
#include "dyno/HierBlockIterator.h"
#include "dyno/Obj.h"
#include "dyno/Pass.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/Register.h"
#include "hw/analysis/SCFTraversal.h"
#include "op/IDs.h"
#include "op/StructuredControlFlow.h"
#include "support/DynBitSet.h"

namespace dyno {

// fixme: still needs mode to respect existing partial order. currently does not
// respect order of side effect instrs.
class OrderInstrsPass : public Pass<OrderInstrsPass> {
  HWContext &ctx;
  ObjMap<Instr, DynSymbSet<std::vector<uint64_t>, 2>> map;
  enum { PRE_MARK = 0, MARK = 1 };

public:
  struct Config {
    bool assertNoCircularDeps = true;
    bool moveStoresBeforeLoads = false;
  };
  Config config;

private:
  void handleUsesInSubBlock(BlockRef block, BlockRef sub,
                            SmallVecImpl<OperandRef> &uses) {
    if (!sub)
      return;
    for (auto instr : HierBlockRange{sub}) {
      for (auto use : instr.others()) {
        auto wire = use->dyn_as<WireRef>();
        if (!wire)
          continue;
        auto instr = HWInstrRef{wire.getDefI()};
        if (!instr.isDescendantOf(sub, ctx))
          handleUse(block, use, uses);
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
          handleUse(block, use, uses);
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

  void handleUse(BlockRef block, OperandRef use,
                 SmallVecImpl<OperandRef> &uses) {
    if (auto asWire = use->dyn_as<WireRef>()) {
      auto instr = asWire.getDefI();
      if (map[instr].at(MARK))
        return;
      if (ctx.getCFG()[instr].blockRef() != block) {
        assert(!instr.isOpc(OP_UNYIELD));
        // dbgs() << "out of block:\n";
        // HWPrinter print{dbgs()};
        // print.printInstr(block.defI(), ctx);
        // dbgs() << "in question:\n";
        // print.printInstr(instr, ctx);
      }

      uses.emplace_back(instr);
    }
  }

  void prioritzeUses(MutArrayRef<OperandRef> uses) {
    std::stable_sort(
        uses.begin(), uses.end(), [](OperandRef lhs, OperandRef rhs) {
          return lhs->as<FatDynObjRef<InstrDefUse>>()->getNumUses() <
                 rhs->as<FatDynObjRef<InstrDefUse>>()->getNumUses();
        });
  }

  void visit(BlockRef block, InstrRef root,
             SmallVecImpl<ObjRef<Instr>> &ordered) {
    struct Frame {
      InstrRef instr;
    };
    SmallVec<Frame, 8> stack{Frame{root}};
    while (!stack.empty()) {
      auto &frame = stack.back();
      auto &instr = frame.instr;

      if (instr.getCustom()) {
        stack.pop_back();
        map[instr].at(MARK) = 1;
        ordered.emplace_back(instr);
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

      SmallVec<OperandRef, 4> uses;
      for (auto use : instr.others())
        handleUse(block, use, uses);
      handleSCFUses(instr, block, uses);
      prioritzeUses(uses);
      for (auto use : uses)
        stack.emplace_back(use.instr());
    }
  }

  void runOnBlock(BlockRef block) {
    SmallVec<ObjRef<Instr>, 32> ordered;
    ordered.reserve(block.size());

    if (config.moveStoresBeforeLoads) {
      for (auto instr : block) {
        if (instr.isOpc(HW_STORE))
          visit(block, instr, ordered);
      }
    }
    for (auto instr : block) {
      visit(block, instr, ordered);
    }

    block.clear_unsafe();
    auto it = block.end();

    InstrRef unyieldInstr = nullref;

    for (auto instr : ordered) {
      auto ref = ctx.getInstrs().resolve(instr);
      it.insertPrev(ref);
      if (ref.isOpc(OP_UNYIELD)) {
        assert(!unyieldInstr);
        unyieldInstr = ref;
      }
    }

    // make sure unyield is first instr again if it exists
    if (unyieldInstr) {
      ctx.getCFG()[unyieldInstr].erase();
      block.begin().insertPrev(unyieldInstr);
    }
    assert(block.size() == ordered.size());
  }

  void runOnModule(ModuleIRef mod) {
    map.clear();
    map.resize(ctx.getInstrs().numIDs());
    for (auto proc : mod.procs()) {
      auto blocks = getSCFBlocksPreorder(proc.block());
      for (auto block : blocks)
        runOnBlock(block);
    }
  }

public:
  void run() {
    for (auto mod : ctx.activeModules()) {
      runOnModule(mod.iref());
    }
  }
  auto make(HWContext &ctx) { return OrderInstrsPass(ctx); }
  explicit OrderInstrsPass(HWContext &ctx) : ctx(ctx) {}
};

}; // namespace dyno
