#pragma once
#include "dyno/Obj.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/Register.h"
#include "hw/analysis/SCFTraversal.h"
#include "support/Debug.h"
#include "support/DynBitSet.h"

namespace dyno {

// fixme: still needs mode to respect existing partial order. currently does not
// respect order of side effect instrs.
class OrderInstrsPass {
  HWContext &ctx;
  ObjMap<Instr, DynSymbSet<std::vector<uint64_t>, 2>> map;
  enum { PRE_MARK = 0, MARK = 1 };

public:
  struct Config {
    bool assertNoCircularDeps = true;
  };
  Config config;

private:
  void handleUse(BlockRef block, OperandRef use,
                 SmallVecImpl<OperandRef> &uses) {
    if (auto asWire = use->dyn_as<WireRef>()) {
      auto instr = asWire.getDefI();
      if (map[instr].at(MARK))
        return;
      if (ctx.getCFG()[instr].blockRef() != block) {
        assert(!instr.isOpc(OP_UNYIELD));
        dbgs() << "out of block:\n";
        HWPrinter print{dbgs()};
        print.printInstr(block.defI(), ctx);
        dbgs() << "in question:\n";
        print.printInstr(instr, ctx);
      }
      uses.emplace_back(instr);
    }
    // else if (auto asReg = use->dyn_as<RegisterRef>()) {
    //   if (!use.instr().isOpc(HW_LOAD))
    //     return;
    //   if (asReg.getNumUses() > 2)
    //     return;
    //   auto block = ctx.getCFG()[use.instr()].blockRef();
    //   for (auto regUse : asReg.uses()) {
    //     if (regUse.instr().isOpc(HW_STORE) &&
    //         ctx.getCFG()[regUse.instr()].blockRef() == block)
    //       uses.emplace_back(regUse.instr());
    //   }
    // }
  }

  void prioritzeUses(MutArrayRef<OperandRef> uses) {
    std::sort(uses.begin(), uses.end(), [](OperandRef lhs, OperandRef rhs) {
      // if (lhs->is<RegisterRef>() != rhs->is<RegisterRef>())
      //   return lhs->is<RegisterRef>();
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
      prioritzeUses(uses);
      for (auto use : uses)
        stack.emplace_back(use.instr());
    }
  }

  void runOnBlock(BlockRef block) {
    SmallVec<ObjRef<Instr>, 32> ordered;
    ordered.reserve(block.size());
    size_t i = 0;
    for (auto instr : block) {
      visit(block, instr, ordered);
      ++i;
    }
    // assert(ordered.size() == block.size() && block.size() == i);
    block.clear_unsafe();
    auto it = block.end();
    for (auto instr : ordered) {
      it.insertPrev(ctx.getInstrs().resolve(instr));
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
  explicit OrderInstrsPass(HWContext &ctx) : ctx(ctx) {}
};

}; // namespace dyno
