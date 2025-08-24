#pragma once

#include "dyno/CFG.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "op/IDs.h"
namespace dyno {

class ControlFlowAnalysis {

  HWContext &ctx;

  template <typename T>
  static auto findFirstShared(ArrayRef<T> a, ArrayRef<T> b) {
    for (size_t i = 0; i < a.size(); i++)
      for (size_t j = 0; j < b.size(); j++)
        if (a[i] == b[j])
          return std::pair<size_t, size_t>(i, j);
    dyno_unreachable("no common element");
  }

  template <typename T>
  static auto findFirstSharedInstr(ArrayRef<T> a, ArrayRef<T> b) {
    for (size_t i = 0; i < a.size(); i++)
      for (size_t j = 0; j < b.size(); j++)
        if (a[i].instr() == b[j].instr())
          return std::pair<size_t, size_t>(i, j);
    dyno_unreachable("no common element");
  }

public:
  explicit ControlFlowAnalysis(HWContext &ctx) : ctx(ctx) {}

  void buildDepStack(SmallVecImpl<OperandRef> &stack, BlockRef instrBl) {
    auto buildStack = [&](BlockRef block, SmallVecImpl<OperandRef> &stack) {
      while (true) {
        auto instr = block.defI();
        stack.emplace_back(*block.def());
        auto parent = HWInstrRef{instr}.parentBlock(ctx);
        if (!parent)
          break;
        block = parent;
      }
    };

    buildStack(instrBl, stack);
  }

  BlockRef findSharedParentBlock(BlockRef instrBl, BlockRef otherBl) {
    if (otherBl == instrBl)
      return otherBl;

    SmallVec<OperandRef, 4> instrStack;
    buildDepStack(instrStack, instrBl);
    SmallVec<OperandRef, 4> otherStack;
    buildDepStack(otherStack, otherBl);

    auto [otherIdx, instrIdx] =
        findFirstShared(ArrayRef{otherStack}, ArrayRef{instrStack});

    BlockRef block = otherStack[otherIdx]->as<BlockRef>();
    assert(block == instrStack[instrIdx]->as<BlockRef>());

    if (block.defI().isOpc(OP_SWITCH))
      block = HWInstrRef{block.defI()}.parentBlock(ctx);

    return block;
  }

  bool isTriviallyMutuallyExclusive(InstrRef instr, InstrRef other) {
    BlockRef otherBl = HWInstrRef{other}.parentBlock(ctx);
    BlockRef instrBl = HWInstrRef{instr}.parentBlock(ctx);
    if (instrBl == otherBl)
      return false;

    SmallVec<OperandRef, 4> instrStack;
    buildDepStack(instrStack, instrBl);
    SmallVec<OperandRef, 4> otherStack;
    buildDepStack(otherStack, otherBl);

    auto [instrIdx, otherIdx] =
        findFirstSharedInstr(ArrayRef{instrStack}, ArrayRef{otherStack});

    auto instrOp = instrStack[instrIdx];
    auto otherOp = otherStack[otherIdx];

    auto parentInstr = instrOp.instr();
    assert(parentInstr == otherOp.instr());

    if (parentInstr.isOpc(OP_SWITCH))
      return true;

    if (instrOp != otherOp && instrOp.instr().isOpc(OP_IF))
      return true;

    return false;
  }
};

}; // namespace dyno
