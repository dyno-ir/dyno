#pragma once

#include "dyno/CFG.h"
#include "dyno/Context.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "op/IDs.h"
#include <utility>
namespace dyno {

class ControlFlowAnalysis {

  Context &ctx;

  template <typename T> static auto findFirstShared(Range<T> a, Range<T> b) {
    // todo binary search
    auto itA = a.end() - 1;
    auto itB = b.end() - 1;
    assert(*itA == *itB && "no shared root?");
    do {
      --itA;
      --itB;
    } while (itA != a.begin() - 1 && itB != b.begin() - 1 && *itA == *itB);
    return std::make_pair(itA - a.begin() + 1, itB - b.begin() + 1);
  }

public:
  explicit ControlFlowAnalysis(Context &ctx) : ctx(ctx) {}

  void buildDepStack(SmallVecImpl<OperandRef> &stack, BlockRef block) {
    while (true) {
      auto instr = block.defI();
      stack.emplace_back(*block.def());
      auto parent = HWInstrRef{instr}.parentBlock(ctx);
      if (!parent)
        break;
      block = parent;
    }
  }
  auto buildDepStack(BlockRef block) {
    SmallVec<ObjRef<Block>, 16> stack;
    while (true) {
      auto instr = block.defI();
      stack.emplace_back(block);
      auto parent = HWInstrRef{instr}.parentBlock(ctx);
      if (!parent)
        break;
      block = parent;
    }
    return stack;
  }

  BlockRef findSharedParentBlock(BlockRef instrBl, BlockRef otherBl) {
    if (otherBl == instrBl)
      return otherBl;

    auto instrStack = buildDepStack(instrBl);
    auto otherStack = buildDepStack(otherBl);

    auto [otherIdx, instrIdx] =
        findFirstShared(Range{otherStack}, Range{instrStack});
    assert(otherStack[otherIdx] == instrStack[instrIdx]);

    BlockRef block = ctx.resolve(otherStack[otherIdx]);

    // switch is a special block, can only have case instrs.
    // return the first regular block above it.
    if (block.defI().isOpc(OP_SWITCH))
      block = HWInstrRef{block.defI()}.parentBlock(ctx);

    return block;
  }

  std::tuple<BlockRef, InstrRef, InstrRef>
  findSharedParentBlockAndInstrs(BlockRef instrBl, BlockRef otherBl) {
    assert(otherBl != instrBl);

    auto instrStack = buildDepStack(instrBl);
    auto otherStack = buildDepStack(otherBl);

    auto [otherIdx, instrIdx] =
        findFirstShared(Range{otherStack}, Range{instrStack});
    assert(otherStack[otherIdx] == instrStack[instrIdx]);

    BlockRef block = ctx.resolve(otherStack[otherIdx]);

    auto instrI = ctx.resolve(instrStack[instrIdx - 1]).defI();
    auto otherI = ctx.resolve(otherStack[otherIdx - 1]).defI();

    if (block.defI().isOpc(OP_SWITCH)) {
      return {HWInstrRef{block.defI()}.parentBlock(ctx), block.defI(),
              block.defI()};
    }

    return {block, instrI, otherI};
  }
};

}; // namespace dyno
