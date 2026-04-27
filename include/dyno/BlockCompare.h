#pragma once
#include "dyno/CFG.h"
#include "dyno/Instr.h"
#include "support/DenseMap.h"

namespace dyno {
class BlockCompare {
  DenseMap<DynObjRef, ObjID> translateMap;

private:
  std::optional<std::pair<InstrRef, InstrRef>>
  compareInstrsImpl(InstrRef instrL, InstrRef instrR) {
#define FAIL_CUR return std::make_pair(instrL, instrR)

    if (instrL.getDialectOpcode() != instrR.getDialectOpcode() ||
        instrL.getNumOperands() != instrR.getNumOperands() ||
        instrL.getNumDefs() != instrR.getNumDefs())
      FAIL_CUR;

    for (auto [opA, opB] : Range{instrL.defs()}.zip(instrR.defs())) {
      if (opA->thin().getType() != opB->thin().getType())
        FAIL_CUR;
      translateMap.insert(opB->thin(), opA->thin().getObjID());

      if (opA->thin().getType() == CORE_BLOCK) {
        if (auto res =
                compareBlocksImpl(opA->as<BlockRef>(), opB->as<BlockRef>()))
          return res;
      }
    }

    for (auto [opA, opB] : Range{instrL.others()}.zip(instrR.others())) {
      if (opA->thin().getType() != opB->thin().getType())
        FAIL_CUR;
      auto bToA = translateMap.find(opB->thin());

      if (opA->thin().getObjID() !=
          (bToA != translateMap.end() ? bToA.val() : opB->thin().getObjID()))
        FAIL_CUR;
    }
#undef FAIL_CUR
  return std::nullopt;
  }

  std::optional<std::pair<InstrRef, InstrRef>> compareBlocksImpl(BlockRef lhs,
                                                                 BlockRef rhs) {
    auto lhsIt = lhs.begin();
    auto rhsIt = rhs.begin();

    while (lhsIt != lhs.end() && rhsIt != rhs.end()) {
      if (auto err = compareInstrsImpl(*lhsIt, *rhsIt))
        return err;
      ++lhsIt;
      ++rhsIt;
    }

    if (lhsIt == lhs.end() && rhsIt == rhs.end())
      return std::nullopt;
    return std::make_pair(*lhsIt, *rhsIt);
  }

public:
  auto compareBlocks(BlockRef lhs, BlockRef rhs) {
    translateMap.clear();
    return compareBlocksImpl(lhs, rhs);
  }
  auto compareInstrs(InstrRef lhs, InstrRef rhs) {
    translateMap.clear();
    return compareInstrsImpl(lhs, rhs);
  }

  BlockCompare() = default;
};
}; // namespace dyno
