#pragma once

#include "op/StructuredControlFlow.h"
namespace dyno {

template <std::invocable<InstrRef> IFunc, std::invocable<BlockRef> BFunc>
inline void traverseSCFPreorder(IFunc ifunc, BFunc bfunc,
                                BlockRef currentBlock) {
  bfunc(currentBlock);
  for (auto instr : currentBlock) {
    ifunc(instr);
    switch (*instr.getDialectOpcode()) {
    case *OP_IF: {
      traverseSCFPreorder(ifunc, bfunc, instr.as<IfInstrRef>().getTrueBlock());
      traverseSCFPreorder(ifunc, bfunc, instr.as<IfInstrRef>().getFalseBlock());
      break;
    }
    case *OP_SWITCH: {
      for (auto caseInstr : instr.as<SwitchInstrRef>().block()) {
        traverseSCFPreorder(ifunc, bfunc, caseInstr.as<CaseInstrRef>().block());
      }
      break;
    }
    case *OP_DO_WHILE: {
      traverseSCFPreorder(ifunc, bfunc, instr.as<DoWhileInstrRef>().getBlock());
      break;
    }
    case *OP_WHILE: {
      traverseSCFPreorder(ifunc, bfunc,
                          instr.as<WhileInstrRef>().getCondBlock());
      traverseSCFPreorder(ifunc, bfunc,
                          instr.as<WhileInstrRef>().getBodyBlock());
      break;
    }
    case *OP_FOR: {
      traverseSCFPreorder(ifunc, bfunc, instr.as<ForInstrRef>().getBlock());
      break;
    }
    default:
      continue;
    }
  }
};

inline SmallVec<InstrRef, 64> getSCFInstrsPreorder(BlockRef block) {
  SmallVec<InstrRef, 64> rv;
  traverseSCFPreorder([&](InstrRef instr) { rv.emplace_back(instr); },
                      [](BlockRef) {}, block);
  return rv;
}

inline SmallVec<BlockRef, 16> getSCFBlocksPreorder(BlockRef block) {
  SmallVec<BlockRef, 16> rv;
  traverseSCFPreorder([](InstrRef) {},
                      [&](BlockRef block) { rv.emplace_back(block); }, block);
  return rv;
}

}; // namespace dyno
