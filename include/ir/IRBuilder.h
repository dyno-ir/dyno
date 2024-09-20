#pragma once

#include "ir/IR.h"
#include <cassert>

class InstrPtr {
public:
  InstrPtr(Instr *instr) : instr(instr) { assert(instr); }

  Instr &operator*() { return *instr; }
  Instr *operator->() { return instr; }
  Instr *get() { return instr; }

protected:
  Instr *instr;
};

class PhiInstrPtr : public InstrPtr {
public:
  PhiInstrPtr(Instr &instr) : InstrPtr(&instr) {}
  PhiInstrPtr(Instr *instr) : InstrPtr(instr) {
    assert(instr && instr->getKind() == Instr::PHI);
  }

  void setupPredecessors(unsigned numPreds) {
    instr->getChainOperand().opChain().allocate(2 * numPreds);
  }

  unsigned getNumPredecessors() {
    return instr->getChainOperand().opChain().getCapacity() / 2;
  }

  CFGBlock &getPredecessorBlock(unsigned n) {
    return instr->getChainOperand().opChain().getOperand(n * 2 + 1).block();
  }
  Operand &getPredecessorUse(unsigned n) {
    return instr->getChainOperand().opChain().getOperand(n * 2);
  }
  Operand &getPredecessorDef(unsigned n) {
    return getPredecessorUse(n).ssaUse().getDef();
  }

  bool isComplete() { return instr->getChainOperand().opChain().isAllocated(); }

  void setPredecessor(unsigned n, Operand &def, CFGBlock &pred) {
    OperandChain &chain = instr->getChainOperand().opChain();
    chain.getOperand(n * 2).emplace<Operand::SSA_USE>(instr, def);
    chain.getOperand(n * 2 + 1).emplace<Operand::BLOCK>(instr, &pred);
  }
};

class AllocaInstrPtr : public InstrPtr {
  AllocaInstrPtr(Instr *instr) : InstrPtr(instr) {
    assert(instr->getKind() == Instr::ALLOCA);
  }
};

template <typename Invariants>
class IRBuilder : public IRManipulator<Invariants> {
  using IRMan = IRManipulator<Invariants>;
  using IRMan::createInstr;

public:
  IRBuilder() = default;
  IRBuilder(Function &func,
               IntrusiveListNode<CFGBlock> &insertionPoint)
      : IRMan(func), insertionPoint(&insertionPoint) {}
  IRBuilder(Function &func,
               IntrusiveListNode<CFGBlock> *insertionPoint)
      : IRMan(func), insertionPoint(insertionPoint) {}
  IRBuilder(Function &func, CFGBlock &insertionPoint)
      : IRMan(func), insertionPoint(&insertionPoint.getSentryEnd()) {}

  void setInsertionPoint(IntrusiveListNode<CFGBlock> *node) {
    insertionPoint = node;
  }
  void setInsertionPoint(IntrusiveListNode<CFGBlock> &node) {
    insertionPoint = &node;
  }
  void setInsertionPoint(CFGBlock &block) {
    insertionPoint = &block.getSentryEnd();
  }

  Instr *getLastInstr() { return lastInstr; }

  CFGBlock &getBlock() {
    assert(insertionPoint);
    return insertionPoint->getParent();
  }

  Operand &getDef(unsigned n = 0) {
    assert(lastInstr);
    return lastInstr->getDef(n);
  }

  Instr &emit(Instr &instr) {
    assert(insertionPoint);
    insertionPoint->insertPrev(instr.cfg());
    lastInstr = &instr;
    return instr;
  }

  Instr &emitInstr(unsigned kind) { return emit(createInstr(kind)); }

  Instr &emitInstr(unsigned kind, unsigned cap) {
    return emit(createInstr(kind, cap));
  }

  Instr &emitExternRef(Reg dst, ExternSSADef &def) {
    Instr &i = emitInstr(Instr::REF_EXTERN, 2);
    i.emplaceOperand<Operand::REG_DEF>(dst);
    i.emplaceOperand<Operand::SSA_USE>(def.operand());
    return i;
  }

  Instr &emitCopy(Reg dst, Reg src) {
    Instr &i = emitInstr(Instr::COPY, 2);
    i.emplaceOperand<Operand::REG_DEF>(dst);
    i.emplaceOperand<Operand::REG_USE>(src);
    return i;
  }

protected:
  IntrusiveListNode<CFGBlock> *insertionPoint = nullptr;
  Instr *lastInstr = nullptr;
};
