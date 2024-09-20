#include "ir/IR.h"
#include <cassert>
#include <utility>

CFGBlock &Operand::getParentBlock() const { return getParent().cfg().getParent(); }

void OperandChain::allocate(unsigned cap) {
  assert(!operands);
  operands = new Operand[cap];
  capacity = cap;
}

void OperandChain::deallocate() {
  delete[] operands;
  capacity = 0;
}

Operand &OperandChain::getOperand(unsigned n) {
  assert(n < capacity);
  return operands[n];
}

OperandChain::iterator OperandChain::begin() { return operands; }

OperandChain::iterator OperandChain::end() { return operands + capacity; }

IntrusiveListNode<CFGBlock> &CFGBlock::getFirstNonPhiSentry() {
  for (auto &instr : instrs()) {
    if (!instr.isPhi()) {
      return instr.cfg();
    }
  }
  return getSentryEnd();
}

Function *Program::getFunction(std::string_view name) {
  auto it = functionIndex.find(name);
  if (it == functionIndex.end()) {
    return nullptr;
  }
  return it->second;
}

Function *Program::createFunction(std::string name) {
  auto func = std::make_unique<Function>(*this, std::move(name));
  auto [it, succ] = functionIndex.try_emplace(func->getName(), func.get());
  if (!succ) {
    return nullptr;
  }
  return functionStorage.emplace_back(std::move(func)).get();
}

void RegDefUse::insertDefUse() {
  if (!contentReg.isVReg()) {
    linkSelf();
    return;
  }
  RegDefUseRoot &root =
      operand().getParentBlock().getParent().getRegInfo().defUseRoot(
          contentReg);
  DefUseChain &chainRoot = root.operand().ssaDef();
  if (operand().isRegDef()) {
    chainRoot.insertPrev(*this);
  } else {
    chainRoot.insertNext(*this);
  }
}

void SSADef::replaceAllUses(Reg reg) {
  for (DefUseChain *curr = chNext; curr != this;) {
    DefUseChain &tmp = *curr;
    curr = curr->chNext;
    // TODO: direct conversion without UB!
    // tmp.contentReg = reg;
    // tmp.operand().kind = Operand::REG_USE;
    tmp.operand().data.emplace<Operand::REG_USE>(reg);
  }
}

void SSADef::replace(Reg reg) {
  replaceAllUses(reg);
  operand().data.emplace<Operand::REG_DEF>(reg);
}

const char *Instr::kindName(unsigned kind) {
  switch ((Kind)kind) {
  case CONST_INT:
    return "CONST_INT";
  case PHI:
    return "PHI";
  case ADD:
    return "ADD";
  case SUB:
    return "SUB";
  case MUL_U:
    return "MUL_U";
  case MUL_S:
    return "MUL_S";
  case DIV_U:
    return "DIV_U";
  case DIV_S:
    return "DIV_S";
  case SL_L:
    return "SL_L";
  case SR_L:
    return "SR_L";
  case SR_A:
    return "SR_A";
  case AND:
    return "AND";
  case OR:
    return "OR";
  case XOR:
    return "XOR";
  case BR:
    return "BR";
  case BR_COND:
    return "BR_COND";
  case CMP:
    return "CMP";
  case RET:
    return "RET";
  case CALL:
    return "CALL";
  case EXT_Z:
    return "EXT_Z";
  case EXT_S:
    return "EXT_S";
  case EXT_A:
    return "EXT_A";
  case CONV:
    return "CONV";
  case TRUNC:
    return "TRUNC";
  case COPY:
    return "COPY";
  case LOAD:
    return "LOAD";
  case STORE:
    return "STORE";
  case ALLOCA:
    return "ALLOCA";
  case REF_EXTERN:
    return "REF_EXTERN";
  case REF_PARAM:
    return "REF_PARAM";
  case EMPTY:
  case INSTR_START:
  case INSTR_END:
  case ARCH_INSTR:
    return "INVALID";
  case UNDEFINED:
    return "UNDEFINED";
    break;
  }
  return "UNKOWN";
}

const char *BrCond::kindName(Kind kind) {
  switch (kind) {
  case EQ:
    return "eq";
  case NE:
    return "ne";
  case LT:
    return "lt";
  case LTU:
    return "ltu";
  case LE:
    return "le";
  case LEU:
    return "leu";
  case GT:
    return "gt";
  case GTU:
    return "gtu";
  case GE:
    return "ge";
  case GEU:
    return "geu";
  }
  return "?";
}
