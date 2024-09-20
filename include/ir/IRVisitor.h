#pragma once
#include "ir/IR.h"

template <class D> class IRVisitor {
public:
  void dispatch(Program *prog) {
    if (prog) {
      dispatch(*prog);
    }
  }
  void dispatch(Function *func) {
    if (func) {
      dispatch(*func);
    }
  }
  void dispatch(CFGBlock *block) {
    if (block) {
      dispatch(*block);
    }
  }
  void dispatch(Instr *instr) {
    if (instr) {
      dispatch(*instr);
    }
  }
  void dispatch(Program &prog) { impl().visitProgram(prog); }
  void dispatch(Function &func) { impl().visitFunction(func); }
  void dispatch(CFGBlock &block) { impl().visitBlock(block); }
  void dispatch(Instr &instr) {
    switch (instr.getKind()) {
    default:
      dispatchDefault(instr);
      break;
    }
  }
  void dispatch(Operand &op) {
    switch (op.getKind()) {
    case Operand::SSA_DEF_TYPE:
      impl().visitOperandSSADef(op);
      break;
    case Operand::SSA_USE:
      impl().visitOperandSSAUse(op);
      break;
    default:
      dispatchDefault(op);
      break;
    }
  }

protected:
  D &impl() { return static_cast<D &>(*this); }

  void dispatchDefault(Instr &instr) { impl().visitInstr(instr); }
  void dispatchDefault(Operand &op) { impl().visitOperand(op); }

  void visitProgram(Program &prog) {
    for (auto &f : prog.functions()) {
      dispatch(f);
    }
  }

  void visitFunction(Function &func) {
    for (auto &b : func) {
      dispatch(b);
    }
  }

  void visitBlock(CFGBlock &block) {
    for (auto it = block.begin(), itEnd = block.end(); it != itEnd;) {
      auto itTmp = it++;
      dispatch(*itTmp);
    }
  }

  void visitInstr(Instr &instr) {
    for (auto &op : instr) {
      dispatch(op);
    }
  }

  void visitOperand(Operand &) {}
  void visitOperandSSADef(Operand &op) { dispatchDefault(op); }
  void visitOperandSSAUse(Operand &op) { dispatchDefault(op); }
};
