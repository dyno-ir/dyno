#pragma once

#include "ir/Arch.h"
#include "ir/IR.h"
#include "ir/IRPass.h"
#include "ir/IRVisitor.h"
#include <functional>
#include <optional>
#include <unordered_map>

class NumberingIRVisitor : public IRVisitor<NumberingIRVisitor> {
  friend class IRVisitor;

public:
  static const IRInfoID ID;

  unsigned defNum(Operand &op);
  std::optional<unsigned> getNum(Operand &op);

protected:
  void visitFunction(Function &func);
  void visitOperandSSADef(Operand &op);

private:
  std::unordered_map<Operand *, unsigned> ssaDefs;
  unsigned nextSSADefNum = 0;
};

class PrintIRVisitor : public IRVisitor<PrintIRVisitor> {
  friend class IRVisitor;

public:
  using callback_t = void(Instr &);
  static const IRInfoID ID;

  PrintIRVisitor(Arch *arch) : arch(arch) {}

  void printNumberedDef(Operand &op);
  void printSSAType(SSAType &type);
  void printReg(Reg reg);
  void printRegClass(unsigned kind);

  NumberingIRVisitor &getNumbering() { return numbering; }

  void setPreInstrCallback(std::function<callback_t> cb) {
    preInstrCallback = cb;
  }

protected:
  void visitProgram(Program &prog);
  void visitFunction(Function &func);
  void visitBlock(CFGBlock &block);
  void visitInstr(Instr &instr);
  void visitOperand(Operand &op);

  NumberingIRVisitor numbering;
  Arch *arch;

  std::function<callback_t> preInstrCallback = [](Instr &) {};
};

class IRPrinterPass : public IRPass<Function> {
public:
  const char *name() { return "IRPrinterPass"; }

  void advertise(IRInfo<Function> &info) { info.advertise<PrintIRVisitor>(); }

  void invalidate(IRInfo<Function> &) {}

  void run(Function &obj, IRInfo<Function> &info) {
    irVisitor = PrintIRVisitor(info.getArchUnchecked());
    irVisitor.dispatch(obj);
    info.publish(irVisitor);
    info.preserveAll();
  }

private:
  PrintIRVisitor irVisitor{nullptr};
};
