#include "ir/IRPrinter.h"

#include <cassert>
#include <iostream>
#include <optional>

#include "ir/IR.h"
#include "support/RTTI.h"
#include "support/Utility.h"

const IRInfoID NumberingIRVisitor::ID = nullptr;
const IRInfoID PrintIRVisitor::ID = nullptr;

void PrintIRVisitor::printNumberedDef(Operand &op) {
  if (auto n = numbering.getNum(op)) {
    std::cout << "%" << *n;
  } else {
    switch (op.getKind()) {
    case Operand::EMPTY:
      std::cout << "empty(";
      break;
    case Operand::SSA_DEF_TYPE:
      std::cout << "unnamed(";
      break;
    case Operand::SSA_DEF_EXTERN: {
      ExternSSADef &def = op.ssaDefExtern();
      if (def.isGlobal()) {
        std::cout << "global(";
        std::cout << def.global().getName();
      } else if (auto *d = as_dyn<MemoryAccessDef>(def)) {
        std::cout << "mem(" << d->getSize() << "," << d->getAlign().getExp();
      } else if (auto *d = as_dyn<FrameDef>(def)) {
        std::cout << "frame(" << d->getId();
      } else {
        std::cout << "unknown(";
      }
      break;
    }
    default:
      assert(false);
      break;
    }
    std::cout << ")";
  }
}

void PrintIRVisitor::printSSAType(SSAType &type) {
  std::cout << SSAType::kindName(type.getKind());
  if (type.getKind() == SSAType::INT) {
    std::cout << static_cast<IntSSAType &>(type).getBits();
  }
}

void PrintIRVisitor::printReg(Reg reg) {
  if (reg.isVReg()) {
    std::cout << "@" << reg.getIdx();
    return;
  }
  if (arch) {
    const ArchReg *archReg = arch->getArchReg(reg);
    if (archReg) {
      std::cout << archReg->name;
      return;
    }
  }
  std::cout << "reg(" << reg.getNum() << ")";
}

void PrintIRVisitor::printRegClass(unsigned kind) {
  if (arch) {
    const ArchRegClass *archRegClass = arch->getArchRegClass(kind);
    if (archRegClass) {
      std::cout << archRegClass->name;
      return;
    }
  }
  std::cout << "RC" << kind;
}

void PrintIRVisitor::visitProgram(Program &prog) {
  for (auto &f : prog.functions()) {
    std::cout << "---\n";
    dispatch(f);
    std::cout << "---\n";
  }
}

void PrintIRVisitor::visitFunction(Function &func) {
  numbering.dispatch(func);
  for (auto &b : func) {
    std::cout << "%" << numbering.getNum(b.operand()).value() << ":\n";
    dispatch(b);
  }
}

void PrintIRVisitor::visitBlock(CFGBlock &block) {
  for (auto &i : block) {
    dispatch(i);
    std::cout << "\n";
  }
}

void PrintIRVisitor::visitInstr(Instr &instr) {
  preInstrCallback(instr);
  const ArchInstr *archInstr =
      arch ? arch->getArchInstr(instr.getKind()) : nullptr;
  const char *name =
      archInstr ? archInstr->name : Instr::kindName(instr.getKind());
  std::cout << name;
  for (auto &op : instr) {
    std::cout << " ";
    dispatch(op);
  }
  if (instr.isVariadic()) {
    std::cout << " ";
    dispatch(instr.getChainOperand());
  }
}

void PrintIRVisitor::visitOperand(Operand &op) {
  switch (op.getKind()) {
  case Operand::EMPTY:
    std::cout << "Empty";
    break;
  case Operand::IMM32:
    std::cout << "imm32(" << op.imm32() << ")";
    break;
  case Operand::SSA_DEF_TYPE:
    std::cout << "def(%" << numbering.defNum(op) << ",";
    printSSAType(op.ssaDef().type());
    std::cout << ")";
    break;
  case Operand::SSA_USE:
    printNumberedDef(op.ssaUse().getDef());
    break;
  case Operand::REG_USE:
    printReg(op.reg());
    break;
  case Operand::REG_DEF:
    std::cout << "def(";
    printReg(op.reg());
    std::cout << ")";
    break;
  case Operand::TYPE:
    printSSAType(op.type());
    break;
  case Operand::BLOCK:
    printNumberedDef(op.block().operand());
    break;
  case Operand::OP_CHAIN: {
    bool first = true;
    std::cout << "[";
    for (auto &o : op.opChain()) {
      if (first) {
        first = false;
      } else {
        std::cout << " ";
      }
      dispatch(o);
    }
    std::cout << "]";
    break;
  }
  case Operand::BRCOND:
    std::cout << BrCond::kindName(op.brCond().getKind());
    break;
  case Operand::MINT:
    std::cout << "MInt(" << op.mInt().getBits() << ","
              << op.mInt().getMWordZext() << ")";
    break;
  default:
    UNREACHABLE("Illegal operand kind");
  }
}

void NumberingIRVisitor::visitFunction(Function &func) {
  for (auto &block : func) {
    defNum(block.operand());
  }
  for (auto &block : func) {
    dispatch(block);
  }
}

void NumberingIRVisitor::visitOperandSSADef(Operand &op) { defNum(op); }

unsigned NumberingIRVisitor::defNum(Operand &op) {
  auto [it, succ] = ssaDefs.insert(std::make_pair(&op, nextSSADefNum));
  if (succ) {
    ++nextSSADefNum;
  }
  return it->second;
}

std::optional<unsigned> NumberingIRVisitor::getNum(Operand &op) {
  auto it = ssaDefs.find(&op);
  if (it != ssaDefs.end()) {
    return it->second;
  }
  return std::nullopt;
}
