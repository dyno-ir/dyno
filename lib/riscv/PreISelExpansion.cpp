#include "riscv/PreISelExpansion.h"
#include "ir/SSAIRBuilder.h"
#include "riscv/Arch.h"
#include <cassert>

namespace {

#include "riscv/InstrSelector.dsl.preISelExpansion.h"

IntSSAType &XTy = IntSSAType::get(riscv::XLEN);

void legalizeSSAUse(Instr::Kind ext, Operand &op, IntSSAType &expectedTy,
                    SSAIRBuilder &ir) {
  assert(op.isSSAUse());
  Operand &def = op.ssaUse().getDef();
  if (!def.isSSARegDef() || def.ssaDef().type().getKind() != SSAType::INT) {
    return;
  }
  IntSSAType &intTy = static_cast<IntSSAType &>(def.ssaDef().type());
  if (intTy.getBits() < expectedTy.getBits()) {
    ir.emitExt(ext, expectedTy, op.ssaUse().getDef());
    op.ssaUse().replace(ir.getDef());
  }
}

void legalizeSSADef(Operand &op, IntSSAType &expectedTy, SSAIRBuilder &ir) {
  assert(op.isSSADef());
  SSAType &ty = op.ssaDef().type();
  if (ty.getKind() != SSAType::INT) {
    return;
  }
  IntSSAType &intTy = static_cast<IntSSAType &>(ty);
  if (intTy.getBits() < expectedTy.getBits()) {
    op.ssaDef().setType(expectedTy);
    ir.emitTrunc(intTy, op);
    op.ssaDef().replaceAllUses(ir.getDef());
    ir.getLastInstr()->getOperand(1).ssaUse().replace(op);
  }
}
void legalizeOperand(Instr::Kind ext, Operand &op, SSAIRBuilder &preIr,
                     SSAIRBuilder &postIr) {
  if (op.isSSADef()) {
    legalizeSSADef(op, XTy, postIr);
  } else if (op.isSSAUse()) {
    legalizeSSAUse(ext, op, XTy, preIr);
  }
}
void legalizeOperands(Instr::Kind ext, Instr &instr,
                      std::initializer_list<Operand *> ops) {
  SSAIRBuilder preIr(instr);
  SSAIRBuilder postIr(instr.getNextNode());
  for (auto *op : ops) {
    legalizeOperand(ext, *op, preIr, postIr);
  }
}

void legalizeAllOperands(Instr::Kind ext, Instr &instr) {
  SSAIRBuilder preIr(instr);
  SSAIRBuilder postIr(instr.getNextNode());
  for (auto &op : instr) {
    legalizeOperand(ext, op, preIr, postIr);
  }
}

void legalizePhi(PhiInstrPtr phi) {
  SSAIRBuilder postIr(phi->getNextNode());
  legalizeSSADef(phi->getDef(), XTy, postIr);
  for (int i = 0, iEnd = phi.getNumPredecessors(); i < iEnd; ++i) {
    SSAIRBuilder preIr(phi.getPredecessorBlock(i).getLast());
    legalizeSSAUse(Instr::EXT_A, phi.getPredecessorUse(i), XTy, preIr);
  }
}

void legalizeCmp(Instr &instr) {
  SSAIRBuilder preIr(instr);
  SSAIRBuilder postIr(instr.getNextNode());
  Instr::Kind ext =
      instr.getOperand(1).brCond().isSigned() ? Instr::EXT_S : Instr::EXT_Z;
  legalizeSSADef(instr.getOperand(0), XTy, postIr);
  legalizeSSAUse(ext, instr.getOperand(2), XTy, preIr);
  legalizeSSAUse(ext, instr.getOperand(3), XTy, preIr);
}

void commuteConstant(Instr &instr, Operand &op1, Operand &op2) {
  Operand &op1Def = op1.ssaUse().getDef();
  Operand &op2Def = op2.ssaUse().getDef();
  if (op1Def.getParent().getKind() != Instr::CONST_INT) {
    return;
  }
  if (op2Def.getParent().getKind() == Instr::CONST_INT) {
    return;
  }
}

void commuteOperands(Operand &op1, Operand &op2) {
  Instr &instr = op1.getParent();
  assert(&instr == &op2.getParent());
  Operand &op1Def = op1.ssaUse().getDef();
  Operand &op2Def = op2.ssaUse().getDef();
  op1.emplace<Operand::SSA_USE>(&instr, op2Def);
  op2.emplace<Operand::SSA_USE>(&instr, op1Def);
}

bool isNormalBrCond(BrCond cond) {
  switch (cond.getKind()) {
  case BrCond::LE:
  case BrCond::LEU:
  case BrCond::GT:
  case BrCond::GTU:
    return false;
  default:
    return true;
  }
}

void normalizeCmp(Instr &instr) {
  BrCond cond = instr.getOperand(1).brCond();

  if (isNormalBrCond(cond))
    return;

  instr.getOperand(1).emplace<Operand::BRCOND>(&instr, cond.commute());
  commuteOperands(instr.getOperand(2), instr.getOperand(3));
}

// TODO: Implement legalization differently
// 1. Tag defs with needed legalizaion artifacts
// 2. Walk all defs and intelligently insert artifacts

} // namespace

namespace riscv {
bool PreISelExpansion::execute(Instr &instr) {
  switch (instr.getKind()) {
  case Instr::PHI:
    legalizePhi(PhiInstrPtr(&instr));
    break;
  case Instr::CONST_INT:
  case Instr::SL_L:
  case Instr::SR_L:
  case Instr::SR_A:
    legalizeAllOperands(Instr::EXT_Z, instr);
    break;
  case Instr::AND:
  case Instr::OR:
  case Instr::XOR:
  case Instr::ADD:
  case Instr::SUB:
    legalizeAllOperands(Instr::EXT_A, instr);
    break;
  case Instr::CMP:
    legalizeCmp(instr);
    normalizeCmp(instr);
    break;
  case Instr::BR_COND:
    legalizeOperands(Instr::EXT_Z, instr, {&instr.getOperand(0)});
    break;
  case Instr::LOAD:
  case Instr::STORE:
    legalizeAllOperands(Instr::EXT_A, instr);
    break;
  }
  if (DslPatExecutor(observer).dslExecutePat(instr)) {
    instr.deleteThis();
  }
  return true;
}
} // namespace riscv
