#pragma once

#include "ir/IRBuilder.h"

template <typename Invariants>
class SSAIRBuilder : public IRBuilder<Invariants> {
  using IRBuilder = IRBuilder<Invariants>;

public:
  using IRBuilder::emitInstr;
  using IRBuilder::IRBuilder;

  Instr &emitConstInt(MInt val) {
    Instr &i = emitInstr(Instr::CONST_INT, 2);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(IntSSAType::get(val.getBits()));
    i.emplaceOperand<Operand::MINT>(val);
    return i;
  }

  Instr &emitBinop(Instr::Kind kind, Operand &lhs, Operand &rhs) {
    assert(lhs.ssaDef().type() == rhs.ssaDef().type());
    Instr &i = emitInstr(kind, 3);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(lhs.ssaDef().type());
    i.emplaceOperand<Operand::SSA_USE>(lhs);
    i.emplaceOperand<Operand::SSA_USE>(rhs);
    return i;
  }

  Instr &emitShiftLeft(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::SL_L, lhs, rhs);
  }
  Instr &emitShiftRightLogical(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::SR_L, lhs, rhs);
  }
  Instr &emitShiftRightArith(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::SR_A, lhs, rhs);
  }

  PhiInstrPtr emitPhi(SSAType &type) {
    Instr &i = emitInstr(Instr::PHI);
    i.allocateVariadicOperands(1);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    return i;
  }

  Operand &emitUndefined(SSAType &type) {
    Instr &i = emitInstr(Instr::UNDEFINED, 1);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    return i.getDef();
  }

  Instr &emitExt(Instr::Kind kind, SSAType &type, Operand &val) {
    assert(Instr::kindIsArtifact(kind));
    assert(val.ssaDef().type().getKind() == SSAType::INT &&
           type.getKind() == SSAType::INT);
    assert(static_cast<IntSSAType &>(type).getBits() >
           static_cast<IntSSAType &>(val.ssaDef().type()).getBits());
    Instr &i = emitInstr(kind, 2);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    i.emplaceOperand<Operand::SSA_USE>(val);
    return i;
  }

  Instr &emitTrunc(SSAType &type, Operand &val) {
    assert(val.ssaDef().type().getKind() == SSAType::INT &&
           type.getKind() == SSAType::INT);
    assert(static_cast<IntSSAType &>(type).getBits() <
           static_cast<IntSSAType &>(val.ssaDef().type()).getBits());
    Instr &i = emitInstr(Instr::TRUNC, 2);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    i.emplaceOperand<Operand::SSA_USE>(val);
    return i;
  }

  Instr &emitCopy(Operand &val) {
    assert(val.isSSARegDef());
    Instr &i = emitInstr(Instr::COPY, 2);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(val.ssaDef().type());
    i.emplaceOperand<Operand::SSA_USE>(val);
    return i;
  }

  Instr &emitCopy(Reg dst, Operand &src) {
    assert(src.isSSARegDef());
    assert(dst);
    Instr &i = emitInstr(Instr::COPY, 2);
    i.emplaceOperand<Operand::REG_DEF>(dst);
    i.emplaceOperand<Operand::SSA_USE>(src);
    return i;
  }

  Instr &emitCopy(SSAType &type, Reg src) {
    assert(src);
    Instr &i = emitInstr(Instr::COPY, 2);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    i.emplaceOperand<Operand::REG_USE>(src);
    return i;
  }

  Instr &emitExtOrTrunc(Instr::Kind kind, SSAType &type, Operand &val) {
    assert(val.ssaDef().type().getKind() == SSAType::INT &&
           type.getKind() == SSAType::INT);
    unsigned dstBits = static_cast<IntSSAType &>(type).getBits();
    unsigned srcBits = static_cast<IntSSAType &>(val.ssaDef().type()).getBits();
    if (dstBits < srcBits) {
      return emitTrunc(type, val);
    } else if (dstBits > srcBits) {
      return emitExt(kind, type, val);
    } else {
      return emitCopy(val);
    }
  }

  Instr &emitCall(Function &func, std::span<Operand *> args) {
    assert(args.size() == func.paramTypes.size() &&
           "Invalid number of arguments");
    Instr &i =
        emitInstr(Instr::CALL, func.returnTypes.size() + 1 + args.size());
    for (auto *ty : func.returnTypes) {
      assert(ty);
      i.emplaceOperand<Operand::SSA_DEF_TYPE>(*ty);
    }
    i.emplaceOperand<Operand::SSA_USE>(func.operand());
    unsigned argNum = 0;
    for (auto *arg : args) {
      assert(arg && arg->ssaDef().type() == *func.paramTypes[argNum]);
      i.emplaceOperand<Operand::SSA_USE>(*arg);
      ++argNum;
    }
    return i;
  }

  Instr &emitReturn(Operand &val) {
    Instr &i = emitInstr(Instr::RET, 1);
    i.emplaceOperand<Operand::SSA_USE>(val);
    return i;
  }
  Instr &emitReturn() {
    Instr &i = emitInstr(Instr::RET);
    return i;
  }

  Operand &emitParamRef(SSAType &ty) {
    Instr &i = emitInstr(Instr::REF_PARAM, 1);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(ty);
    return i.getDef();
  }

  Operand &emitExternRef(SSAType &type, ExternSSADef &def) {
    Instr &i = emitInstr(Instr::REF_EXTERN, 2);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    i.emplaceOperand<Operand::SSA_USE>(def.operand());
    return i.getDef();
  }

  Instr &emitLoad(SSAType &type, Operand &addr, MemoryAccessDef &memAccess) {
    Instr &i = emitInstr(Instr::LOAD, 3);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(type);
    i.emplaceOperand<Operand::SSA_USE>(addr);
    i.emplaceOperand<Operand::SSA_USE>(memAccess.operand());
    return i;
  }

  Instr &emitStore(Operand &val, Operand &addr, MemoryAccessDef &memAccess) {
    Instr &i = emitInstr(Instr::STORE, 3);
    i.emplaceOperand<Operand::SSA_USE>(val);
    i.emplaceOperand<Operand::SSA_USE>(addr);
    i.emplaceOperand<Operand::SSA_USE>(memAccess.operand());
    return i;
  }

  Instr &emitCmp(BrCond cond, Operand &lhs, Operand &rhs) {
    assert(lhs.ssaDef().type() == rhs.ssaDef().type());
    Instr &i = emitInstr(Instr::CMP, 4);
    i.emplaceOperand<Operand::SSA_DEF_TYPE>(IntSSAType::get(1));
    i.emplaceOperand<Operand::BRCOND>(cond);
    i.emplaceOperand<Operand::SSA_USE>(lhs);
    i.emplaceOperand<Operand::SSA_USE>(rhs);
    return i;
  }

  Instr &emitBr(CFGBlock &dst) {
    Instr &i = emitInstr(Instr::BR, 1);
    i.emplaceOperand<Operand::SSA_USE>(dst.operand());
    return i;
  }

  Instr &emitNot(Operand &val) {
    Instr &i =
        emitConstInt(MInt::negOne(val.ssaDef().type().intType().getBits()));
    return emitXor(val, i.getDef());
  }

  Instr &emitNeg(Operand &val) {
    Instr &i =
        emitConstInt(MInt::zero(val.ssaDef().type().intType().getBits()));
    return emitSub(i.getDef(), val);
  }

  Instr &emitBrCond(Operand &cond, CFGBlock &dstTrue, CFGBlock &dstFalse) {
    assert(cond.ssaDef().type() == IntSSAType::get(1));
    Instr &i = emitInstr(Instr::BR_COND, 3);
    i.emplaceOperand<Operand::SSA_USE>(cond);
    i.emplaceOperand<Operand::SSA_USE>(dstTrue.operand());
    i.emplaceOperand<Operand::SSA_USE>(dstFalse.operand());
    return i;
  }

  Instr &emitAdd(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::ADD, lhs, rhs);
  }
  Instr &emitSub(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::SUB, lhs, rhs);
  }
  Instr &emitMul(Operand &lhs, Operand &rhs, bool isSigned) {
    return emitBinop(isSigned ? Instr::MUL_S : Instr::MUL_U, lhs, rhs);
  }
  Instr &emitAnd(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::AND, lhs, rhs);
  }
  Instr &emitOr(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::OR, lhs, rhs);
  }
  Instr &emitXor(Operand &lhs, Operand &rhs) {
    return emitBinop(Instr::XOR, lhs, rhs);
  }
};
