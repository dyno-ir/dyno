#pragma once

#include "dyno/Instr.h"
#include "dyno/RefUnion.h"
#include "op/IDs.h"

namespace dyno {

class ArithInstrRef
    : public OpcodeInstrRef<InstrRef, OP_ADD, OP_SUB, OP_MUL, OP_UDIV, OP_SDIV,
                            OP_UMOD, OP_SMOD> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  bool isDiv() const { return isOpc(OP_UDIV, OP_SDIV); }
  bool isMod() const { return isOpc(OP_UMOD, OP_SMOD); }
  bool isNeg() const { return isOpc(OP_SUB) && getNumOthers() == 1; }
};

#define COMPARISON_OP(op, ...) , op
class ComparisonInstrRef
    : public OpcodeInstrRef<InstrRef FOR_OP_ALL_COMPARE_OPS(COMPARISON_OP)> {
#undef COMPARISON_OP
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  bool isEquality() const {
    return isOpc(OP_ICMP_EQ, OP_ICMP_NE) || isVerilogEquality();
  }
  bool isVerilogEquality() const {
    switch (*getDialectOpcode()) {
#define COMPARISON_CASE(op, ...) case *op:
      FOR_OP_SPECIAL_COMPARE_OPS(COMPARISON_CASE)
#undef COMPARISON_CASE
      return true;
    default:
      return false;
    }
  }
  bool isSigned() const {
    return isOpc(OP_ICMP_SLT, OP_ICMP_SLE, OP_ICMP_SGT, OP_ICMP_SGE);
  }
  DialectOpcode getSignedOpcode() const {
    switch (*getDialectOpcode()) {
    case *OP_ICMP_ULT:
      return OP_ICMP_SLT;
    case *OP_ICMP_ULE:
      return OP_ICMP_SLE;
    case *OP_ICMP_UGT:
      return OP_ICMP_SGT;
    case *OP_ICMP_UGE:
      return OP_ICMP_SGE;
    default:
      return getDialectOpcode();
    }
  }
};

class BitwiseInstrRef
    : public OpcodeInstrRef<InstrRef, OP_AND, OP_OR, OP_XOR, OP_NOT> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;
};

class ShiftInstrRef : public OpcodeInstrRef<InstrRef, OP_SLL, OP_SRL, OP_SRA> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  bool isLogical() const { return isOpc(OP_SLL, OP_SRL); }

  DialectOpcode getSignedOpcode() const {
    return isOpc(OP_SRL) ? DialectOpcode(OP_SRA) : getDialectOpcode();
  }
};

using OperatorInstrRef = FatRefUnion<ArithInstrRef, ComparisonInstrRef,
                                     BitwiseInstrRef, ShiftInstrRef>;

} // namespace dyno
