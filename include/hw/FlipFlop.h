#pragma once

#include "dyno/Constant.h"
#include "hw/HWInstr.h"
#include "support/Utility.h"
namespace dyno {

class FlipFlopIRef : public OpcodeInstrRef<HWInstrRef, HW_FLIP_FLOP> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

private:
  constexpr static uint numBaseOperands = 4;
  constexpr static uint numClkEnOperands = 2;
  constexpr static uint numRstOperands = 3;

public:
  RegisterRef clk() { return operand(0)->as<RegisterRef>(); }
  uint32_t clkPolarity() { return operand(1)->as<ConstantRef>().getExactVal(); }
  RegisterRef d() { return operand(2)->as<RegisterRef>(); }
  RegisterRef q() { return operand(3)->as<RegisterRef>(); }

  bool hasClkEn() const {
    return ((getNumOperands() - numBaseOperands) % numRstOperands != 0);
  }
  uint numRsts() const {
    return ((getNumOperands() - numBaseOperands -
             (hasClkEn() ? numClkEnOperands : 0)) /
            numRstOperands);
  }

private:
  uint rstBase(uint i = 0) const {
    return (hasClkEn() ? (numBaseOperands + numClkEnOperands)
                       : numBaseOperands) +
           i * numRstOperands;
  }
  uint clkEnBase() const { return numBaseOperands; }

public:
  RegisterRef rst(uint i = 0) {
    return operand(rstBase(i) + 0)->as<RegisterRef>();
  }
  uint32_t rstPolarity(uint i = 0) {
    return operand(rstBase(i) + 1)->as<ConstantRef>().getExactVal();
  }
  ConstantRef rstVal(uint i = 0) {
    return operand(rstBase(i) + 2)->as<ConstantRef>();
  }

  RegisterRef clkEn() { return operand(clkEnBase() + 0)->as<RegisterRef>(); }
  uint32_t clkEnPolarity() {
    return operand(clkEnBase() + 1)->as<ConstantRef>().getExactVal();
  }

  void flipPolarity(OperandRef ref) {
    auto pol = (ref + 1);
    assert(pol->is<ConstantRef>() && pol->as<ConstantRef>().getNumBits() == 1);
    pol.replace(ConstantRef::fromBool(!pol->as<ConstantRef>().getExactVal()));
  }
  uint32_t getPolarity(OperandRef ref) {
    auto pol = (ref + 1);
    assert(pol->is<ConstantRef>() && pol->as<ConstantRef>().getNumBits() == 1);
    return pol->as<ConstantRef>().getExactVal();
  }

  enum UseType {
    CLOCK,
    D,
    Q,
    ENABLE,
    RESET_BEGIN,
    RESET_0 = RESET_BEGIN,
    RESET_1 = RESET_BEGIN + numRstOperands,
    RESET_2 = RESET_BEGIN + 2 * numRstOperands,
    RESET_END = RESET_BEGIN +
                ((InstrRef::max_others - numBaseOperands - numClkEnOperands) /
                 numRstOperands)
  };
  UseType classifyUse(OperandRef use) {
    assert(use.instr() == *this);
    uint idx = use - *other_begin();
    if (idx == 0)
      return CLOCK;
    if (idx == 2)
      return D;
    if (idx == 3)
      return Q;
    if (hasClkEn() && idx == clkEnBase())
      return ENABLE;
    if (numRsts() > 0 && (idx - rstBase()) % numRstOperands == 0)
      return UseType(RESET_BEGIN + ((idx - rstBase()) / numRstOperands));
    dyno_unreachable("invalid operand");
  }
};

}; // namespace dyno
