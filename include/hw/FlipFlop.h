#pragma once

#include "dyno/Constant.h"
#include "hw/HWInstr.h"
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
    return (getNumOperands() == numBaseOperands + numClkEnOperands) ||
           (getNumOperands() ==
            numBaseOperands + numClkEnOperands + numRstOperands);
  }
  bool hasRst() const {
    return (getNumOperands() == numBaseOperands + numRstOperands) ||
           (getNumOperands() ==
            numBaseOperands + numClkEnOperands + numRstOperands);
  }

private:
  uint rstBase() const {
    return hasClkEn() ? numBaseOperands + numClkEnOperands : numBaseOperands;
  }
  uint clkEnBase() const {
    return hasRst() ? numBaseOperands + numRstOperands : numBaseOperands;
  }

public:
  RegisterRef rst() { return operand(rstBase() + 0)->as<RegisterRef>(); }
  uint32_t rstPolarity() {
    return operand(rstBase() + 1)->as<ConstantRef>().getExactVal();
  }
  ConstantRef rstVal() { return operand(rstBase() + 2)->as<ConstantRef>(); }

  RegisterRef clkEn() { return operand(clkEnBase() + 0)->as<RegisterRef>(); }
  uint32_t clkEnPolarity() {
    return operand(clkEnBase() + 1)->as<ConstantRef>().getExactVal();
  }
};

}; // namespace dyno
