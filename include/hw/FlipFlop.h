#pragma once

#include "dyno/Constant.h"
#include "hw/HWInstr.h"
#include "hw/HWValue.h"
#include "op/IDs.h"
#include "support/Utility.h"
namespace dyno {

class FlipFlopIRef
    : public OpcodeInstrRef<HWInstrRef, HW_FLIP_FLOP, HW_FLIP_FLOP_SRST> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

private:
public:
  constexpr static unsigned numBaseOperands = 3;
  constexpr static unsigned numRstOperands = 2;
  HWValue clkRaw() const { return other(0)->as<HWValue>(); }

  std::pair<HWValue, bool> clkAndPol() const {
    auto raw = clkRaw();
    if (auto w = raw.dyn_as<WireRef>(); w && w.getDefI().isOpc(OP_NOT))
      return {w.getDefI().other(0)->as<HWValue>(), false};
    return {raw, true};
  }
  HWValue clk() const { return clkAndPol().first; }
  bool clkPol() const { return clkAndPol().second; }

  HWValue d() const { return other(1)->as<HWValue>(); }

  WireRef q() const { return def(0)->as<WireRef>(); }

  HWValue clkEnRaw() const { return other(2)->as<HWValue>(); }
  bool hasClkEn() const {
    return !(clkEnRaw().is<ConstantRef>() &&
             clkEnRaw().as<ConstantRef>().valueEquals(1));
  }

  std::pair<HWValue, bool> clkEnAndPol() const {
    auto raw = clkEnRaw();
    if (auto w = raw.dyn_as<WireRef>(); w && w.getDefI().isOpc(OP_NOT))
      return {w.getDefI().other(0)->as<HWValue>(), false};
    return {raw, true};
  }
  HWValue clkEn() const { return clkEnAndPol().first; }
  bool clkEnPolarity() const { return clkEnAndPol().second; }

  unsigned numRsts() const {
    return ((getNumOperands() - numBaseOperands) / numRstOperands);
  }

private:
  unsigned rstBase(unsigned i = 0) const {
    return numBaseOperands + i * numRstOperands;
  }

public:
  HWValue rstRaw(unsigned i = 0) const {
    return other(rstBase(i) + 0)->as<HWValue>();
  }
  std::pair<HWValue, bool> rstAndPol(unsigned i = 0) const {
    auto raw = rstRaw(i);
    if (auto w = raw.dyn_as<WireRef>(); w && w.getDefI().isOpc(OP_NOT))
      return {w.getDefI().other(0)->as<HWValue>(), false};
    return {raw, true};
  }
  HWValue rst(unsigned i = 0) const { return rstAndPol(i).first; }
  bool rstPol(unsigned i = 0) const { return rstAndPol(i).second; }
  HWValue rstVal(unsigned i = 0) const {
    return other(rstBase(i) + 1)->as<HWValue>();
  }

  auto rsts() {
    return Range{other_begin() + rstBase(), other_end()}
        .as<HWValue>()
        .tuple<2>();
  }

  bool getPolarity(OperandRef ref) const {
    if (auto w = ref.dyn_as<WireRef>(); w && w.getDefI().isOpc(OP_NOT))
      return false;
    return true;
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
                ((InstrRef::max_others - numBaseOperands) / numRstOperands)
  };
  UseType classifyUse(OperandRef use) {
    assert(use.instr() == *this);
    if (use.isDef())
      return Q;

    unsigned idx = use - *other_begin();
    if (idx == 0)
      return CLOCK;
    if (idx == 1)
      return D;
    if (idx == 2)
      return ENABLE;
    if (numRsts() > 0 && (idx - rstBase()) % numRstOperands == 0)
      return UseType(RESET_BEGIN + ((idx - rstBase()) / numRstOperands));
    dyno_unreachable("invalid operand");
  }
};

}; // namespace dyno
