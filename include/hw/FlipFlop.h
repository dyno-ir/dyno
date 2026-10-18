#pragma once

#include "dyno/Constant.h"
#include "dyno/Instr.h"
#include "hw/HWInstr.h"
#include "hw/HWValue.h"
#include "hw/analysis/WireVariable.h"
#include "op/IDs.h"
#include "support/Utility.h"
namespace dyno {

template <typename Derived> class FFLatchResetMixin {

  const auto &cself() const { return *static_cast<const Derived *>(this); }
  auto &self() { return *static_cast<Derived *>(this); }

protected:
  unsigned rstBase(unsigned i = 0) const {
    return Derived::numBaseOperands + i * Derived::numRstOperands;
  }

public:
  unsigned numRsts() const {
    return ((cself().getNumOperands() - Derived::numBaseOperands -
             Derived::numInitOperands) /
            Derived::numRstOperands);
  }

  HWValue rstRaw(unsigned i = 0) const {
    return cself().other(rstBase(i) + 0)->template as<HWValue>();
  }
  std::pair<HWValue, bool> rstAndPol(unsigned i = 0) const {
    auto raw = rstRaw(i);
    if (auto w = raw.template dyn_as<WireRef>(); w && w.getDefI().isOpc(OP_NOT))
      return {w.getDefI().other(0)->template as<HWValue>(), false};
    return {raw, true};
  }
  HWValue rst(unsigned i = 0) const { return rstAndPol(i).first; }
  bool rstPol(unsigned i = 0) const { return rstAndPol(i).second; }
  HWValue rstVal(unsigned i = 0) const {
    return cself().other(rstBase(i) + 1)->template as<HWValue>();
  }

  auto rsts() {
    return Range{self().other_begin() + rstBase(),
                 self().other_end() - !!self().initValue()}
        .template as<HWValue>()
        .template tuple<2>();
  }

  HWValue initValue() {
    if ((self().getNumOthers() & 1) == (Derived::numBaseOperands & 1))
      return nullref;
    return self().other_end()[-1]->template as<HWValue>();
  }
};

class FlipFlopIRef
    : public OpcodeInstrRef<HWInstrRef, HW_FLIP_FLOP, HW_FLIP_FLOP_SRST>,
      public FFLatchResetMixin<FlipFlopIRef> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

private:
public:
  constexpr static unsigned numBaseOperands = 3;
  constexpr static unsigned numRstOperands = 2;
  constexpr static unsigned numInitOperands = 1; // trailing
  HWValue clkRaw() const { return other(0)->as<HWValue>(); }

  // todo: make *raw signals the main one, leave polarity up to user
  std::pair<HWValue, bool> clkAndPol() const {
    return WireVariable::getSignalWithPolarity(clkRaw());
  }
  HWValue clk() const { return clkAndPol().first; }
  bool clkPol() const { return clkAndPol().second; }

  HWValue d() const { return other(1)->as<HWValue>(); }
  OperandRef dOp() const { return other(1); }

  WireRef q() const { return def(0)->as<WireRef>(); }

  HWValue clkEnRaw() const {
    auto rv = other(2)->as<HWValue>();
    assert(rv.getNumBits() == 1);
    return rv;
  }
  OperandRef clkEnRawOp() const { return other(2); }
  bool hasClkEn() const {
    return !(clkEnRaw().is<ConstantRef>() &&
             clkEnRaw().as<ConstantRef>().valueEquals(1));
  }

  std::pair<HWValue, bool> clkEnAndPol() const {
    return WireVariable::getSignalWithPolarity(clkEnRaw());
  }
  HWValue clkEn() const { return clkEnAndPol().first; }
  bool clkEnPolarity() const { return clkEnAndPol().second; }

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
    INIT_VAL,
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
    if (idx == getNumOthers() - 1)
      return INIT_VAL;
    if (numRsts() > 0 && (idx - rstBase()) % numRstOperands == 0)
      return UseType(RESET_BEGIN + ((idx - rstBase()) / numRstOperands));
    dyno_unreachable("invalid operand");
  }
};

class LatchIRef : public OpcodeInstrRef<InstrRef, HW_LATCH>,
                  public FFLatchResetMixin<LatchIRef> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;
  constexpr static unsigned numBaseOperands = 2;
  constexpr static unsigned numRstOperands = 2;
  constexpr static unsigned numInitOperands = 1; // trailing

  HWValue d() const { return other(0)->as<HWValue>(); }
  HWValue en() const { return other(1)->as<HWValue>(); }
  WireRef q() const { return def(0)->as<WireRef>(); }

  enum UseType {
    D,
    Q,
    ENABLE,
    INIT_VAL,
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
      return D;
    if (idx == 1)
      return ENABLE;
    if (idx == getNumOthers() - 1)
      return INIT_VAL;
    if (numRsts() > 0 && (idx - rstBase()) % numRstOperands == 0)
      return UseType(RESET_BEGIN + ((idx - rstBase()) / numRstOperands));
    dyno_unreachable("invalid operand");
  }
};

}; // namespace dyno
