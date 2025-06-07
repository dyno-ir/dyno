#pragma once
#include "hw/BitRange.h"
#include "hw/HWInstr.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"

namespace dyno {

template <typename Derived> class RangeMixin {

  Derived &self() { return *static_cast<Derived *>(this); }

public:
  std::pair<uint32_t, uint32_t> getConstAccessRange() {
    uint32_t addr = 0;
    uint32_t len = *self().reg()->numBits;
    if (self().hasRange() && self().range()->isConstant()) {
      addr = self().range()->getAddr().template as<ConstantRef>().getExactVal();
      len = self().range()->getLen().template as<ConstantRef>().getExactVal();
    }
    return std::make_pair(addr, len);
  }
};

class LoadIRef : public OpcodeInstrRef<HWInstrRef, HW_LOAD>,
                 public RangeMixin<LoadIRef> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  // maybe do typed operand ref?

  WireRef value() { return operand(0)->as<WireRef>(); }
  RegisterRef reg() { return operand(1)->as<RegisterRef>(); }

  // BitRangeOperand range() { return BitRangeOperand{operand(2)}; }
  bool hasRange() { return getNumOperands() > 3; }
  std::optional<BitRangeOperand> range() {
    return hasRange() ? std::make_optional(BitRangeOperand{operand(2)})
                      : std::nullopt;
  }
};
class StoreIRef : public OpcodeInstrRef<HWInstrRef, HW_STORE, HW_STORE_DEFER>,
                  public RangeMixin<StoreIRef> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  HWValue value() { return operand(0)->as<HWValue>(); }
  RegisterRef reg() { return operand(1)->as<RegisterRef>(); }

  bool hasRange() { return getNumOperands() > 3; }
  std::optional<BitRangeOperand> range() {
    return hasRange() ? std::make_optional(BitRangeOperand{operand(2)})
                      : std::nullopt;
  }

  bool hasTrigger() { return getNumOperands() == (hasRange() ? 5 : 3); }
  TriggerRef trigger() {
    if (!hasTrigger())
      return nullref;
    return operand(hasRange() ? 4 : 2)->as<TriggerRef>();
  }
};

}; // namespace dyno
