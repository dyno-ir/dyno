#pragma once
#include "hw/BitRange.h"
#include "hw/HWInstr.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"

namespace dyno {

class LoadIRef : public OpcodeInstrRef<HWInstrRef, HW_LOAD> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  // maybe do typed operand ref?


  WireRef value() { return operand(0)->as<WireRef>(); }
  RegisterRef reg() { return operand(1)->as<RegisterRef>(); }

  BitRangeOperand range() { return BitRangeOperand{operand(2)}; }
  bool hasRange() { return getNumOperands() > 3; }
  std::optional<BitRangeOperand> getRange() {
    return hasRange() ? std::make_optional(range()) : std::nullopt;
  }
};
class StoreIRef
    : public OpcodeInstrRef<HWInstrRef, HW_STORE, HW_STORE_DEFER> {
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
