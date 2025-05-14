#pragma once
#include "hw/HWValue.h"
#include "support/RTTI.h"

namespace dyno {

template <typename Derived> class BitRangeMixin {
  Derived &self() { return *static_cast<Derived *>(this); }
  const Derived &cself() const { return *static_cast<const Derived *>(this); }

  bool isFull() {
    return self().getAddr() == ConstantRef::fromU32(0) &&
           self().getLen() == nullref;
  }

  template <typename Derived2>
  friend bool operator==(const BitRangeMixin<Derived> &lhs,
                         const BitRangeMixin<Derived2> &rhs) {
    return lhs.cself().getAddr() == rhs.cself().getAddr() &&
           lhs.cself().getLen() == rhs.cself().getLen();
  }
};

class BitRange;
class BitRangeOperand;

class BitRange : public BitRangeMixin<BitRange> {
public:
  HWValue addr;
  HWValue len;

  HWValue getAddr() const { return addr; }
  HWValue getLen() const { return len; }

  BitRange() = default;
  BitRange(BitRangeOperand);
  BitRange(HWValue addr, HWValue len) : addr(addr), len(len) {}
  BitRange(ConstantRef addr, ConstantRef len)
      : addr(HWValue{addr}), len(HWValue{len}) {}

  static BitRange full() { return BitRange(ConstantRef::fromU32(0), nullref); }
};

class BitRangeOperand : public BitRangeMixin<BitRange> {
public:
  OperandRef base;
  HWValue getAddr() const { return base->as<HWValue>(); }
  HWValue getLen() const {
    auto it = base;
    ++it;
    assert(it != base.instr().end());
    return (it)->as<HWValue>();
  }
};

inline BitRange::BitRange(BitRangeOperand operand)
    : addr(operand.getAddr()), len(operand.getLen()) {}

}; // namespace dyno
