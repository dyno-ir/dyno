#pragma once
#include "hw/HWValue.h"
#include "support/Optional.h"
#include "support/RTTI.h"
#include "support/Utility.h"

namespace dyno {

template <typename Derived> class BitRangeMixin {
  Derived &self() { return *static_cast<Derived *>(this); }
  const Derived &cself() const { return *static_cast<const Derived *>(this); }

public:
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

  bool hasLen() const { return !!cself().getLen(); }
  bool isConstant() const {
    return cself().getAddr().template is<ConstantRef>() &&
           (!hasLen() || cself().getLen().template is<ConstantRef>());
  }

  template <typename Derived2>
  static bool equalsWithDefaultSize(const BitRangeMixin<Derived> &lhs,
                                    const BitRangeMixin<Derived2> &rhs,
                                    Optional<uint32_t> defaultSize);

  uint32_t getExactConstantLen() {
    return cself().getLen().template as<ConstantRef>().getExactVal();
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
  BitRange(uint32_t addr, uint32_t len)
      : addr(ConstantRef::fromU32(addr)), len(ConstantRef::fromU32(len)) {}

  static BitRange full() { return BitRange(ConstantRef::fromU32(0), nullref); }
};

class BitRangeOperand : public BitRangeMixin<BitRangeOperand> {
public:
  OperandRef base;
  HWValue getAddr() const { return base->as<HWValue>(); }
  HWValue getLen() const {
    auto it = base + 1;
    assert(it != base.instr().end());
    return (it)->as<HWValue>();
  }
  BitRangeOperand(OperandRef base) : base(base) {}
};

inline BitRange::BitRange(BitRangeOperand operand)
    : addr(operand.getAddr()), len(operand.getLen()) {}

template <typename Derived>
template <typename Derived2>
inline bool BitRangeMixin<Derived>::equalsWithDefaultSize(
    const BitRangeMixin<Derived> &lhs, const BitRangeMixin<Derived2> &rhs,
    Optional<uint32_t> defaultSize) {
  if (!defaultSize)
    return lhs == rhs;
  assert(lhs.hasLen() ||
         rhs.hasLen() &&
             "length wildcard on both sides of BitRange comparison");
  if (!lhs.hasLen())
    return BitRange{lhs.cself().getAddr(),
                    ConstantRef::fromU32(*defaultSize)} == rhs;
  if (!rhs.hasLen())
    return lhs ==
           BitRange{rhs.cself().getAddr(), ConstantRef::fromU32(*defaultSize)};
  dyno_unreachable("");
}

}; // namespace dyno
