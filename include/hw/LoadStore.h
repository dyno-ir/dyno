#pragma once
#include "hw/HWInstr.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include <algorithm>

namespace dyno {

template <typename Derived> struct AddressGenTermMixin {
  Derived &self() { return *static_cast<Derived *>(this); }
  const Derived &cself() const { return *static_cast<const Derived *>(this); }

public:
};

class AddressGenTermOperand
    : public AddressGenTermMixin<AddressGenTermOperand> {
  OperandRef base;

public:
  HWValue getIdx() const { return base[0].as<HWValue>(); }
  uint32_t getFact() const { return base[1].as<ConstantRef>().getExactVal(); }
  Optional<uint32_t> getMax() const {
    auto val = base[2].as<ConstantRef>().getExactVal();
    if (val == ~0u)
      return nullopt;
    return val;
  }

  AddressGenTermOperand(OperandRef base) : base(base) {}
};

class AddressGenTerm : public AddressGenTermMixin<AddressGenTerm> {
  HWValue idx;
  uint32_t fact;
  Optional<uint32_t> max;

public:
  HWValue getIdx() const { return idx; }
  uint32_t getFact() const { return fact; }
  Optional<uint32_t> getMax() const { return max; }

  AddressGenTerm() = default;
  AddressGenTerm(HWValue idx, uint32_t fact)
      : idx(idx), fact(fact), max(nullopt) {}
  AddressGenTerm(HWValue idx, uint32_t fact, uint32_t max)
      : idx(idx), fact(fact), max(max) {}
  AddressGenTerm(AddressGenTermOperand other)
      : idx(other.getIdx()), fact(other.getFact()), max(other.getMax()) {}
};
template <typename Derived> class AddressGenMixin {
  Derived &self() { return *static_cast<Derived *>(this); }
  static constexpr uint TermSize = 3;

  uint getTermsBaseIndex() { return self().addressGenBaseIndex() + 1; }

public:
  auto terms() {
    auto beginIt = self().other_end();
    if (getNumTerms() != 0)
      beginIt = self().other_begin() + getTermsBaseIndex();

    return Range{beginIt, self().other_end()}
        .transform([](size_t idx,
                      OperandRef ref) -> std::optional<AddressGenTermOperand> {
          if (idx % TermSize != 0)
            return std::nullopt;
          return AddressGenTermOperand{ref};
        })
        .discard_optional();
  };

  AddressGenTermOperand term(uint i = 0) {
    return AddressGenTermOperand{idx(i)};
  }
  OperandRef idx(uint i = 0) {
    return self().other(getTermsBaseIndex() + TermSize * i);
  }
  OperandRef factor(uint i = 0) {
    return self().other(getTermsBaseIndex() + TermSize * i + 1);
  }
  OperandRef max(uint i = 0) {
    return self().other(getTermsBaseIndex() + TermSize * i + 2);
  }

  OperandRef base() { return self().other(self().addressGenBaseIndex()); }
  uint32_t getBase() {
    return hasBase() ? base()->template as<ConstantRef>().getExactVal() : 0;
  }

  uint getNumTerms() {
    if (!hasBase())
      return 0;
    auto n = self().getNumOthers() - getTermsBaseIndex();
    assert(n % TermSize == 0);
    return n / TermSize;
  }

  bool hasBase() {
    return self().getNumOthers() > self().addressGenBaseIndex();
  }

  bool isConstantOffs() { return getNumTerms() == 0; }

  std::pair<uint32_t, uint32_t> getConstAccessRange() {
    if (isConstantOffs())
      return std::make_pair(getBase(), self().getLen());

    Optional<uint32_t> endOffs = self().getLen();
    for (auto term : terms()) {
      auto max = term.getMax();
      if (!max) {
        endOffs = nullopt;
        break;
      }
      *endOffs += (*max - 1) * term.getFact();
    }

    auto pessimisticMax = self().getMemoryLen() - getBase();

    uint32_t max;
    if (endOffs)
      max = std::min(*endOffs, pessimisticMax);
    else
      max = pessimisticMax;

    return std::make_pair(getBase(), max);
  }
};

class LoadIRef : public OpcodeInstrRef<HWInstrRef, HW_LOAD>,
                 public AddressGenMixin<LoadIRef> {
public:
  uint addressGenBaseIndex() { return 1; }

public:
  using OpcodeInstrRef::OpcodeInstrRef;

  WireRef value() { return operand(0)->as<WireRef>(); }
  RegisterRef reg() { return operand(1)->as<RegisterRef>(); }

  uint32_t getMemoryLen() { return *reg().getNumBits(); }
  uint32_t getLen() { return *value().getNumBits(); }

  bool isFullReg() {
    return getLen() == reg().getNumBits() && isConstantOffs() && getBase() == 0;
  }
};

class StoreIRef : public OpcodeInstrRef<HWInstrRef, HW_STORE, HW_STORE_DEFER>,
                  public AddressGenMixin<StoreIRef> {
public:
  uint addressGenBaseIndex() { return hasTrigger() ? 3 : 2; }

public:
  using OpcodeInstrRef::OpcodeInstrRef;

  HWValue value() { return operand(0)->as<HWValue>(); }
  RegisterRef reg() { return operand(1)->as<RegisterRef>(); }

  bool hasTrigger() {
    return getNumOperands() > 2 && operand(2)->is<TriggerRef>();
  }
  TriggerIRef trigger() {
    if (!hasTrigger())
      return nullref;
    return operand(2)->as<TriggerRef>().iref();
  }

  uint32_t getMemoryLen() { return *reg().getNumBits(); }
  uint32_t getLen() { return *value().getNumBits(); }

  bool isFullReg() {
    return getLen() == reg().getNumBits() && isConstantOffs() && getBase() == 0;
  }
};

class SpliceIRef : public OpcodeInstrRef<HWInstrRef, HW_SPLICE>,
                   public AddressGenMixin<SpliceIRef> {
public:
  uint addressGenBaseIndex() { return 1; }

public:
  using OpcodeInstrRef::OpcodeInstrRef;
  OperandRef out() { return operand(0); }
  OperandRef in() { return operand(1); }

  uint32_t getMemoryLen() { return *in()->as<HWValue>().getNumBits(); }
  uint32_t getLen() { return *out()->as<HWValue>().getNumBits(); }
};

class InsertIRef : public OpcodeInstrRef<HWInstrRef, HW_INSERT>,
                   public AddressGenMixin<InsertIRef> {
public:
  uint addressGenBaseIndex() { return 2; }

public:
  using OpcodeInstrRef::OpcodeInstrRef;
  OperandRef out() { return operand(0); }
  OperandRef in() { return operand(1); }
  OperandRef val() { return operand(2); }

  uint32_t getMemoryLen() { return *out()->as<WireRef>().getNumBits(); }
  uint32_t getLen() { return *val()->as<HWValue>().getNumBits(); }
};

}; // namespace dyno
