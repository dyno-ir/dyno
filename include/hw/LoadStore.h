#pragma once
#include "dyno/Constant.h"
#include "dyno/Obj.h"
#include "hw/HWInstr.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/MemoryPort.h"
#include "hw/Pointer.h"
#include <algorithm>
#include <utility>

namespace dyno {

template <typename Derived> struct AddressGenTermMixin {
  Derived &self() { return *static_cast<Derived *>(this); }
  const Derived &cself() const { return *static_cast<const Derived *>(this); }

public:
};

template <typename IdxT, typename RefT = OperandRef>
class AddressGenTermOperandBase
    : public AddressGenTermMixin<AddressGenTermOperandBase<IdxT>> {
  RefT base;

public:
  IdxT getIdx() const { return base[0].template as<IdxT>(); }
  uint32_t getFact() const {
    return base[1].template as<ConstantRef>().getExactVal();
  }
  Optional<uint32_t> getMax() const {
    auto val = base[2].template as<ConstantRef>().getExactVal();
    if (val == ~0u)
      return nullopt;
    return val;
  }

  OperandRef getUnderlyingOperand() const { return base; }

  AddressGenTermOperandBase(RefT base) : base(base) {}
};
using AddressGenTermOperand = AddressGenTermOperandBase<HWValue>;
using AddressGenTermRef = AddressGenTermOperandBase<HWValue, FatDynObjRef<> *>;

class AddressGenTerm : public AddressGenTermMixin<AddressGenTerm> {
  HWValue idx;
  uint32_t fact;
  Optional<uint32_t> max;

public:
  HWValue getIdx() const { return idx; }
  uint32_t getFact() const { return fact; }
  Optional<uint32_t> getMax() const { return max; }

  AddressGenTerm() = default;
  AddressGenTerm(HWValue idx, uint32_t fact, Optional<uint32_t> max = nullopt)
      : idx(idx), fact(fact), max(max) {}
  AddressGenTerm(AddressGenTermOperand other)
      : idx(other.getIdx()), fact(other.getFact()), max(other.getMax()) {}
  friend bool operator==(const AddressGenTerm &lhs, const AddressGenTerm &rhs) {
    return lhs.idx == rhs.idx && lhs.fact == rhs.fact && lhs.max == rhs.max;
  }
};
template <typename Derived> class AddressGenMixin {
  Derived &self() { return *static_cast<Derived *>(this); }
  const Derived &self() const { return *static_cast<const Derived *>(this); }
  static constexpr unsigned TermSize = 3;

  unsigned getTermsBaseIndex() const {
    return self().addressGenBaseIndex() + 1;
  }

public:
  auto terms() {
    auto beginIt = self().other_end();
    if (getNumTerms() != 0)
      beginIt = self().other_begin() + getTermsBaseIndex();

    return Range{beginIt, self().other_end()}.step(3).transform(
        [](size_t, auto ref) { return AddressGenTermOperand{ref}; });
  }

  AddressGenTermOperand term(unsigned i = 0) {
    return AddressGenTermOperand{idx(i)};
  }
  OperandRef idx(unsigned i = 0) {
    return self().other(getTermsBaseIndex() + TermSize * i);
  }
  OperandRef factor(unsigned i = 0) {
    return self().other(getTermsBaseIndex() + TermSize * i + 1);
  }
  OperandRef max(unsigned i = 0) {
    return self().other(getTermsBaseIndex() + TermSize * i + 2);
  }

  OperandRef base() {
    return *(self().other_begin() + self().addressGenBaseIndex());
  }
  uint32_t getBase() {
    return hasBase() ? base()->template as<ConstantRef>().getExactVal() : 0;
  }

  unsigned getNumTerms() const {
    if (!hasBase())
      return 0;
    auto n = self().getNumOthers() - getTermsBaseIndex();
    assert(n % TermSize == 0);
    return n / TermSize;
  }

  bool hasBase() const {
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

template <typename U, typename T>
inline bool addressingFragsEqual(AddressGenMixin<T> &lhs,
                                 AddressGenMixin<U> &rhs) {
  if (lhs.getNumTerms() != rhs.getNumTerms())
    return false;
  for (auto [lhs, rhs] : lhs.terms().zip(rhs.terms())) {
    if (AddressGenTerm{lhs} != AddressGenTerm{rhs})
      return false;
  }
  return true;
}

class GEPIRef : public OpcodeInstrRef<HWInstrRef, HW_GEP>,
                public AddressGenMixin<GEPIRef> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;
  unsigned addressGenBaseIndex() const { return 0; }
  PointerRef ptr() { return def(0)->as<PointerRef>(); }

  Optional<uint32_t> getMaxOffset() {
    uint32_t endOffs = 0;
    for (auto term : terms()) {
      auto max = term.getMax();
      if (!max) {
        return nullopt;
      }
      endOffs += (*max - 1) * term.getFact();
    }
    return endOffs;
  }
};

class LoadIRef : public OpcodeInstrRef<HWInstrRef, HW_LOAD>,
                 public AddressGenMixin<LoadIRef> {
public:
  unsigned addressGenBaseIndex() const { return 1; }

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
  unsigned addressGenBaseIndex() const { return hasTrigger() ? 3 : 2; }

public:
  using OpcodeInstrRef::OpcodeInstrRef;

  HWValue value() { return operand(0)->as<HWValue>(); }
  RegisterRef reg() { return operand(1)->as<RegisterRef>(); }

  bool hasTrigger() const {
    return getNumOperands() > 2 && operand(2)->is<TriggerRef>();
  }
  TriggerIRef trigger() const {
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
  unsigned addressGenBaseIndex() const { return 1; }

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
  unsigned addressGenBaseIndex() const { return 2; }

public:
  using OpcodeInstrRef::OpcodeInstrRef;
  OperandRef out() { return operand(0); }
  OperandRef in() { return operand(1); }
  OperandRef val() { return operand(2); }

  uint32_t getMemoryLen() { return *out()->as<WireRef>().getNumBits(); }
  uint32_t getLen() { return *val()->as<HWValue>().getNumBits(); }
};

template <typename Derived> class PointerMixin {

public:
  Derived &self() { return *static_cast<Derived *>(this); }

  auto getConstAccessRange() {
    auto addrOp = self().addr();
    if (!addrOp)
      return std::make_pair(0U, self().getLen());
    if (auto c = addrOp.template dyn_as<ConstantRef>()) {
      return std::make_pair(c.template as<ConstantRef>().getExactVal(),
                            self().getLen());
    }
    auto gep = addrOp.template as<PointerRef>()
                   .getDef()
                   .instr()
                   .template as<GEPIRef>();
    auto base = gep.getBase();
    auto maxOffset = gep.getMaxOffset();
    auto pessimisticMax = self().getMemoryLen() - base;

    if (maxOffset)
      return std::make_pair(0U, std::min(*maxOffset, pessimisticMax));
    return std::make_pair(0U, pessimisticMax);
  }

  GEPIRef gep() {
    auto a = self().addr();
    if (!a || !a.template is<PointerRef>())
      return nullref;
    return a.template as<PointerRef>().getDef().instr().template as<GEPIRef>();
  }
  using TermsT = decltype(std::declval<GEPIRef>().terms());
  auto terms() {
    if (auto g = gep())
      return g.terms();
    return TermsT::emptyRange();
  }

  uint32_t base() {
    auto a = self().addr();
    if (!a)
      return 0U;
    if (auto asConst = a.template dyn_as<ConstantRef>())
      asConst.getExactVal();
    return gep().getBase();
  }
};



class MemLoadIRef : public OpcodeInstrRef<HWInstrRef, HW_MEM_LOAD>,
                    public PointerMixin<MemLoadIRef> {
public:
  unsigned forwardsIndex() const { return hasTrigger() + triggerIndex(); }
  unsigned addrIndex() const { return hasEn() ? 2 : 1; }
  unsigned triggerIndex() const { return hasAddr() + addrIndex(); }

public:
  using OpcodeInstrRef::OpcodeInstrRef;

  WireRef value() { return def(0)->as<WireRef>(); }
  MemoryPortRef port() { return def(1)->as<MemoryPortRef>(); }

  RegisterRef reg() { return other(0)->as<RegisterRef>(); }

  WireRef en() const {
    if (getNumOthers() < 2)
      return nullref;
    return other(1)->dyn_as<WireRef>();
  }
  bool hasEn() const { return !!en(); }

  TriggerRef trigger() const {
    if (getNumOthers() < triggerIndex() + 1)
      return nullref;
    return other(triggerIndex())->dyn_as<TriggerRef>();
  }
  bool hasTrigger() const { return !!trigger(); }

  uint32_t getMemoryLen() { return *reg().getNumBits(); }
  uint32_t getLen() { return *value().getNumBits(); }

  HWAddress addr() const {
    if (getNumOthers() < addrIndex() + 1)
      return nullref;
    return other(addrIndex())->as<HWAddress>();
  }
  bool hasAddr() const { return !!addr(); }

  auto writeForwards() {
    return Range{other_begin() + forwardsIndex(), other_end()}.zip(
        port()->writeForwardMeta);
  }

  bool isFullReg() { return getLen() == reg().getNumBits() && !addr(); }
};

class MemStoreIRef : public OpcodeInstrRef<HWInstrRef, HW_MEM_STORE>,
                     public PointerMixin<MemStoreIRef> {
public:
  unsigned addrIndex() const { return hasEn() ? 3 : 2; }
  unsigned triggerIndex() const { return hasAddr() + addrIndex(); }

public:
  using OpcodeInstrRef::OpcodeInstrRef;

  MemoryPortRef port() { return def(0)->as<MemoryPortRef>(); }
  WireRef value() { return other(0)->as<WireRef>(); }

  RegisterRef reg() { return other(1)->as<RegisterRef>(); }

  WireRef en() const {
    if (getNumOthers() < 3)
      return nullref;
    return other(2)->dyn_as<WireRef>();
  }
  bool hasEn() const { return !!en(); }

  TriggerRef trigger() const {
    if (getNumOthers() < triggerIndex() + 1)
      return nullref;
    return other(triggerIndex())->dyn_as<TriggerRef>();
  }
  bool hasTrigger() const { return !!trigger(); }

  uint32_t getMemoryLen() { return *reg().getNumBits(); }
  uint32_t getLen() { return *value().getNumBits(); }

  HWAddress addr() const {
    if (getNumOthers() < addrIndex() + 1)
      return nullref;
    return other(addrIndex())->as<HWAddress>();
  }
  bool hasAddr() const { return !!addr(); }

  bool isFullReg() { return getLen() == reg().getNumBits() && !addr(); }
};

}; // namespace dyno
