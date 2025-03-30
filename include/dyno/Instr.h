#pragma once

#include "dyno/IDs.h"
#include "support/Bits.h"
#include "support/RTTI.h"
#include "support/SmallVec.h"
#include <cassert>
#include <cstdint>
#include <dyno/Interface.h>
#include <dyno/Obj.h>
#include <string_view>
#include <support/InlineStorage.h>
#include <support/Ranges.h>
#include <utility>

namespace dyno {

class Instr;
class Operand;
class InstrRef;
class OperandRef;
class InstrDefUse;
class InsrBuilder;

class Operand : public RTTIUtilMixin<Operand> {
  friend class Instr;
  friend class InstrRef;
  friend class OperandRef;
  friend class InstrBuilder;
  friend class InstrDefUse;

  DynObjRef ref;
  InlineStorage<8> custom;

  static inline bool isDefUseOperand(DynObjRef ref) {
    return ref.getTyID() & bit_mask_msb<TyID::num_t>();
  }

public:
  Operand(const Operand &) = delete;
  Operand(Operand &&) = delete;
  Operand &operator=(const Operand &) = delete;
  Operand &operator=(Operand &&) = delete;

public:
  template <typename T = void> FatDynObjRef<T> fat() const {
    auto ptr = custom.as<T *>();
    return {ref, *ptr};
  }

  template <typename T> void emplace(FatDynObjRef<T> newRef) {
    ref = newRef;
    InlineStorageRef<T *>{custom}.emplace(newRef.getPtr());
  }

  template <typename T> void emplace(FatObjRef<T> newRef) {
    ref = newRef;
    InlineStorageRef<T *>{custom}.emplace(newRef.getPtr());
  }

  void emplace(DynObjRef newRef) {
    assert(!isDefUseOperand(newRef));
    ref = newRef;
  }

  void destroy();

  // we need this so is_impl functions can take this as an arg (todo: in OperandRef)
  operator FatDynObjRef<>() const { return fat(); }
  // for as<>
  template <typename T> explicit operator T() const {
    return static_cast<T>(fat());
  }
};
static_assert(sizeof(Operand) == 16);

using OpcodeID = IDImpl<uint16_t>;

class Instr : public TrailingObjArr<Instr, Operand> {
  friend class TrailingObjArr;
  friend class InstrRef;
  friend class OperandRef;
  friend class InstrBuilder;

  DialectID dialect;
  uint8_t _unused; // num extra operands/storage
  OpcodeID opc;
  uint16_t numOperands;
  uint16_t numDefs;
  InlineStorage<8> customStorage;

public:
  using iterator = Operand *;

  Instr(DynObjRef, uint16_t numOperands, DialectID dialect, OpcodeID opc)
      : dialect(dialect), opc(opc), numOperands(numOperands) {}

  Instr(const Instr &) = delete;
  Instr(Instr &&) = delete;
  Instr &operator=(const Instr &) = delete;
  Instr &operator=(Instr &&) = delete;

  ~Instr() {
    for (auto &op : *this) {
      op.destroy();
    }
  }

private:
  iterator begin() { return trailing(); }
  iterator end() { return trailing() + numOperands; }

  iterator def_begin() { return trailing(); }
  iterator def_end() { return trailing() + numDefs; }
  iterator other_begin() { return trailing() + numDefs; }
  iterator other_end() { return trailing() + numOperands; }

  Operand &operand(unsigned n) {
    assert(n < numOperands);
    return *(begin() + n);
  }

  size_t getNumTrailing() { return numOperands; }
};
static_assert(sizeof(Instr) == 16);
static_assert(TrailingObj<Instr>);

// could maybe make this just a ptr to the operand and find parent another way
class OperandRef {
  friend class InstrBuilder;

  // why do we store dialect + type here if we know it's an instr?
  FatDynObjRef<Instr> instrRef;

public:
  using iterator_category = std::bidirectional_iterator_tag;
  using value_type = Operand;
  using difference_type = std::ptrdiff_t;
  using pointer = value_type *;
  using reference = value_type &;

  explicit OperandRef(FatDynObjRef<Instr> instrRef) : instrRef(instrRef) {}
  OperandRef(FatObjRef<Instr> instrRef, unsigned opNum) : instrRef(instrRef) {
    this->instrRef.setCustom(opNum);
  }

  InstrRef instr() const;

  unsigned getNum() const { return instrRef.getCustom(); }

  bool isDef() const { return instrRef.getCustom() < instrRef->numDefs; }

  DynObjRef getRef() const { return (*this)->ref; }

  bool hasDefUse() { return Operand::isDefUseOperand(getRef()); }

  InstrDefUse &defUse() {
    assert(hasDefUse());
    return *(*this)->fat<InstrDefUse>();
  }

  OperandRef &operator++() {
    instrRef.setCustom(instrRef.getCustom() + 1);
    return *this;
  }

  OperandRef operator++(int) {
    auto tmp(*this);
    ++(*this);
    return tmp;
  }

  OperandRef &operator--() {
    instrRef.setCustom(instrRef.getCustom() - 1);
    return *this;
  }

  OperandRef operator--(int) {
    auto tmp(*this);
    --(*this);
    return tmp;
  }

  friend bool operator==(const OperandRef &a, const OperandRef &b) {
    return a.instrRef == b.instrRef;
  }

  Operand &operator*() const { return instrRef->operand(getNum()); }
  Operand *operator->() const { return &instrRef->operand(getNum()); }
  explicit operator Operand &() const { return instrRef->operand(getNum()); }

  // This works but a little overkill
  // operator FatDynObjRef<>() const {
  //  return instrRef->operand(getNum()).fat();
  //}

private:
  void addToDefUse() const;
};

class InstrRef : public FatObjRef<Instr> {
  friend class InstrDefUse;
  using FatObjRef<Instr>::FatObjRef;

public:
  class iterator {
    OperandRef ref;

  public:
    using iterator_category = std::bidirectional_iterator_tag;
    using value_type = OperandRef;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type *;
    using reference = value_type &;

    iterator() : ref(FatObjRef<Instr>{}) {}
    iterator(OperandRef ref) : ref(ref) {}

    iterator &operator++() {
      ++ref;
      return *this;
    }

    iterator operator++(int) {
      auto tmp(*this);
      ++(*this);
      return tmp;
    }

    iterator &operator--() {
      --ref;
      return *this;
    }

    iterator operator--(int) {
      auto tmp(*this);
      --(*this);
      return tmp;
    }

    friend bool operator==(const iterator &a, const iterator &b) {
      return a.ref == b.ref;
    }

    OperandRef &operator*() const { return const_cast<OperandRef &>(ref); }
    OperandRef *operator->() { return &ref; }
  };
  static_assert(std::bidirectional_iterator<iterator>);

  InstrRef() {}
  explicit InstrRef(FatObjRef<Instr> instrRef) : FatObjRef<Instr>(instrRef) {}
  InstrRef(ObjID obj, void *ptr) : FatObjRef<Instr>(obj, ptr) {}
  InstrRef(nullref_t) : FatObjRef<Instr>(nullref) {}

  iterator begin() { return OperandRef{*this, 0}; }
  iterator end() { return OperandRef{*this, (*this)->numOperands}; }
  iterator def_begin() { return begin(); }
  iterator def_end() { return other_begin(); }
  iterator other_begin() { return OperandRef{*this, (*this)->numDefs}; }
  iterator other_end() { return end(); }

  OperandRef operand(unsigned n) {
    assert(n < getNumOperands());
    return OperandRef{*this, n};
  }

  OperandRef def(unsigned n = 0) {
    assert(n < getNumDefs());
    return OperandRef{*this, n};
  }

  OperandRef other(unsigned n) {
    assert(n < getNumOthers());
    return OperandRef{*this, getNumDefs() + n};
  }

  DialectID getDialect() { return (*this)->dialect; }
  OpcodeID getOpcode() { return (*this)->opc; }
  unsigned getNumOperands() { return (*this)->numOperands; }
  unsigned getNumDefs() { return (*this)->numDefs; }
  unsigned getNumOthers() { return (*this)->numOperands - (*this)->numDefs; }

  Range<iterator> defs() { return {def_begin(), def_end()}; }
  Range<iterator> others() { return {other_begin(), other_end()}; }
};

class InstrDefUse {
  friend class Operand;
  friend class OperandRef;

  SmallVec<OperandRef, 4> refs;
  uint16_t numDefs = 0;

public:
  using iterator = const OperandRef *;

  unsigned getNumDefsAndUses() { return refs.size(); }
  unsigned getNumDefs() { return numDefs; }
  unsigned getNumUses() { return refs.size() - numDefs; }

  const OperandRef &getDef() {
    assert(numDefs == 1);
    return *def_begin();
  }

  bool hasSingleDef() { return numDefs == 1; }

  iterator getSingleDef() {
    if (!hasSingleDef())
      return nullptr;
    return def_begin();
  }

  bool hasSingleUse() { return getNumUses() == 1; }

  iterator getSingleUse() {
    if (!hasSingleUse())
      return nullptr;
    return use_begin();
  }

  iterator begin() { return refs.begin(); }
  iterator end() { return refs.end(); }

  iterator def_begin() { return refs.begin(); }
  iterator def_end() { return def_begin() + numDefs; }
  iterator use_begin() { return def_end(); }
  iterator use_end() { return end(); }

  Range<iterator> defs() { return {def_begin(), def_end()}; }
  Range<iterator> uses() { return {use_begin(), use_end()}; }

private:
  void insert(OperandRef opRef) {
    assert(opRef.hasDefUse());
    assert(!opRef->ref.isCustom());
    unsigned pos;
    if (opRef.isDef()) {
      pos = numDefs++;
      auto it = refs.begin() + pos;
      if (it != refs.end()) {
        (*it)->ref.setCustom(refs.size());
      }
      refs.insert_unordered(it, opRef);
    } else {
      pos = refs.size();
      refs.emplace_back(opRef);
    }
    opRef->ref.setCustom(pos);
  }

  void erase(DynObjRef ref) {
    unsigned pos = ref.getCustom();
    assert(pos < refs.size());
    assert(refs.size() > 0);
    if (refs[pos].isDef()) {
      --numDefs;
    }
    auto it = refs.begin() + pos;
    if (it == refs.end() - 1) {
      refs.back()->ref.setCustom(0);
    } else {
      refs.back()->ref.setCustom(pos);
    }
    refs.erase_unordered(it);
  }
};

inline void Operand::destroy() {
  if (isDefUseOperand(ref)) {
    fat<InstrDefUse>()->erase(ref);
  }
}

inline InstrRef OperandRef::instr() const { return instrRef.as<InstrRef>(); }

inline void OperandRef::addToDefUse() const {
  assert(Operand::isDefUseOperand(getRef()));
  (*this)->fat<InstrDefUse>()->insert(*this);
}

template <> struct ObjTraits<Instr> {
  static constexpr DialectID dialect{DIALECT_CORE};
  static constexpr TyID ty{CORE_INSTR};
  using FatRefT = InstrRef;
  using RefT = ObjRef<Instr>;
};

class InstrBuilder {
  OperandRef op;

public:
  InstrBuilder(const InstrBuilder &) = delete;
  InstrBuilder(InstrBuilder &&) = delete;
  InstrBuilder &operator=(const InstrBuilder &) = delete;
  InstrBuilder &operator=(InstrBuilder &&) = delete;

  InstrBuilder(InstrRef instr) : op(instr, 0) {
    instr->numDefs = instr->numOperands;
  }

  ~InstrBuilder() {
    assert(op.getNum() == op.instr()->numOperands &&
           "InstrBuilder did not initialize all operands");
  }

  template <typename T, typename... Args> InstrBuilder &add(Args &&...args) {
    using RefT = ObjTraits<T>::FatRefT;
    addRef(RefT(std::forward<Args>(args)...));
    return *this;
  }

  InstrBuilder &other() {
    op.instr()->numDefs = op.getNum();
    return *this;
  }

  /*template <typename RefT>
  InstrBuilder &addRef(RefT ref)
    requires(IsAnyObjRef<RefT>)
  {
    op->emplace(ref);
    if (op.hasDefUse()) {
      op.addToDefUse();
    }
    ++op;
    return *this;
  }*/
  InstrBuilder &addRef(FatDynObjRef<> ref) {
    op->emplace(ref);
    if (op.hasDefUse()) {
      op.addToDefUse();
    }
    ++op;
    return *this;
  }
};

struct OpcodeInfo {
  std::string_view name;
};

constexpr OpcodeInfo coreOpcodeInfo[] = {{"block_instr"}};

template <> struct InterfaceTraits<OpcodeInfo> {
  static const OpcodeInfo *dispatch1(InstrRef ref,
                                     const OpcodeInfo **interfaces) {
    return interfaces[ref.getDialect()];
  }
  static const OpcodeInfo &dispatch2(InstrRef ref,
                                     const OpcodeInfo *interface) {
    return interface[ref.getOpcode()];
  }
};

class BinopInstrRef : public InstrRef {};

} // namespace dyno
template <> struct IsByValueRTTI<dyno::Operand> : std::true_type {};
