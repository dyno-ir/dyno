#pragma once

#include "dyno/IDs.h"
#include "support/Bits.h"
#include "support/RTTI.h"
#include "support/SmallVec.h"
#include "support/Utility.h"
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
  friend class GenericOperand;

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

  template <typename T = void> FatDynObjRef<T> fat() const {
    auto ptr = custom.as<T *>();
    return {ref, *ptr};
  }
  template <typename T = void> FatDynObjRef<T> cleanFat() const {
    auto f = fat<T>();
    if (isDefUseOperand(ref))
      f.clearCustom();
    return f;
  }

public:
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

  void *ptr() { return *custom.as<void *>(); }

  void destroy();

  // we need this so is_impl functions can take this as an arg (todo: in
  // OperandRef)
  operator FatDynObjRef<>() const { return cleanFat(); }
  // for as<>
  template <typename T> explicit operator T() const {
    return static_cast<T>(cleanFat());
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

  using insert_hook_t = bool (*)(InstrDefUse *, OperandRef);
  using erase_hook_t = bool (*)(InstrDefUse *, DynObjRef);

  uint16_t numDefs = 0;
  SmallVec<OperandRef, 4> refs;
  // could move existence into a bit field, and store these after InstrDefUse
  // in the parent object.
  insert_hook_t insertHook = nullptr;
  erase_hook_t eraseHook = nullptr;

public:
  using operand_t = Operand;
  using operand_ref_t = OperandRef;
  using iterator = const OperandRef *;

  unsigned getNumDefsAndUses() const { return refs.size(); }
  unsigned getNumDefs() const { return numDefs; }
  unsigned getNumUses() const { return refs.size() - numDefs; }

  const OperandRef &getDef() {
    assert(numDefs == 1);
    return *def_begin();
  }

  bool hasSingleDef() const { return numDefs == 1; }

  iterator getSingleDef() {
    if (!hasSingleDef())
      return nullptr;
    return def_begin();
  }

  bool hasSingleUse() const { return getNumUses() == 1; }

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

  void setInsertHook(insert_hook_t insertHook) {
    this->insertHook = insertHook;
  }
  void setEraseHook(erase_hook_t eraseHook) { this->eraseHook = eraseHook; }

  void manual_move(uint from, uint to) {
    if (to == from)
      return;
    assert(to <= refs.size());
    assert(from < refs.size());
    if (to == refs.size()) {
      refs.emplace_back(std::move(refs[from]));
    } else {
      refs[to] = std::move(refs[from]);
    }
    refs[to]->ref.setCustom(to);
  }

  void manual_insert(uint idx, OperandRef opRef) {
    assert(idx <= refs.size());
    if (idx == refs.size()) {
      refs.emplace_back(opRef);
    } else {
      refs[idx] = opRef;
    }
    refs[idx]->ref.setCustom(idx);
  }

  void manual_pop_back() {
    refs.back()->ref.setCustom(0);
    refs.pop_back();
  }

private:
  void insert(OperandRef opRef) {
    assert(opRef.hasDefUse());
    assert(!opRef->ref.isCustom());
    if (insertHook) [[unlikely]] {
      if (insertHook(this, opRef)) {
        if (opRef.isDef())
          numDefs++;
        return;
      }
    }
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
    if (eraseHook) [[unlikely]] {
      bool isDef = refs[pos].isDef();
      if (eraseHook(this, ref)) {
        if (isDef)
          numDefs--;
        return;
      }
    }
    if (refs[pos].isDef()) {
      if (numDefs > 1 && pos != numDefs - 1) {
        refs[pos] = std::move(refs[numDefs - 1]);
        refs[pos]->ref.setCustom(pos);
        pos = numDefs - 1;
      }
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

inline InstrRef OperandRef::instr() const { return instrRef.as<InstrRef>(); }

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

template <typename Base, uint NumCategories, auto ClassifierF>
class CategoricalDefUse : public Base {
public:
  using classifier_func_t = uint (*)(typename Base::operand_ref_t);
  std::array<uint32_t, NumCategories> catBounds = {};
  CategoricalDefUse() {
    this->setEraseHook(erase);
    this->setInsertHook(insert);
  }
  static uint
  classifyIdx(CategoricalDefUse<Base, NumCategories, ClassifierF> *self,
              uint idx) {
    // i bet this is faster than binary search
    for (size_t i = 0; i < NumCategories; i++)
      if (self->catBounds[i] > idx)
        return i;
    dyno_unreachable("not classified");
  }
  static uint classifyIdxBinSearch(
      CategoricalDefUse<Base, NumCategories, ClassifierF> *self, uint idx) {
    size_t lb = 0;
    size_t ub = NumCategories - 1;

    while (true) {
      size_t center = lb + (ub - lb) / 2;

      size_t lower = (center == 0) ? 0 : self->catBounds[center - 1];
      size_t upper = self->catBounds[center];

      if (idx < lower)
        ub = center;
      else if (idx >= upper)
        lb = center + 1;
      else
        return center;
    }
  }

  static bool insert(Base *base, Base::operand_ref_t ref) {

    auto self =
        static_cast<CategoricalDefUse<Base, NumCategories, ClassifierF> *>(
            base);

    uint useClassID = ClassifierF(ref);
    // O(#categories) insertion, keeps inter-category order but not intra (base
    // case of this for use+def is implemented in instrDefUse)
    for (uint id = NumCategories - 1; id != useClassID; id--) {
      base->manual_move(self->catBounds[id - 1], self->catBounds[id]);
      self->catBounds[id]++;
    }
    base->manual_insert(self->catBounds[useClassID]++, ref);
    return true;
  }
  static bool erase(Base *base, DynObjRef ref) {

    auto self =
        static_cast<CategoricalDefUse<Base, NumCategories, ClassifierF> *>(
            base);

    uint useClassID = classifyIdx(self, ref.getCustom());
    // move last ref in same category into slot we're freeing
    base->manual_move(self->catBounds[useClassID] - 1, ref.getCustom());
    for (uint id = useClassID; id < NumCategories - 1; id++) {
      // move last of next category into last of current category (now first of
      // next category)
      base->manual_move(self->catBounds[id + 1] - 1, self->catBounds[id] - 1);
      self->catBounds[id]--;
    }

    base->manual_pop_back();
    self->catBounds[NumCategories - 1]--;
    return true;
  }
};

#if 0
inline void GenericOperand::setLinkedPos(uint16_t pos) {
  if (auto asInstrRef = ref.dyn_as<InstrRef>())
    asInstrRef.operand(ref.getCustom())->ref.setCustom(pos);
  else
    refdDefUse().refs[ref.getCustom()].ref.setCustom(pos);
}
#endif

inline void OperandRef::addToDefUse() const {
  assert(Operand::isDefUseOperand(getRef()));

  // try to not slow down the hot path too much.
  // could also write specializations of this without dynamic dispatch
  // if ref type is known.
  (*this)->fat<InstrDefUse>()->insert(*this);
}

inline void Operand::destroy() {
  if (isDefUseOperand(ref)) {
    // try to not slow down the hot path too much.
    // could also write specializations of this without dynamic dispatch
    // if ref type is known.
    fat<InstrDefUse>()->erase(ref);
  }

  // maybe delete/decrement refcnt of constant operands here?
  // if (ref.getTyID() == CORE_CONSTANT) {}
}

} // namespace dyno
template <> struct IsByValueRTTI<dyno::Operand> : std::true_type {};
