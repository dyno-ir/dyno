#pragma once
#include "dyno/NewDeleteObjStore.h"
#include "dyno/Obj.h"
#include "support/Any.h"
#include "support/Bits.h"
#include "support/Utility.h"
#include "type/IDs.h"
#include <bit>
#include <cstdint>

namespace dyno {

struct TypeObj {
  template <typename... Args> TypeObj(Args &&...) {
    dyno_unreachable("virtual, do not construct");
  }
};

class BaseTypeRef;
class StructTypeRef;
class EnumTypeRef;
class ArrayTypeRef;

struct StructTypeObj;
struct EnumTypeObj;
struct ArrayTypeObj;

class TypeDialectTypeID {
public:
  enum { BASE, STRUCT, ENUM, ARRAY };
  TypeDialectTypeID() = delete;
};

class TypeRef : public ObjRef<TypeObj> {
public:
  using ObjRef::ObjRef;
  explicit constexpr TypeRef(ObjRef ref) : ObjRef(ref) {}
  uint32_t typeID() { return getObjID() & 3; }
};
class FatTypeRef : public FatObjRef<TypeObj> {
public:
  using FatObjRef::FatObjRef;
  FatTypeRef(FatObjRef ref) : FatObjRef(ref) {}

  static bool is_impl(const BaseTypeRef &) { return true; }
  static bool is_impl(const StructTypeRef &) { return true; }
  static bool is_impl(const EnumTypeRef &) { return true; }
  static bool is_impl(const ArrayTypeRef &) { return true; }
  static bool is_impl(FatDynObjRef<> ref) {
    return ref.getType() ==
           Any{TYPE_GENERIC, TYPE_BASE, TYPE_ARRAY, TYPE_ENUM, TYPE_STRUCT};
  }

  operator TypeRef() { return TypeRef(getObjID()); }
  uint32_t typeID() { return getObjID() & 3; }
};

template <> struct ObjTraits<TypeObj> {
  static constexpr DialectType ty{TYPE_GENERIC};
  using FatRefT = FatTypeRef;
};

class BaseTypeRef : public TypeRef {
public:
  using TypeRef::TypeRef;

private:
  constexpr auto dialect() { return BitField<uint32_t, 8, 3>{obj.num}; }
  constexpr auto id() { return BitField<uint32_t, 21, 11>{obj.num}; }

public:
  constexpr DialectID getDialectID() { return DialectID{uint8_t(dialect())}; }
  constexpr uint32_t getID() { return id(); }

  constexpr BaseTypeRef(DialectID dialect, uint32_t id) {
    this->dialect() = dialect;
    this->id() = id;
  }
  explicit constexpr BaseTypeRef(DynObjRef ref)
      : TypeRef(ref.as<ObjRef<TypeObj>>()) {
    assert(is_impl(ref));
  }
  static bool is_impl(TypeRef ref) {
    return ref.typeID() == TypeDialectTypeID::BASE;
  }
  static bool is_impl(DynObjRef ref) {
    return ref.getType() == TYPE_BASE ||
           (ref.getType() == TYPE_GENERIC &&
            ref.as<TypeRef>().typeID() == TypeDialectTypeID::BASE);
  }

  operator FatTypeRef() { return FatTypeRef{*this, nullptr}; }
};

struct StructTypeObj {
  struct StructElem {
    TypeRef type;
    uint32_t ident;

    bool operator==(const StructElem &o) const {
      return type == o.type && ident == o.ident;
    }
  };

  StructTypeObj(FatObjRef<StructTypeObj> o) : elemns(o->elemns) {}
  StructTypeObj(SmallVec<StructElem, 16> &&elemns)
      : elemns(std::move(elemns)) {}
  StructTypeObj(DynObjRef, const StructTypeObj &o) : elemns(o.elemns) {};
  SmallVec<StructElem, 16> elemns;
  StructTypeObj() = default;

  static uint32_t hash(const StructTypeObj &o) {
    uint64_t hash = 0;
    for (auto &elem : o.elemns)
      hash = hash_combine64(hash, hash_u64(std::bit_cast<uint64_t>(elem)));
    return hash_reduce_64_to_32(hash);
  }
  bool operator==(const StructTypeObj &o) const {
    return Range{elemns}.equals(Range{o.elemns});
  }
};

class StructTypeRef : public FatObjRef<StructTypeObj> {
public:
  using FatObjRef::FatObjRef;
  explicit constexpr StructTypeRef(FatObjRef ref) : FatObjRef(ref) {
    assert(is_impl(ref));
  }
  static bool is_impl(TypeRef ref) {
    return ref.typeID() == TypeDialectTypeID::STRUCT;
  }
  explicit constexpr StructTypeRef(FatObjRef<TypeObj> ref)
      : FatObjRef(ref.getObjID(), ref.getPtr()) {
    assert(is_impl(ref));
  }
  static bool is_impl(DynObjRef ref) {
    return ref.getType() == TYPE_STRUCT ||
           (ref.getType() == TYPE_GENERIC &&
            ref.as<TypeRef>().typeID() == TypeDialectTypeID::STRUCT);
  }

  operator TypeRef() { return TypeRef{getObjID()}; }
  operator FatTypeRef() { return FatTypeRef{TypeRef(*this), (TypeObj *)ptr}; }
};

template <> struct ObjTraits<StructTypeObj> {
  static constexpr DialectType ty{TYPE_STRUCT};
  using FatRefT = StructTypeRef;
};

struct EnumTypeObj {
  struct EnumElemn {
    DynObjRef value;
    uint32_t ident;

    bool operator==(const EnumElemn &o) const {
      return value == o.value && ident == o.ident;
    }
  };

  EnumTypeObj(FatObjRef<EnumTypeObj> o)
      : underlying(o->underlying), elemns(o->elemns) {}
  EnumTypeObj(FatTypeRef underlying, SmallVec<EnumElemn, 16> &&elemns)
      : underlying(underlying), elemns(std::move(elemns)) {}
  EnumTypeObj(DynObjRef, const EnumTypeObj &o)
      : underlying(o.underlying), elemns(o.elemns) {};

  EnumTypeObj() = default;

  FatTypeRef underlying;

  SmallVec<EnumElemn, 16> elemns;

  static uint32_t hash(const EnumTypeObj &o) {
    uint64_t hash = 0;
    for (auto &elem : o.elemns) {
      hash =
          hash_combine64(hash, hash_u64(std::bit_cast<uint64_t>(elem.value)));
      hash =
          hash_combine64(hash, hash_u64(std::bit_cast<uint32_t>(elem.ident)));
    }
    return hash_reduce_64_to_32(hash);
  }
  bool operator==(const EnumTypeObj &o) const {
    return underlying == o.underlying && Range{elemns}.equals(Range{o.elemns});
  }
};

class EnumTypeRef : public FatObjRef<EnumTypeObj> {
public:
  using FatObjRef::FatObjRef;
  explicit constexpr EnumTypeRef(FatObjRef ref) : FatObjRef(ref) {
    assert(is_impl(ref));
  }
  static bool is_impl(TypeRef ref) {
    return ref.typeID() == TypeDialectTypeID::ENUM;
  }
  explicit constexpr EnumTypeRef(FatObjRef<TypeObj> ref)
      : FatObjRef(ref.getObjID(), ref.getPtr()) {
    assert(is_impl(ref));
  }
  static bool is_impl(DynObjRef ref) {
    return ref.getType() == TYPE_ENUM ||
           (ref.getType() == TYPE_GENERIC &&
            ref.as<TypeRef>().typeID() == TypeDialectTypeID::ENUM);
  }

  operator TypeRef() { return TypeRef{getObjID()}; }
  operator FatTypeRef() { return FatTypeRef{TypeRef(*this), (TypeObj *)ptr}; }
};

template <> struct ObjTraits<EnumTypeObj> {
  static constexpr DialectType ty{TYPE_ENUM};
  using FatRefT = EnumTypeRef;
};

struct ArrayTypeObj {
  ArrayTypeObj(FatObjRef<ArrayTypeObj> o)
      : start(o->start), len(o->len), element(o->element) {}
  ArrayTypeObj(DynObjRef start, DynObjRef len, TypeRef elem)
      : start(start), len(len), element(elem) {}
  ArrayTypeObj(DynObjRef, const ArrayTypeObj &o)
      : start(o.start), len(o.len), element(o.element) {};
  ArrayTypeObj() = default;

  DynObjRef start;
  DynObjRef len;
  TypeRef element;

  static uint32_t hash(const ArrayTypeObj &o) {
    uint64_t hash = hash_u64(std::bit_cast<uint64_t>(o.start));
    hash = hash_combine64(hash, hash_u64(std::bit_cast<uint64_t>(o.len)));
    hash = hash_combine64(hash, hash_u64(std::bit_cast<uint32_t>(o.element)));
    return hash_reduce_64_to_32(hash);
  }

  bool operator==(const ArrayTypeObj &o) const {
    return start == o.start && len == o.len && element == o.element;
  }
};

class ArrayTypeRef : public FatObjRef<ArrayTypeObj> {
public:
  using FatObjRef::FatObjRef;
  explicit constexpr ArrayTypeRef(FatObjRef ref) : FatObjRef(ref) {
    assert(is_impl(ref));
  }
  explicit constexpr ArrayTypeRef(FatObjRef<TypeObj> ref)
      : FatObjRef(ref.getObjID(), ref.getPtr()) {
    assert(is_impl(ref));
  }
  static bool is_impl(TypeRef ref) {
    return ref.typeID() == TypeDialectTypeID::ARRAY;
  }
  static bool is_impl(DynObjRef ref) {
    return ref.getType() == TYPE_ARRAY ||
           (ref.getType() == TYPE_GENERIC &&
            ref.as<TypeRef>().typeID() == TypeDialectTypeID::ARRAY);
  }

  operator TypeRef() { return TypeRef{getObjID()}; }
  operator FatTypeRef() { return FatTypeRef{TypeRef(*this), (TypeObj *)ptr}; }
};

template <> struct ObjTraits<ArrayTypeObj> {
  static constexpr DialectType ty{TYPE_ARRAY};
  using FatRefT = ArrayTypeRef;
};

template <typename T, auto HashFunc = T::hash> class DedupeStore {
private:
  using Traits = ObjTraits<T>;
  using FatRefT = Traits::FatRefT;
  using CreateHookT = void (*)(FatRefT);
  using DestroyHookT = void (*)(FatRefT);

  DenseMultimap<Unhashed<uint32_t>, ObjRef<T>> map;
  NewDeleteObjStore<T> store;

  auto hash(const T &t) {
    auto h = HashFunc(t);
    // we can't use empty/tombstone, so just remap to zero. This should
    // basically never fire so should be fine, otherwise we can go to 31 bit
    // hash.
    if (h == DenseMapInfo<uint32_t>::getEmptyKey() ||
        h == DenseMapInfo<uint32_t>::getTombstoneKey()) [[unlikely]] {
      h = 0;
    }
    return h;
  }
  auto find(uint32_t hash, const T &t) {
    auto it = map.find(hash);
    for (; it != map.end(); it = map.find_next(it)) {
      auto fat = store.resolve(it.val());
      if (*fat == t) [[likely]]
        return it;
    }
    return map.end();
  }

public:
  static constexpr DialectType ty{Traits::ty};
  using value_type = T;
  FatRefT create(const T &t) {
    auto h = hash(t);
    if (auto it = find(h, t); it != map.end())
      return store.resolve(it.val());

    auto ref = store.create(t);
    map.insert(h, ref);
    return ref;
  }

  uint32_t numIDs() { return store.size(); }

  FatRefT resolve(ObjRef<T> ref) { return store.resolve(ref); }

  void destroy(FatRefT ref) {
    if (auto it = find(hash(*ref), ref); it != map.end())
      map.erase(it);
    else
      dyno_unreachable("not in map?");
    store.destroy(ref);
  }

  void clear() {
    map.clear();
    store.clear();
  }
};

template <typename Base, uint32_t Shift, uint32_t Offs> class RemapStore {
public:
  static constexpr DialectType ty{Base::ty};
  using Traits = ObjTraits<typename Base::value_type>;
  using FatRefT = Traits::FatRefT;
  using ThinRefT = ObjRef<typename Base::value_type>;
  using value_type = Base::value_type;

private:
  Base base;

public:
  static FatRefT tfOut(FatRefT ref) {
    return FatRefT{ObjID((ref.getObjID() << Shift) + Offs), ref.getPtr()};
  }
  static FatRefT tfIn(FatRefT ref) {
    assert((ref.getObjID() & bit_mask_ones<uint32_t>(Shift)) == Offs &&
           "passed in type with wrong shift, wrong subtype?");
    return FatRefT{ObjID(ref.getObjID() >> Shift), ref.getPtr()};
  }
  static ThinRefT tfIn(ThinRefT ref) {
    assert((ref.getObjID() & bit_mask_ones<uint32_t>(Shift)) == Offs &&
           "passed in type with wrong shift, wrong subtype?");
    return ThinRefT{ObjID(ref.getObjID() >> Shift)};
  }
  template <typename... Args> FatRefT create(Args &&...args) {
    return tfOut(base.create(std::forward<Args>(args)...));
  }
  void destroy(FatRefT ref) { base.destroy(tfIn(ref)); }
  FatRefT resolve(ObjRef<typename Base::value_type> ref) {
    return tfOut(base.resolve(tfIn(ref)));
  }

  auto objs() {
    return Range{base.begin(), base.end()}.transform(
        [](size_t, auto ref) { return tfOut(ref); });
  }

  auto begin() { return objs().begin(); }
  auto end() { return objs().end(); }

  // do not use this directly if possible, sparse
  ObjID::num_t numIDs() { return base.size() << Shift; }

  void reset() { base.reset(); }
};

class TypeObjStore {
public:
  static constexpr DialectType ty{TYPE_GENERIC};
  static constexpr uint32_t typeBits = 2;
  static constexpr uint32_t typeMask = bit_mask_ones<uint32_t>(typeBits);

public:
  using value_type = TypeObj;

  RemapStore<DedupeStore<StructTypeObj>, 2, 0> structStore;
  RemapStore<DedupeStore<EnumTypeObj>, 2, 1> enumStore;
  RemapStore<DedupeStore<ArrayTypeObj>, 2, 2> arrayStore;

  FatTypeRef resolve(ObjRef<TypeObj> ref) {
    switch (ref.getObjID() & typeMask) {
    case 0:
      return FatTypeRef{
          structStore.resolve(ObjRef<StructTypeObj>(ref.getObjID()))};
    case 1:
      return FatTypeRef{enumStore.resolve(ObjRef<EnumTypeObj>(ref.getObjID()))};
    case 2:
      return FatTypeRef{
          arrayStore.resolve(ObjRef<ArrayTypeObj>(ref.getObjID()))};
    default:
      dyno_unreachable("invalid type obj id");
    }
  }
};

}; // namespace dyno
