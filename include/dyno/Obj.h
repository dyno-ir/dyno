#pragma once

#include "support/Bits.h"
#include "support/DenseMapInfo.h"
#include "support/RTTI.h"
#include <bit>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <type_traits>
namespace dyno {

template <typename T> struct ObjTraits;

template <typename NumT> class IDImpl {
public:
  static constexpr IDImpl invalid() { return IDImpl{bit_mask_ones<NumT>()}; }
  static const inline IDImpl INVALID = invalid();

  using num_t = NumT;
  num_t num;

  constexpr IDImpl() = default;
  constexpr explicit IDImpl(num_t num) : num(num) {}

  operator num_t() { return num; }

  explicit operator bool() const { return *this != invalid(); }

  constexpr bool operator==(IDImpl o) const { return num == o.num; }

  template <typename... T> bool anyOf(T... ids) {
    for (auto id : {ids...}) {
      if (id == num)
        return true;
    }
    return false;
  }
};

using DialectID = IDImpl<uint8_t>;
using TyID = IDImpl<uint8_t>;
using ObjID = IDImpl<uint32_t>;
using InterfaceID = IDImpl<uint16_t>;

const inline TyID::num_t TY_DEF_USE_START = bit_mask_msb<TyID::num_t>();

class DynObjRef;
template <typename T = void> class FatDynObjRef;
template <typename T> class ObjRef;
template <typename T>
  requires(!std::is_void_v<T>)
class FatObjRef;

template <typename T>
concept IsFatDynObjRef = (requires {
  typename T::value_type;
} && std::derived_from<T, FatDynObjRef<typename T::value_type>>);

template <typename T>
concept IsDynObjRef = std::derived_from<T, DynObjRef>;

template <typename T>
concept IsFatObjRef = (requires { typename T::value_type; } &&
                       std::derived_from<T, FatObjRef<typename T::value_type>>);

template <typename T>
concept IsObjRef = (requires { typename T::value_type; } &&
                    std::derived_from<T, ObjRef<typename T::value_type>>);

template <typename T>
concept IsPureObjRef = IsObjRef<T> && !IsFatObjRef<T>;

template <typename T>
concept IsPureDynObjRef = IsDynObjRef<T> && !IsFatDynObjRef<T>;

template <typename T>
concept IsAnyDynRef = IsDynObjRef<T> || IsFatDynObjRef<T>;

template <typename T>
concept IsAnyFatRef = IsFatObjRef<T> || IsFatDynObjRef<T>;

template <typename T>
concept IsAnyObjRef =
    IsDynObjRef<T> || IsFatDynObjRef<T> || IsObjRef<T> || IsFatObjRef<T>;

/// Note: Can be uninitialized!
template <typename T>
class ObjRef : public ByValueRTTIUtilMixin<ObjRef<T>>, ByValueRTTITag {
protected:
  ObjID obj;

public:
  using Traits = ObjTraits<T>;
  using value_type = T;

  ObjRef() {}
  ObjRef(nullref_t) : obj(ObjID::INVALID) {}
  explicit constexpr ObjRef(ObjID obj) : obj(obj) {}

  explicit operator bool() const { return static_cast<bool>(obj); }

  DialectID getDialectID() { return Traits::dialect; }
  TyID getTyID() { return Traits::ty; }
  ObjID getObjID() { return obj; }

  static bool is_impl(const DynObjRef &Ref);
};

/// Note: Can be uninitialized!
class alignas(uint64_t) DynObjRef : public ByValueRTTIUtilMixin<DynObjRef>,
                                    ByValueRTTITag {
public:
  template <unsigned N, unsigned Pos>
  using CustomField = BitField<uint16_t, N, Pos>;

protected:
  DialectID dialect;
  TyID ty;
  uint16_t custom;
  ObjID obj;

  template <typename FieldT> FieldT customField() { return FieldT{custom}; }
  template <typename FieldT> const FieldT customField() const {
    return FieldT{const_cast<uint16_t &>(custom)};
  }

public:
  template <typename T> static DynObjRef ofTy(uint16_t custom = 0) {
    return {ObjTraits<T>::dialect, ObjTraits<T>::ty, ObjID::INVALID, custom};
  }

  template <typename T> static DynObjRef ofObj(ObjID obj, uint16_t custom = 0) {
    return {ObjTraits<T>::dialect, ObjTraits<T>::ty, obj, custom};
  }

  DynObjRef() = default;
  constexpr DynObjRef(nullref_t)
      : dialect(0), ty(0), custom(0), obj(ObjID::invalid()) {}

  static DynObjRef invalid() { return nullref; };

  template <typename T>
  DynObjRef(ObjRef<T> ref) : DynObjRef(ofObj<T>(ref.getObjID())) {}

  constexpr DynObjRef(DialectID dialect, TyID ty, ObjID obj, uint16_t custom)
      : dialect(dialect), ty(ty), custom(custom), obj(obj) {}

  bool isCustom() const { return custom != 0; }

  void setCustom(uint16_t custom) { this->custom = custom; }
  void clearCustom() { custom = 0; }

  explicit operator bool() const { return isCustom() || bool(obj); }

  DialectID getDialectID() const { return dialect; }
  TyID getTyID() const { return ty; }
  ObjID getObjID() const { return obj; }
  uint16_t getCustom() const { return custom; }

  friend bool operator==(const DynObjRef &a, const DynObjRef &b) {
    return a.custom == b.custom && a.dialect == b.dialect && a.ty == b.ty &&
           a.obj == b.obj;
  }

  // always true, we can support arbitrary ObjRefs
  template <typename T> static bool is_impl(ObjRef<T>) { return true; }

  template <IsPureObjRef T> explicit operator T() const {
    assert((::is<T, DynObjRef>(*this)));
    return T{obj};
  }
  template <IsPureDynObjRef T> explicit operator T() const {
    assert((::is<T, DynObjRef>(*this)));
    return T{obj};
  }

  // FatDynObjRef<> fat();
};

/// Note: Can be uninitialized!
template <typename T>
  requires(!std::is_void_v<T>)
class FatObjRef : public ObjRef<T>, public ByValueRTTIUtilMixin<FatObjRef<T>> {
public:
  template <unsigned N, unsigned Pos>
  using CustomField = BitField<uint16_t, N, Pos>;
  template <unsigned N, unsigned Pos>
  using SpecialField = BitField<uint16_t, N, Pos>;

protected:
  uint16_t custom;
  uint16_t special;
  T *ptr;

  template <typename FieldT> FieldT customField() { return FieldT{custom}; }
  template <typename FieldT> const FieldT customField() const {
    return FieldT{const_cast<uint16_t &>(custom)};
  }

public:
  using value_type = T;
  using ByValueRTTIUtilMixin<FatObjRef<T>>::as;
  using ByValueRTTIUtilMixin<FatObjRef<T>>::dyn_as;
  using ByValueRTTIUtilMixin<FatObjRef<T>>::is;
  FatObjRef() = default;
  FatObjRef(nullref_t)
      : ObjRef<T>(nullref), custom(0), special(0), ptr(nullptr) {}
  // FatObjRef(const FatObjRef &other) = default;
  // FatObjRef& operator=(const FatObjRef& rhs) {
  //   this->obj = rhs.obj;
  //   this->custom = rhs.custom;
  //   this->special = rhs.special;
  //   this->ptr = rhs.ptr;
  //   return *this;
  // }

  FatObjRef(ObjRef<T> ref, T *ptr, uint16_t custom = 0)
      : ObjRef<T>(ref), custom(custom), special(0), ptr(ptr) {}
  FatObjRef(ObjRef<T> ref, T &ptr, uint16_t custom = 0)
      : ObjRef<T>(ref), custom(custom), special(0), ptr(&ptr) {}
  FatObjRef(ObjID obj, T *ptr, uint16_t custom = 0)
      : ObjRef<T>(obj), custom(custom), special(0), ptr(ptr) {}
  FatObjRef(ObjID obj, T &ptr, uint16_t custom = 0)
      : ObjRef<T>(obj), custom(custom), special(0), ptr(&ptr) {}
  FatObjRef(ObjID obj, void *ptr, uint16_t custom = 0)
      : ObjRef<T>(obj), custom(custom), special(0),
        ptr(reinterpret_cast<T *>(ptr)) {}

  static bool is_impl(const DynObjRef &Ref);

  bool isCustom() const { return custom != 0; }

  void setCustom(uint16_t custom) { this->custom = custom; }
  void clearCustom() { custom = 0; }

  explicit operator bool() const { return isCustom() || bool(this->obj); }

  T *getPtr() const { return ptr; }
  T &operator*() const {
    assert(ptr && "ptr uninitialized");
    return *ptr;
  }
  T *operator->() const {
    assert(ptr && "ptr uninitialized");
    return ptr;
  }
  uint16_t getCustom() const { return custom; }

  template <IsFatDynObjRef U> explicit operator U() const {
    assert((::is<U, FatObjRef>(*this)));
    return U{DynObjRef::ofObj<typename U::value_type>(this->obj, custom),
             reinterpret_cast<U::value_type *>(ptr)};
  }
};
static_assert(sizeof(FatObjRef<int>) == 16);

/// Note: Can be uninitialized!
template <typename T>
class FatDynObjRef : public DynObjRef, public ByValueRTTIUtilMixin<FatDynObjRef<T>> {
protected:
  T *ptr;

public:
  using value_type = T;
  using ByValueRTTIUtilMixin<FatDynObjRef<T>>::as;
  using ByValueRTTIUtilMixin<FatDynObjRef<T>>::dyn_as;
  using ByValueRTTIUtilMixin<FatDynObjRef<T>>::is;
  FatDynObjRef() = default;
  FatDynObjRef(nullref_t) : DynObjRef(nullref), ptr(nullptr) {}
  FatDynObjRef(DynObjRef ref, T *ptr) : DynObjRef(ref), ptr(ptr) {}
  template <typename U = T, typename = std::enable_if_t<!std::is_void_v<U>>>
  FatDynObjRef(DynObjRef ref, U &ptr) : DynObjRef(ref), ptr(&ptr) {}
  template <typename U = T, typename = std::enable_if_t<!std::is_void_v<U>>>
  FatDynObjRef(FatObjRef<U> ref)
      : DynObjRef(DynObjRef::ofObj<U>(ref.getObjID(), ref.getCustom())),
        ptr(reinterpret_cast<T *>(ref.getPtr())) {}

  template <typename V, typename U = T,
            typename = std::enable_if_t<std::is_void_v<U>>>
  FatDynObjRef(FatDynObjRef<V> ref) : DynObjRef(ref), ptr(ref.getPtr()) {}

  template <typename U> static bool is_impl(ObjRef<U>) { return true; }

  T *getPtr() const { return ptr; }
  template <typename U = T, typename = std::enable_if_t<!std::is_void_v<U>>>
  U &operator*() const {
    assert(ptr && "ptr uninitialized");
    return *ptr;
  }
  template <typename U = T, typename = std::enable_if_t<!std::is_void_v<U>>>
  U *operator->() const {
    assert(ptr && "ptr uninitialized");
    return ptr;
  }

  template <typename U> explicit operator FatDynObjRef<U>() {
    return FatDynObjRef<U>{*static_cast<DynObjRef *>(this),
                           reinterpret_cast<U *>(ptr)};
  }

  template <IsFatObjRef U> explicit operator U() const {
    assert((::is<U, FatDynObjRef>(*this)));
    return U{obj, ptr, custom};
  }
  template <IsFatDynObjRef U> explicit operator U() const {
    assert((::is<U, FatDynObjRef>(*this)));
    return U{*this, reinterpret_cast<U::value_type *>(ptr)};
  }
#ifdef __clang__
  // these exist in DynObjRef, clang needs duplicate tho
  template <IsPureObjRef U> explicit operator U() const {
    assert((::is<U, FatDynObjRef>(*this)));
    return U{obj};
  }
  template <IsPureDynObjRef U> explicit operator U() const {
    assert((::is<U, FatDynObjRef>(*this)));
    return U{*this};
  }
#endif

  static bool is_impl(FatDynObjRef<>) { return true; }
};

template <typename T> struct ObjTraits {
  /*static constexpr DialectID dialect{0};*/
  /*static constexpr TyID ty{0};*/
  /*using RefT = ObjRef;*/
  /*using FatRefT = FatDynObjRef<T>;*/
};

template <typename Derived, typename T> class TrailingObjArr {
private:
  Derived &derived() { return static_cast<Derived &>(*this); }
  const Derived &derived() const { return static_cast<const Derived &>(*this); }

protected:
  TrailingObjArr() { static_assert(alignof(T) <= alignof(Derived)); }
  T *trailing() { return reinterpret_cast<T *>(&derived() + 1); }
  const T *trailing() const {
    return reinterpret_cast<const T *>(&derived() + 1);
  }

public:
  static constexpr size_t getAllocSize(size_t n) {
    return sizeof(Derived) + n * sizeof(T);
  }

  size_t getAllocSize() { return getAllocSize(derived().getNumTrailing()); }
};
static_assert(sizeof(FatDynObjRef<>) == 16);

template <typename T>
concept TrailingObj = requires(T x) {
  T::getAllocSize(size_t{0});
  x.getAllocSize();
};

template <typename T> bool ObjRef<T>::is_impl(const DynObjRef &Ref) {
  return Ref.getDialectID() == Traits::dialect && Ref.getTyID() == Traits::ty;
}

template <typename T>
  requires(!std::is_void_v<T>)
bool FatObjRef<T>::is_impl(const DynObjRef &Ref) {
  return Ref.getDialectID() == ObjTraits<T>::dialect &&
         Ref.getTyID() == ObjTraits<T>::ty;
}

// FatDynObjRef<> DynObjRef::fat() {
//   return FatDynObjRef<>{*this, GlobalResolver::resolve(dialect, ty)};
// }

} // namespace dyno

template <> struct std::hash<dyno::DynObjRef> {
  size_t operator()(const dyno::DynObjRef &ref) const {
    return std::bit_cast<size_t>(ref);
  }
};

template <> struct DenseMapInfo<dyno::DynObjRef> {
  // we don't want to use nullref here, otherwise we can't store nullref in map.
  static constexpr dyno::DynObjRef getEmptyKey() {
    return dyno::DynObjRef{dyno::DialectID::invalid(), dyno::TyID::invalid(),
                           dyno::ObjID::invalid(), 2};
  }
  static constexpr dyno::DynObjRef getTombstoneKey() {
    return dyno::DynObjRef{dyno::DialectID::invalid(), dyno::TyID::invalid(),
                           dyno::ObjID::invalid(), 1};
  }
  static unsigned getHashValue(const dyno::DynObjRef &k) {
    return std::hash<dyno::DynObjRef>()(k);
  }
  static unsigned isEqual(const dyno::DynObjRef &lhs,
                          const dyno::DynObjRef &rhs) {
    return std::bit_cast<uint64_t>(lhs) == std::bit_cast<uint64_t>(rhs);
  }
};
