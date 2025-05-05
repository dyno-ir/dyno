#pragma once

#include <bit>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <support/Bits.h>
#include <support/RTTI.h>
#include <type_traits>
namespace dyno {

template <typename T> struct ObjTraits;

template <typename NumT> class IDImpl {
public:
  static inline IDImpl INVALID = IDImpl{bit_mask_ones<NumT>()};

  using num_t = NumT;
  num_t num;

  IDImpl() {}
  constexpr explicit IDImpl(num_t num) : num(num) {}

  operator num_t() { return num; }

  explicit operator bool() const { return *this != INVALID; }

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

/// Note: Can be uninitialized!
template <typename T> class ObjRef : public RTTIUtilMixin<ObjRef<T>> {
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
class alignas(uint64_t) DynObjRef : public RTTIUtilMixin<DynObjRef> {
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
  template <typename T> static DynObjRef ofTy() {
    return {ObjTraits<T>::dialect, ObjTraits<T>::ty, ObjID::INVALID, 0};
  }

  template <typename T> static DynObjRef ofObj(ObjID obj) {
    return {ObjTraits<T>::dialect, ObjTraits<T>::ty, obj, 0};
  }

  DynObjRef() {}
  DynObjRef(nullref_t) : dialect(0), ty(0), custom(0), obj(ObjID::INVALID) {}

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

  /*template <typename T, typename ResolverT>*/
  /*FatDynObjRef<T> fat(ResolverT &resolver) const {*/
  /*  assert(is<T>());*/
  /*  return {*this, resolver.get(*this)};*/
  /*}*/

  friend bool operator==(const DynObjRef &a, const DynObjRef &b) {
    return a.custom == b.custom && a.dialect == b.dialect && a.ty == b.ty &&
           a.obj == b.obj;
  }

  // always true, we can support arbitrary ObjRefs
  template <typename T> static bool is_impl(ObjRef<T>) { return true; }

  template <typename T> explicit operator ObjRef<T>() const {
    assert((::is<ObjRef<T>, DynObjRef>(*this)));
    return ObjRef<T>{obj};
  }

  template <typename T> explicit operator T() const {
    assert((::is<T, DynObjRef>(*this)));
    return T{obj};
  }

  // FatDynObjRef<> fat();
};
static_assert(sizeof(DynObjRef) == 8);

/// Note: Can be uninitialized!
template <typename T>
  requires(!std::is_void_v<T>)
class FatObjRef : public ObjRef<T>, public RTTIUtilMixin<FatObjRef<T>> {
protected:
  T *ptr;

public:
  using value_type = T;
  using RTTIUtilMixin<FatObjRef<T>>::as;
  using RTTIUtilMixin<FatObjRef<T>>::dyn_as;
  using RTTIUtilMixin<FatObjRef<T>>::is;
  FatObjRef() {}
  FatObjRef(nullref_t) : ObjRef<T>(nullref), ptr(nullptr) {}
  FatObjRef(ObjRef<T> ref, T *ptr) : ObjRef<T>(ref), ptr(ptr) {}
  FatObjRef(ObjRef<T> ref, T &ptr) : ObjRef<T>(ref), ptr(&ptr) {}
  FatObjRef(ObjID obj, T *ptr) : ObjRef<T>(obj), ptr(ptr) {}
  FatObjRef(ObjID obj, T &ptr) : ObjRef<T>(obj), ptr(&ptr) {}
  FatObjRef(ObjID obj, void *ptr)
      : ObjRef<T>(obj), ptr(reinterpret_cast<T *>(ptr)) {}

  static bool is_impl(const DynObjRef &Ref);

  T *getPtr() const { return ptr; }
  T &operator*() const {
    assert(ptr && "ptr uninitialized");
    return *ptr;
  }
  T *operator->() const {
    assert(ptr && "ptr uninitialized");
    return ptr;
  }
};

/// Note: Can be uninitialized!
template <typename T>
class FatDynObjRef : public DynObjRef, public RTTIUtilMixin<FatDynObjRef<T>> {
protected:
  T *ptr;

public:
  using value_type = T;
  using RTTIUtilMixin<FatDynObjRef<T>>::as;
  using RTTIUtilMixin<FatDynObjRef<T>>::dyn_as;
  using RTTIUtilMixin<FatDynObjRef<T>>::is;
  FatDynObjRef() {}
  FatDynObjRef(nullref_t) : DynObjRef(nullref), ptr(nullptr) {}
  FatDynObjRef(DynObjRef ref, T *ptr) : DynObjRef(ref), ptr(ptr) {}
  template <typename U = T, typename = std::enable_if_t<!std::is_void_v<U>>>
  FatDynObjRef(DynObjRef ref, U &ptr) : DynObjRef(ref), ptr(&ptr) {}
  template <typename U = T, typename = std::enable_if_t<!std::is_void_v<U>>>
  FatDynObjRef(FatObjRef<U> ref) : DynObjRef(ref), ptr(ref.getPtr()) {}

  template <typename V, typename U = T,
            typename = std::enable_if_t<std::is_void_v<U>>>
  FatDynObjRef(FatDynObjRef<V> ref) : DynObjRef(ref), ptr(ref.getPtr()) {}

  FatDynObjRef(FatDynObjRef<> ref)
      : DynObjRef(ref), ptr(reinterpret_cast<T *>(ref.getPtr())) {}

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

  template <typename U> explicit operator FatObjRef<U>() const {
    assert((::is<FatObjRef<U>, FatDynObjRef>(*this)));
    return FatObjRef<U>{ObjRef<U>{obj}, reinterpret_cast<U *>(ptr)};
  }
  template <typename U> explicit operator U() const {
    assert((::is<U, FatDynObjRef>(*this)));
    return U{obj, ptr};
  }
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

// template <typename T>
// concept IsFatDynObjRef = (requires {
//   typename T::value_type;
// } && std::is_same_v<T, FatDynObjRef<typename T::value_type>>);

// template <typename T>
// concept IsDynObjRef = std::derived_from<T, DynObjRef>;

// template <typename T>
// concept IsFatObjRef = (requires { typename T::value_type; } &&
//                        std::derived_from<T, FatObjRef<typename
//                        T::value_type>>);

// template <typename T>
// concept IsObjRef = (requires { typename T::value_type; } &&
//                     std::derived_from<T, ObjRef<typename T::value_type>>);

// template <typename T>
// concept IsAnyDynRef = IsDynObjRef<T> || IsFatDynObjRef<T>;

// template <typename T>
// concept IsAnyFatRef = IsFatObjRef<T> || IsFatDynObjRef<T>;

// template <typename T>
// concept IsAnyObjRef = IsDynObjRef<T> || IsFatDynObjRef<T> || IsObjRef<T> ||
// IsFatObjRef<T>;

// FatDynObjRef<> DynObjRef::fat() {
//   return FatDynObjRef<>{*this, GlobalResolver::resolve(dialect, ty)};
// }

} // namespace dyno

template <> struct std::hash<dyno::DynObjRef> {
  size_t operator()(const dyno::DynObjRef &ref) const {
    return std::bit_cast<size_t>(ref);
  }
};

template <typename T> struct IsByValueRTTI<dyno::ObjRef<T>> : std::true_type {};

template <typename T>
struct IsByValueRTTI<dyno::FatObjRef<T>> : std::true_type {};

template <> struct IsByValueRTTI<dyno::DynObjRef> : std::true_type {};

template <typename T>
struct IsByValueRTTI<dyno::FatDynObjRef<T>> : std::true_type {};
