#pragma once

#include <bit>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <dyno/IDs.h>
#include <functional>
#include <string_view>
#include <support/Bits.h>

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
};

using DialectID = IDImpl<uint8_t>;
using TyID = IDImpl<uint8_t>;
using ObjID = IDImpl<uint32_t>;
using InterfaceID = IDImpl<uint16_t>;

/// Note: Can be uninitialized!
template <typename T> class ObjRef {
protected:
  ObjID obj;

public:
  using Traits = ObjTraits<T>;

  ObjRef() {}
  explicit constexpr ObjRef(ObjID obj) : obj(obj) {}

  explicit operator bool() const { return static_cast<bool>(obj); }

  DialectID getDialectID() { return Traits::dialect; }
  TyID getTyID() { return Traits::ty; }
  ObjID getObjID() { return obj; }
};

/// Note: Can be uninitialized!
class alignas(uint64_t) DynObjRef {
public:
  template <unsigned N, unsigned Pos>
  using CustomField = BitField<uint16_t, N, Pos>;

protected:
  DialectID dialect;
  TyID ty;
  uint16_t custom;
  ObjID obj;

  template <typename FieldT> FieldT customField() { return FieldT{custom}; }

public:
  template <typename T> static DynObjRef ofTy() {
    return {ObjTraits<T>::dialect, ObjTraits<T>::ty, ObjID::INVALID, 0};
  }

  template <typename T> static DynObjRef ofObj(ObjID obj) {
    return {ObjTraits<T>::dialect, ObjTraits<T>::ty, obj, 0};
  }

  static DynObjRef invalid() {
    return DynObjRef{DialectID{0}, TyID{0}, ObjID::INVALID, 0};
  };

  DynObjRef() {}

  template <typename T>
  DynObjRef(ObjRef<T> ref) : DynObjRef(ofObj<T>(ref.getObjID())) {}

  constexpr DynObjRef(DialectID dialect, TyID ty, ObjID obj, uint16_t custom)
      : dialect(dialect), ty(ty), custom(custom), obj(obj) {}

  bool isCustom() const { return custom != 0; }

  void setCustom(uint16_t custom) { this->custom = custom; }
  void clearCustom() { custom = 0; }

  explicit operator bool() const { return isCustom() || bool(obj); }

  template <typename T> bool isTy() const {
    return dialect == ObjTraits<T>::dialect && ty == ObjTraits<T>::ty;
  }

  /*template <typename T> auto as() const {*/
  /*  assert(isTy<T>());*/
  /*  return ObjTraits<T>::RefT(*this);*/
  /*}*/

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
};
static_assert(sizeof(DynObjRef) == 8);

/// Note: Can be uninitialized!
template <typename T> class FatObjRef : public ObjRef<T> {
protected:
  T *ptr;

public:
  FatObjRef() {}
  FatObjRef(ObjRef<T> ref, T *ptr = nullptr) : ObjRef<T>(ref), ptr(ptr) {}
  FatObjRef(ObjRef<T> ref, T &ptr) : ObjRef<T>(ref), ptr(&ptr) {}

  /*template <typename TT = T> auto as() const {*/
  /*  assert(is<TT>());*/
  /*  using RefT = ObjTraits<TT>::FatRefT;*/
  /*  return RefT((*this));*/
  /*}*/

  T *getPtr() const { return ptr; }
  T &operator*() const { return *ptr; }
  T *operator->() const { return ptr; }
};

/// Note: Can be uninitialized!
template <typename T = void> class FatDynObjRef : public DynObjRef {
protected:
  T *ptr;

public:
  FatDynObjRef() {}
  FatDynObjRef(DynObjRef ref, T *ptr = nullptr) : DynObjRef(ref), ptr(ptr) {}
  FatDynObjRef(DynObjRef ref, T &ptr) : DynObjRef(ref), ptr(&ptr) {}
  FatDynObjRef(FatObjRef<T> ref) : DynObjRef(ref), ptr(ref.getPtr()) {}

  /*template <typename TT = T> auto as() const {*/
  /*  assert(is<TT>());*/
  /*  using RefT = ObjTraits<TT>::FatRefT;*/
  /*  return RefT((*this));*/
  /*}*/

  /*explicit operator bool() { return ptr; }*/

  T *getPtr() const { return ptr; }
  T &operator*() const { return *ptr; }
  T *operator->() const { return ptr; }
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

protected:
  TrailingObjArr() { static_assert(alignof(T) <= alignof(Derived)); }
  T *trailing() { return reinterpret_cast<T *>(&derived() + 1); }

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

} // namespace dyno

template <> struct std::hash<dyno::DynObjRef> {
  size_t operator()(const dyno::DynObjRef &ref) const {
    return std::bit_cast<size_t>(ref);
  }
};
