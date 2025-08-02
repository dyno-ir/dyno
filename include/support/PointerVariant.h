#pragma once

#include "support/Bits.h"
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <type_traits>
#include <utility>

template <typename Ptr, unsigned LowBitsAvail, unsigned IntBits>
class PointerIntPairBase {
protected:
  uintptr_t base;

public:
  using Value = BitField<uintptr_t, IntBits, LowBitsAvail - IntBits>;
  using CValue = BitField<const uintptr_t, IntBits, LowBitsAvail - IntBits>;
  static constexpr uintptr_t PointerMask =
      bit_mask_zeros<uintptr_t>(LowBitsAvail);

  Ptr getPtr() { return reinterpret_cast<Ptr>(getPtrAsInt()); }
  uintptr_t getPtrAsInt() { return base & PointerMask; }
  auto getInt() const { return CValue{base}; }
  void setInt(unsigned val) { Value{base} = val; }
  void setPtr(Ptr ptr) { setPtrAsInt(reinterpret_cast<uintptr_t>(ptr)); }
  void setPtrAsInt(uintptr_t val) {
    assert(!(val & ~PointerMask) && "num low bits avail incorrect");
    base |= (val & PointerMask);
  }
  void clear() { base = 0; }
};

template <typename T> struct PointerIntTraits;
template <> struct PointerIntTraits<void *> {
  // assuming malloc alignment
  static constexpr unsigned numLowBitsAvail = 2;
};
template <typename T> struct PointerIntTraits<T *> {
  static constexpr unsigned numLowBitsAvail = clog2(alignof(T) - 1);
};

// template <typename Ptr, unsigned IntBits>
// class PointerIntPair
//     : public PointerIntPairBase<Ptr, PointerIntTraits<Ptr>::numLowBitsAvail,
//                                 IntBits> {};

template <typename Head, typename... Tail> struct PointerIntsVariantTraits {
  static constexpr size_t MinNumLowBitsAvail =
      std::min(PointerIntsVariantTraits<Head>::MinNumLowBitsAvail,
               PointerIntsVariantTraits<Tail...>::MinNumLowBitsAvail);
};
template <typename Head> struct PointerIntsVariantTraits<Head> {
  static constexpr size_t MinNumLowBitsAvail =
      PointerIntTraits<Head>::numLowBitsAvail;
};

template <typename T>
concept IntegralOrEnum = std::is_integral_v<T> || std::is_enum_v<T>;

template <IntegralOrEnum T> struct PointerIntsVariantTraits<T> {
  static_assert(bit_mask_sz<std::make_unsigned_t<T>> < 64,
                "cannot encode 64 bit integer.");
  static constexpr size_t MinNumLowBitsAvail =
      64 - bit_mask_sz<std::make_unsigned_t<T>>;
};

template <typename... Types> class PointersIntsVariant {
  static constexpr size_t NumArgs = sizeof...(Types);
  static constexpr size_t NumVarBits = clog2(NumArgs);
  static constexpr size_t LowBitsAvail =
      PointerIntsVariantTraits<Types...>::MinNumLowBitsAvail;
  PointerIntPairBase<void *, LowBitsAvail, NumVarBits> base;

  template <typename T> static constexpr size_t type_index() {
    return type_index_impl<T, Types...>(
        std::make_index_sequence<sizeof...(Types)>{});
  }

  template <typename T, typename... Ts, size_t... Is>
  static constexpr size_t type_index_impl(std::index_sequence<Is...>) {
    size_t result = 0;
    bool found = ((std::is_same_v<T, Ts> ? (result = Is, true) : false) || ...);
    assert(found);
    return result;
  }

public:
  template <typename T> PointersIntsVariant &operator=(T t) {
    base.clear();
    base.setInt(type_index<T>());
    if constexpr (std::is_pointer_v<T>)
      base.setPtr(reinterpret_cast<void *>(t));
    else
      base.setPtrAsInt(uintptr_t(t) << LowBitsAvail);
    return *this;
  }

  template <typename T> bool operator==(T t) {
    if (!is<T>())
      return false;
    return as<T>() == t;
  }

  template <typename T> PointersIntsVariant(T t) { *this = t; }
  PointersIntsVariant(std::nullptr_t) { base.base = 0; }

  template <typename T> bool is() const {
    return base.getInt() == type_index<T>();
  }

  template <typename T> T as() {
    assert(is<T>());
    if constexpr (std::is_pointer_v<T>)
      return reinterpret_cast<T>(base.getPtr());
    else
      return T(base.getPtrAsInt() >> LowBitsAvail);
  }

  template <typename T> std::optional<T> dyn_as() {
    if (!is<T>())
      return std::nullopt;
    return as<T>();
  }
};

// template <typename T, unsigned IntBits>
// struct PointerIntTraits<PointerIntPair<T, IntBits>> {
//   static constexpr unsigned numLowBitsAvail =
//       PointerIntTraits<T>::numLowBitsAvail - IntBits;
// };
