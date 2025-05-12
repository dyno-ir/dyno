#pragma once
#include "dyno/Obj.h"

namespace dyno {

template <typename T, typename... Types>
constexpr bool is_one_of = (std::is_same_v<T, Types> || ...);

template <IsAnyObjRef... Types> class FatRefUnion : public FatDynObjRef<> {
public:
  // template <typename T, typename = std::enable_if_t<is_one_of<T, Types...>>>
  // FatRefUnion(T ref) : FatDynObjRef(ref) {}
  FatRefUnion(nullref_t) : FatDynObjRef(nullref) {}
  FatRefUnion() = default;

  template <typename T> bool is_impl(T ref) {
    return (Types::is_impl(ref) || ...);
  }

  template <IsFatDynObjRef T> FatRefUnion(T ref) : FatDynObjRef(ref) {
    assert(is_impl(ref));
  }

  template <IsFatObjRef T> FatRefUnion(T ref) : FatDynObjRef(ref) {
    assert(is_impl(ref));
  }

  template <IsFatDynObjRef T> FatRefUnion &operator=(const T &val) {
    assert(is_impl(val));
    *static_cast<FatDynObjRef<> *>(this) = val;
    return *this;
  }
  template <IsFatObjRef T> FatRefUnion &operator=(const T &val) {
    assert(is_impl(val));
    *static_cast<FatDynObjRef<> *>(this) = val;
    return *this;
  }
};

}; // namespace dyno
