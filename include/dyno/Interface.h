#pragma once

#include "dyno/ObjMap.h"
#include "support/ArrayRef.h"
#include <array>
#include <dyno/Obj.h>
#include <type_traits>

namespace dyno {

template <typename T> class Interface;

template <typename T> struct InterfaceTraits {
  static const ArrayRef<T> dispatch1(DynObjRef ref,
                                     const ArrayRef<T> *interfaces) {
    return interfaces[ref.getDialectID()];
  }
  static const T &dispatch2(DynObjRef ref, ArrayRef<T> interface) {
    return interface.front();
  }
  static const unsigned ID = ~0;
};

template <typename T> class ArrayInterface {
private:
  using Traits = InterfaceTraits<T>;
  using Dispatch1T = decltype(Traits::dispatch1(nullref, nullptr));
  using Dispatch2T = decltype(Traits::dispatch2(nullref, Dispatch1T()));
  std::array<std::remove_const_t<Dispatch1T>, 256> entries = {};

public:
  constexpr ArrayInterface() = default;
  Dispatch1T operator[](DialectID dialect) { return entries[dialect]; }

  template <typename RefT> Dispatch2T operator[](RefT ref) {
    return Traits::dispatch2(
        ref,
        Traits::dispatch1(ref, const_cast<std::remove_const_t<Dispatch1T> *>(
                                   entries.data())));
  }

  constexpr void registerDialect(DialectID dialect, const Dispatch1T val) {
    entries[dialect] = val;
  }
};

template <typename T> class Interface {
private:
  using Traits = InterfaceTraits<T>;
  using Dispatch1T = decltype(Traits::dispatch1(nullref, nullptr));
  using Dispatch2T = decltype(Traits::dispatch2(nullref, Dispatch1T()));
  const Dispatch1T *entries;

public:
  constexpr Interface(const Dispatch1T *entries) : entries(entries) {}
  Dispatch1T operator[](DialectID dialect) { return entries[dialect]; }

  template <typename RefT> Dispatch2T operator[](RefT ref) {
    return Traits::dispatch2(
        ref, Traits::dispatch1(
                 ref, const_cast<std::remove_const_t<Dispatch1T> *>(entries)));
  }

  /*template <typename... Args> auto operator()(ObjRef ref, Args &&...args) {*/
  /*  return (*this->operator[](ref))(ref, std::forward<Args>(args)...);*/
  /*}*/
};

template <size_t NumDialects, typename... Types> class Interfaces {
  std::array<std::tuple<Types...>, NumDialects> arr = {};

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

  template <typename T> constexpr T &get(size_t index) {
    return std::get<type_index<T>()>(arr[index]);
  }
  template <typename T> constexpr const T &get(size_t index) const {
    return std::get<type_index<T>()>(arr[index]);
  }

public:
  template <typename T> constexpr void registerVal(size_t i, const T &t) {
    assert(i < NumDialects);
    get<T>(i) = t;
  }

  template <typename T> constexpr const T &getVal(size_t i) const {
    assert(i < NumDialects);
    return get<T>(i);
  }

  template <typename T> constexpr T &getVal(size_t i) {
    assert(i < NumDialects);
    return get<T>(i);
  }

  template <typename T, typename... Args>
  auto call(FatDynObjRef<> ref, Args &&...args) const {
    auto fn = getVal<T>(ref.getDialectID());
    assert(fn && "not registered");
    return fn(std::forward(args...));
  }
};

class DynInterfaces {
private:
  std::vector<void *> interfaces;
  unsigned numDialects;

public:
  DynInterfaces(unsigned numDialects, unsigned numInterfaces)
      : interfaces(numInterfaces * numDialects), numDialects(numDialects) {}

  template <typename T>
  void registerInterface(unsigned dialectID,
                         InterfaceTraits<T>::DispatchT entry) {
    using Traits = InterfaceTraits<T>;
    interfaces[numDialects * Traits::ID + dialectID] = entry;
  }

  template <typename T> Interface<T> get() {
    using Traits = InterfaceTraits<T>;
    return reinterpret_cast<Traits::DispatchT *>(
        &interfaces[numDialects * Traits::ID]);
  }
};

// Essentially a separate ObjMapVec<Type, Value> for each passed type.
// Combines sizeof...(Types) ObjMapVecs, statically. For each operation, selects
// the correct one for passed type with zero overhead.
template <typename Value, typename... Types> class StaticGenericObjVecMap {
  static constexpr size_t numTypes = sizeof...(Types);
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
  template <typename K> auto &map() { return std::get<type_index<K>()>(maps); }
  std::tuple<ObjMapVec<Types, Value>...> maps;

  template <typename K> void ensure(ObjRef<K> ref) {
    return map<K>().ensure(ref);
  }
  template <typename K> bool inRange(ObjRef<K> ref) {
    return map<K>().inRange(ref);
  }
  template <typename K> void reserve(size_t sz) { return map<K>().reserve(sz); }
  template <typename K> void resize(size_t sz) { return map<K>().resize(sz); }
  template <typename K> void clear() { return map<K>().clear(); }
  template <typename K> size_t size() { return map<K>().size(); }
  template <typename K> auto &operator[](ObjRef<K> ref) {
    return map<K>()[ref];
  }
  template <typename K> auto &get_ensure(ObjRef<K> ref) {
    return map<K>().get_ensure(ref);
  }
  template <typename K> auto begin() { return map<K>().begin(); }
  template <typename K> auto end() { return map<K>().end(); }
};

// load from 64k lookup table of dialect + type
// InterfaceTraits for type tells you what to do with the value

// e.g. resolving arbitrary DynObjRef to FatDynObjRef
// dialect + type lookup gives you ptr
// ptr must implement operator [] (you don't know the type obj store type but
// should still be generic...)
// i guess you need to define at runtime what type of interface you registered
// for type via interface traits.

} // namespace dyno
