#pragma once

#include <array>
#include <dyno/Obj.h>

namespace dyno {

template <typename T> class Interface;

template <typename T> struct InterfaceTraits {
  static const T *dispatch1(DynObjRef ref, const T **interfaces) {
    return interfaces[ref.getDialectID()];
  }
  static const T &dispatch2(DynObjRef ref, const T *interface) {
    return *interface;
  }
  static const unsigned ID = ~0;
};

template <typename T, size_t N> class StaticInterface {
  friend class Interface<T>;

  std::array<T *, N> entries;

public:
  constexpr StaticInterface(std::initializer_list<T *> entries)
      : entries(entries) {}
};

template <typename T> class Interface {
private:
  using Traits = InterfaceTraits<T>;
  using DispatchT = const T *;
  using DispatchRefT = const T &;
  const DispatchT *entries;

public:
  Interface(const DispatchT *entries) : entries(entries) {}

  template <size_t N>
  Interface(StaticInterface<T, N> interface) : entries(interface.entries) {}

  DispatchT operator[](DialectID dialect) { return entries[dialect]; }

  template <typename RefT> DispatchRefT operator[](RefT ref) {
    return Traits::dispatch2(
        ref, Traits::dispatch1(ref, const_cast<const T **>(entries)));
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

// load from 64k lookup table of dialect + type
// InterfaceTraits for type tells you what to do with the value

// e.g. resolving arbitrary DynObjRef to FatDynObjRef
// dialect + type lookup gives you ptr
// ptr must implement operator [] (you don't know the type obj store type but
// should still be generic...)
// i guess you need to define at runtime what type of interface you registered
// for type via interface traits.

} // namespace dyno
