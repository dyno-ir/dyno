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
  DispatchT *entries;

public:
  Interface(DispatchT *entries) : entries(entries) {}

  template <size_t N>
  Interface(StaticInterface<T, N> interface) : entries(interface.entries) {}

  DispatchT operator[](DialectID dialect) { return entries[dialect]; }

  template <typename RefT> DispatchRefT operator[](RefT ref) {
    return Traits::dispatch2(ref, Traits::dispatch1(ref, entries));
  }

  /*template <typename... Args> auto operator()(ObjRef ref, Args &&...args) {*/
  /*  return (*this->operator[](ref))(ref, std::forward<Args>(args)...);*/
  /*}*/
};

// class Interfaces {
// private:
//   std::vector<void *> interfaces;
//   unsigned numDialects;

// public:
//   Interfaces(unsigned numDialects, unsigned numInterfaces)
//       : interfaces(numInterfaces * numDialects), numDialects(numDialects) {}

//   template <typename T>
//   void registerInterface(unsigned dialectID,
//                          InterfaceTraits<T>::DispatchT entry) {
//     using Traits = InterfaceTraits<T>;
//     interfaces[numDialects * Traits::ID + dialectID] = entry;
//   }

//   template <typename T> InterfaceDispatch<T> get() {
//     using Traits = InterfaceTraits<T>;
//     return reinterpret_cast<Traits::DispatchT *>(
//         &interfaces[numDialects * Traits::ID]);
//   }
// };

// load from 64k lookup table of dialect + type
// InterfaceTraits for type tells you what to do with the value

// e.g. resolving arbitrary DynObjRef to FatDynObjRef
// dialect + type lookup gives you ptr
// ptr must implement operator [] (you don't know the type obj store type but
// should still be generic...)
// i guess you need to define at runtime what type of interface you registered
// for type via interface traits.

} // namespace dyno
