#pragma once

#include <utility>
template <typename Derived, typename StorageT>
class CustomInstrRef : public Derived {
public:
  using Derived::Derived;

  CustomInstrRef(const Derived &derived) : Derived(derived) {};

  void emplace(StorageT &&t) {
    InlineStorageRef<StorageT>(this->customStorage()).emplace(std::move(t));
  }
  StorageT &get() { return *InlineStorageRef<StorageT>(this->customStorage()); }

  template <std::invocable T> StorageT &getOrEmplace(T &&func) {
    if (auto &v = get())
      return v;
    emplace(func());
    return get();
  }
};
