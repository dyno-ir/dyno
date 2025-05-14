#pragma once
#include "DenseMapInfo.h"

namespace dyno {

struct nullopt_t {};
inline constexpr nullopt_t nullopt{};

// Simple optional without extra storage, uses DenseMapInfo empty for invalid
// state.
template <typename T, auto Invalid = DenseMapInfo<T>::getEmptyKey()>
class Optional {
  T val;

public:
  Optional() : val(Invalid) {}
  Optional(const T &val) : val(val) { assert(val != Invalid); }
  Optional(T &&val) : val(std::move(val)) { assert(val != Invalid); }
  Optional &operator=(const T &rhs) {
    assert(rhs != Invalid);
    val = rhs;
    return *this;
  }
  Optional &operator=(T &&rhs) {
    assert(rhs != Invalid);
    val = std::move(rhs);
    return *this;
  };

  Optional(const Optional &) = default;
  Optional(Optional &&) = default;
  Optional &operator=(const Optional &) = default;
  Optional &operator=(Optional &&) = default;

  Optional(nullopt_t) : Optional() {}
  Optional &operator=(nullopt_t) { val = Invalid; }

  friend bool operator==(const Optional &rhs, nullopt_t) { return !rhs; }

  explicit operator bool() { return val != Invalid; }
  operator T() { return val; };

  T value() { return val; }
  T value_or(T &&alt) { return (*this) ? val : alt; }

  T &operator*() {
    assert(*this);
    return val;
  }
};

}; // namespace dyno
