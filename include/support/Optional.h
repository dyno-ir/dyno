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
  constexpr Optional() : val(Invalid) {}
  constexpr Optional(const T &val) : val(val) { assert(val != Invalid); }
  constexpr Optional(T &&val) : val(std::move(val)) { assert(val != Invalid); }
  constexpr Optional &operator=(const T &rhs) {
    assert(rhs != Invalid);
    val = rhs;
    return *this;
  }
  constexpr Optional &operator=(T &&rhs) {
    assert(rhs != Invalid);
    val = std::move(rhs);
    return *this;
  };

  constexpr Optional(const Optional &) = default;
  constexpr Optional(Optional &&) = default;
  constexpr Optional &operator=(const Optional &) = default;
  constexpr Optional &operator=(Optional &&) = default;

  constexpr Optional(nullopt_t) : Optional() {}
  constexpr Optional &operator=(nullopt_t) { val = Invalid; return *this; }

  constexpr friend bool operator==(const Optional &rhs, nullopt_t) { return !rhs; }

  explicit operator bool() { return val != Invalid; }
  constexpr operator T() { return val; };

  constexpr T value() { return val; }
  constexpr T value_or(T &&alt) { return (*this) ? val : alt; }

  constexpr T &operator*() {
    assert(*this);
    return val;
  }
};

}; // namespace dyno
