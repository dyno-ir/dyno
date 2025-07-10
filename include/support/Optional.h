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
  constexpr Optional &operator=(nullopt_t) {
    val = Invalid;
    return *this;
  }

  constexpr friend bool operator==(const Optional &lhs, nullopt_t) {
    return !lhs;
  }
  constexpr friend bool operator==(const Optional &lhs, const Optional &rhs) {
    return lhs.val == rhs.val;
  }

  explicit operator bool() const { return val != Invalid; }
  explicit constexpr operator T() {
    assert(*this);
    return val;
  };

  constexpr T value() {
    assert(*this);
    return val;
  }
  constexpr T value_or(T &&alt) { return (*this) ? val : alt; }

  constexpr T &operator*() {
    assert(*this);
    return val;
  }
  constexpr const T &operator*() const {
    assert(*this);
    return val;
  }

  bool has() const { return bool(*this); }
};

}; // namespace dyno
