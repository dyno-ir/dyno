#pragma once
#include "DenseMapInfo.h"
#include <cassert>

namespace dyno {

struct nullopt_t {};
inline constexpr nullopt_t nullopt{};

// Simple optional without extra storage, uses DenseMapInfo empty for invalid
// state.
template <typename T, auto InvalidF = DenseMapInfo<T>::getEmptyKey>
class Optional {
  T val;

public:
  constexpr Optional() : val(InvalidF()) {}
  constexpr Optional(const T &val) : val(val) { assert(val != InvalidF()); }
  constexpr Optional(T &&val) : val(std::move(val)) {
    assert(val != InvalidF());
  }
  constexpr Optional &operator=(const T &rhs) {
    assert(rhs != InvalidF());
    val = rhs;
    return *this;
  }
  constexpr Optional &operator=(T &&rhs) {
    assert(rhs != InvalidF());
    val = std::move(rhs);
    return *this;
  };

  constexpr Optional(const Optional &) = default;
  constexpr Optional(Optional &&) = default;
  constexpr Optional &operator=(const Optional &) = default;
  constexpr Optional &operator=(Optional &&) = default;

  constexpr Optional(nullopt_t) : Optional() {}
  constexpr Optional &operator=(nullopt_t) {
    val = InvalidF();
    return *this;
  }

  constexpr friend bool operator==(const Optional &lhs, nullopt_t) {
    return !lhs;
  }
  constexpr friend bool operator==(const Optional &lhs, const Optional &rhs) {
    return lhs.val == rhs.val;
  }

  explicit operator bool() const { return val != InvalidF(); }

  template <typename U = T>
    requires(!std::is_same_v<U, bool>)
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
  constexpr T *operator->() {
    assert(*this);
    return &val;
  }
  constexpr const T *operator->() const {
    assert(*this);
    return &val;
  }

  bool has() const { return bool(*this); }
};

template <> class Optional<bool> : public Optional<uint8_t> {
public:
  using Optional<uint8_t>::Optional;
};

}; // namespace dyno

template <typename T> struct DenseMapInfo<dyno::Optional<T>> {
  static T getEmptyKey() {
    return DenseMapInfo<T>::getEmptyKey() +
           (std::numeric_limits<T>::is_signed ? 1 : (-1));
  }
  static T getTombstoneKey() {
    return DenseMapInfo<T>::getTombstoneKey() +
           (std::numeric_limits<T>::is_signed ? 1 : (-1));
  }
  static unsigned getHashValue(const T &k) { return std::hash<T>()(k); }
  static bool isEqual(const T &lhs, const T &rhs) { return lhs == rhs; }
};
