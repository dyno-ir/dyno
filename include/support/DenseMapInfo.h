#pragma once
#include <functional>

template <typename T> struct DenseMapInfo {
  static constexpr T getEmptyKey() { return ~T(0); }
  static constexpr T getTombstoneKey() { getEmptyKey() - 1; }
  static unsigned getHashValue(const T &k) { return std::hash<T>()(k); }
  static unsigned isEqual(const T &lhs, const T &rhs) { return lhs == rhs; }
};
