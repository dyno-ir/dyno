#pragma once
#include <cstdint>
#include <functional>

template <typename T>
concept Pointer = std::is_pointer_v<T>;

template <typename T> struct DenseMapInfo {
  // static constexpr T getEmptyKey() { return ~T(0); }
  // static constexpr T getTombstoneKey() { getEmptyKey() - 1; }
  // static unsigned getHashValue(const T &k) { return std::hash<T>()(k); }
  // static unsigned isEqual(const T &lhs, const T &rhs) { return lhs == rhs; }
};

template <Pointer T> struct DenseMapInfo<T> {
  static T getEmptyKey() { return reinterpret_cast<T>(UINTPTR_MAX); }
  static T getTombstoneKey() { return reinterpret_cast<T>(UINTPTR_MAX - 1); }
  static unsigned getHashValue(const T &k) { return std::hash<T>()(k); }
  static unsigned isEqual(const T &lhs, const T &rhs) { return lhs == rhs; }
};

template <std::integral T> struct DenseMapInfo<T> {
  static constexpr T getEmptyKey() { return ~T(0); }
  static constexpr T getTombstoneKey() { getEmptyKey() - 1; }
  static unsigned getHashValue(const T &k) { return std::hash<T>()(k); }
  static unsigned isEqual(const T &lhs, const T &rhs) { return lhs == rhs; }
};
