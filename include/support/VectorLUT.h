#pragma once

#include "support/Optional.h"
#include <cassert>
#include <vector>
template <typename T> class VectorLUT {
public:
  using value_type = T;
  using reference = T &;
  using pointer = T *;
  using const_reference = const T &;
  using const_pointer = const T *;

private:
  size_t base = 0;
  std::vector<dyno::Optional<T>> data;

public:
  void ensure(size_t idx) {
    if (idx < base) {
      std::vector<T> newVec;
      newVec.reserve(data.size() + (base - idx));
      newVec.resize(base - idx);
      newVec.insert_range(newVec.end(), data);
      data = std::move(newVec);
      base = idx;
    } else if (idx - base >= data.size()) {
      data.resize(idx - base + 1);
    }
  }

  void insert(size_t k, T &&value) {
    ensure(k);
    auto &entry = data[k - base];
    assert(!entry);
    entry = std::move(value);
  }
  void insertOrAssign(size_t k, T &&value) {
    ensure(k);
    auto &entry = data[k - base];
    entry = std::move(value);
  }
  dyno::Optional<T> &find(size_t k) {
    ensure(k);
    return data[k - base];
  }
};
