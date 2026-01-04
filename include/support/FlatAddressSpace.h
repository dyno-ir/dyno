#pragma once

#include "support/Bits.h"
#include "support/ErrorRecovery.h"
#include <concepts>
#include <cstddef>
#include <memory>
#include <sys/mman.h>
#include <type_traits>
template <typename T, size_t NumElements> struct FlatAddressSpace {
  void *memory;
  static_assert(checked_mul(sizeof(T), NumElements), "");

  FlatAddressSpace() {
    memory = mmap(NULL, sizeof(T) * NumElements, PROT_READ | PROT_WRITE,
                  MAP_ANONYMOUS | MAP_PRIVATE | MAP_NORESERVE, -1, 0);
    if (memory == (void *)-1)
      report_fatal_error("failed to mmap FlatAddressSpace");
  }

  ~FlatAddressSpace() { munmap(memory, sizeof(T) * NumElements); }

  T &operator[](size_t i) { return reinterpret_cast<T *>(memory)[i]; }

  size_t size() const { return NumElements; }
};

template <typename T, size_t NumElements> class FlatAddressSpaceVec {
public:
  using value_type = T;
  using size_type = size_t;
  using pointer = T *;
  using iterator = T *;
  using const_iterator = const T *;
  using param_type = T &;
  using reference = T &;
  using const_reference = const T &;

private:
  FlatAddressSpace<T, NumElements> space;
  size_t sz;
  void destroyElts() { std::destroy_n(begin(), sz); }

public:
  FlatAddressSpaceVec() : sz(0) {}

  iterator begin() { return reinterpret_cast<iterator>(space.memory); }
  iterator end() { return begin() + sz; }
  pointer data() { return begin(); }
  const_iterator begin() const {
    return reinterpret_cast<const_iterator>(space.memory);
  }
  const_iterator end() const { return begin() + sz; }

  reference operator[](size_t i) {
    assert(i < sz && "oob");
    return begin()[i];
  }
  const_reference operator[](size_t i) const {
    assert(i < sz && "oob");
    return begin()[i];
  }
  ~FlatAddressSpaceVec() { destroyElts(); }

  template <typename... Args> reference emplace_back(Args &&...args) {
    assert(sz != NumElements && "out of preallocated space");
    T *obj = std::construct_at<T>(std::forward<Args>(args)...);
    ++sz;
    return *obj;
  }
  void push_back(const T &val) { emplace_back(val); }
  void push_back(T &&val) { emplace_back(std::move(val)); }

  void clear() {
    destroyElts();
    sz = 0;
  }

  size_t size() const { return sz; }

  void resize(size_t newSz) {
    assert(newSz <= space.size());
    if (newSz == sz)
      return;
    if (newSz > sz) {
      std::uninitialized_value_construct(begin() + sz, begin() + newSz);
    } else {
      std::destroy(begin() + newSz, begin() + sz);
    }
    sz = newSz;
  }
  void resize(size_t newSz, const T &value) {
    assert(newSz <= space.size());
    if (newSz == sz)
      return;
    if (newSz > sz) {
      std::uninitialized_fill(begin() + sz, begin() + newSz, value);
    } else {
      std::destroy(begin() + newSz, begin() + sz);
    }
    sz = newSz;
  }

  void reserve(size_t) {}
};
