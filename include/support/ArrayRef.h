#pragma once

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iterator>
template <typename T> class ArrayRef {
  using iterator_category = std::random_access_iterator_tag;
  using value_type = T;
  using pointer = T *;
  using reference = T &;
  using const_pointer = const T *;
  using const_reference = const T &;
  using difference_type = uintptr_t;

  using iterator = const_pointer;
  using const_iterator = const_pointer;

  const T *ptr;
  size_t sz;

public:
  const_iterator begin() const { return ptr; }
  const_iterator end() const { return ptr + sz; }

  size_t size() { return sz; }

  const_reference operator[](size_t i) {
    assert(i < sz);
    return ptr[i];
  }

  ArrayRef(const_iterator begin, const_iterator end)
      : ptr(begin), sz(end - begin) {}
  ArrayRef(const_iterator begin, size_t size) : ptr(begin), sz(size) {}

  const_reference back() { return ptr[sz - 1]; }
  const_reference front() { return ptr[0]; }

  static constexpr ArrayRef empty() { return ArrayRef{nullptr, size_t(0)}; }

  template <typename U> ArrayRef(const U &u) : ArrayRef(u.begin(), u.end()) {}
};

template <typename U> ArrayRef(const U &u) -> ArrayRef<typename U::value_type>;

template <typename T> class MutArrayRef {
  using iterator_category = std::random_access_iterator_tag;
  using value_type = T;
  using pointer = T *;
  using reference = T &;
  using const_pointer = const T *;
  using const_reference = const T &;
  using difference_type = uintptr_t;

  using iterator = pointer;
  using const_iterator = const_pointer;

  const T *ptr;
  size_t sz;

public:
  iterator begin() const { return ptr; }
  iterator end() const { return ptr + sz; }

  size_t size() { return sz; }

  reference operator[](size_t i) {
    assert(i < sz);
    return ptr[i];
  }

  MutArrayRef(const_iterator begin, const_iterator end)
      : ptr(begin), sz(end - begin) {}
  MutArrayRef(const_iterator begin, size_t size) : ptr(begin), sz(size) {}

  reference back() { return ptr[sz - 1]; }
  reference front() { return ptr[0]; }

  static constexpr MutArrayRef empty() { return MutArrayRef{nullptr, size_t(0)}; }

  template <typename U> MutArrayRef(U &u) : MutArrayRef(u.begin(), u.end()) {}
};

template <typename U> MutArrayRef(U &u) -> MutArrayRef<typename U::value_type>;
