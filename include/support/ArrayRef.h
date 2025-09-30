#pragma once

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <type_traits>
template <typename T> class ArrayRef {
public:
  using iterator_category = std::random_access_iterator_tag;
  using value_type = T;
  using pointer = T *;
  using reference = T &;
  using const_pointer = const T *;
  using const_reference = const T &;
  using difference_type = uintptr_t;

  using iterator = const_pointer;
  using const_iterator = const_pointer;

private:
  const T *ptr;
  size_t sz;

public:
  const_iterator begin() const { return ptr; }
  const_iterator end() const { return ptr + sz; }

  size_t size() const { return sz; }
  bool empty() const { return size() == 0; }

  const_reference operator[](size_t i) {
    assert(i < sz);
    return ptr[i];
  }

  ArrayRef(const_iterator begin, const_iterator end)
      : ptr(begin), sz(end - begin) {}
  ArrayRef(const_iterator begin, size_t size) : ptr(begin), sz(size) {}

  const_reference back() { return ptr[sz - 1]; }
  const_reference front() { return ptr[0]; }

  ArrayRef drop_front() const {
    assert(sz >= 1);
    return ArrayRef{ptr + 1, sz - 1};
  }
  ArrayRef drop_back() const {
    assert(sz >= 1);
    return ArrayRef{ptr, sz - 1};
  }

  const_pointer data() { return ptr; }

  static constexpr ArrayRef emptyRef() { return ArrayRef{nullptr, size_t(0)}; }

  template <typename U> ArrayRef(const U &u) : ArrayRef(u.begin(), u.end()) {}
};

template <typename U> ArrayRef(const U &u) -> ArrayRef<typename U::value_type>;

template <typename T>
concept IsArrayRef = (requires {
  typename T::value_type;
} && std::is_same_v<std::remove_cv_t<T>, ArrayRef<typename T::value_type>>);

template <typename T> class MutArrayRef {
public:
  using iterator_category = std::random_access_iterator_tag;
  using value_type = T;
  using pointer = T *;
  using reference = T &;
  using const_pointer = const T *;
  using const_reference = const T &;
  using difference_type = uintptr_t;

  using iterator = pointer;
  using const_iterator = const_pointer;

private:
  T *ptr;
  size_t sz;

public:
  iterator begin() const { return ptr; }
  iterator end() const { return ptr + sz; }

  size_t size() const { return sz; }
  bool empty() const { return size() == 0; }

  reference operator[](size_t i) const {
    assert(i < sz);
    return ptr[i];
  }

  MutArrayRef(iterator begin, iterator end) : ptr(begin), sz(end - begin) {}
  MutArrayRef(iterator begin, size_t size) : ptr(begin), sz(size) {}

  reference back() { return ptr[sz - 1]; }
  reference front() { return ptr[0]; }

  MutArrayRef drop_front() const {
    assert(sz >= 1);
    return MutArrayRef{ptr + 1, sz - 1};
  }
  MutArrayRef drop_back() const {
    assert(sz >= 1);
    return MutArrayRef{ptr, sz - 1};
  }

  template <typename U> bool all(U func) {
    return std::all_of(begin(), end(), func);
  }
  template <typename U> bool any(U func) {
    return std::any_of(begin(), end(), func);
  }
  template <typename U> void for_each(U func) {
    std::for_each(begin(), end(), func);
  }

  pointer data() { return ptr; }

  static constexpr MutArrayRef emptyRef() {
    return MutArrayRef{nullptr, size_t(0)};
  }

  template <typename U> MutArrayRef(U &u) : MutArrayRef(u.begin(), u.end()) {}
};

template <typename U> MutArrayRef(U &u) -> MutArrayRef<typename U::value_type>;

template <typename T, typename SizeT = uint32_t> struct ThinArrayRef {
public:
  using size_type = uint32_t;

private:
  size_type idx = 0;
  size_type len = 0;

public:
  ArrayRef<T> resolve(ArrayRef<T> storage) {
    return ArrayRef<T>{&storage[idx], len};
  }
  MutArrayRef<T> resolve(MutArrayRef<T> storage) {
    return MutArrayRef<T>{&storage[idx], len};
  }
  size_type size() const { return len; }

  constexpr static ThinArrayRef emptyRef() { return ThinArrayRef{0, 0}; }

  ThinArrayRef(size_type idx, size_type len) : idx(idx), len(len) {}
  ThinArrayRef() = default;
};
