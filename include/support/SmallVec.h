#pragma once

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <memory>
#include <support/Bits.h>
#include <support/InlineStorage.h>
#include <utility>

template <typename T, unsigned N> class SmallVec;
template <typename T> class SmallVecImpl;

template <typename T, unsigned N> class SmallVec : public SmallVecImpl<T> {
  friend class SmallVecImpl<T>;
  InlineStorageArr<T, N> storage;

public:
  SmallVec()
      : SmallVecImpl<T>(std::launder(reinterpret_cast<T *>(storage.storage)),
                        N) {}
};

template <typename T> class SmallVecImpl {
public:
  using size_type = unsigned;
  using iterator = T *;
  using param_type = T &;

private:
  size_type sz;
  size_type cap;
  T *arr;

  void grow(size_type minSz) {
    if (minSz <= cap)
      return;
    assert(cap > 0);
    assert(minSz <= cap * 2);

    size_type newCap = 2 * cap;
    T *newArr = reinterpret_cast<T *>(::operator new[](newCap * sizeof(T)));
    std::uninitialized_move(begin(), end(), newArr);
    destroy();
    arr = newArr;
    cap = newCap;
  }

  T *getInlineArrPtr() { return *static_cast<SmallVec<T, 1> &>(*this).storage; }

  void destroy() {
    if (!arr)
      return; // Moved-from state
    destroyElts();
    deallocate();
  }

  void destroyElts() { std::destroy_n(begin(), sz); }

  void deallocate() {
    if (isSmall())
      return;
    ::operator delete[](arr);
  }

protected:
  SmallVecImpl(T *arr, size_type cap) : sz(0), cap(cap), arr(arr) {}

  ~SmallVecImpl() { destroy(); }

  SmallVecImpl(const SmallVecImpl &) = delete;
  SmallVecImpl(SmallVecImpl &&o) = delete;
  SmallVecImpl &operator=(const SmallVecImpl &) = delete;
  SmallVecImpl &operator=(SmallVecImpl &&) = delete;

public:
  T &operator[](size_type pos) {
    assert(pos < sz);
    return arr[pos];
  }

  template <typename... Args> T &emplace_back(Args &&...args) {
    grow(sz + 1);
    T *obj = std::construct_at<T>(end(), std::forward<Args>(args)...);
    ++sz;
    return *obj;
  }

  void clear() {
    destroyElts();
    sz = 0;
  }

  bool isSmall() { return arr == getInlineArrPtr(); }

  size_type size() const { return sz; }
  bool empty() const { return !sz; }

  T &back() {
    assert(sz > 0);
    return *(end() - 1);
  }

  bool erase_unordered(iterator it) {
    // Don't self-move assign. I think C++17 allows this tho
    // TODO: Gate this if trivially move-assignable?
    if (it != &back()) {
      *it = std::move(back());
      pop_back();
      return true;
    }
    pop_back();
    return false;
  }

  void insert_unordered(iterator it, const T &val) {
    assert(it <= end());
    if (it == end()) {
      emplace_back(val);
      return;
    }
    size_t pos = it - begin();
    grow(sz + 1);
    T *obj = std::construct_at<T>(end(), std::move(arr[pos]));
    arr[pos] = val;
    ++sz;
  }

  T &front() {
    assert(sz > 0);
    return *arr;
  }

  void pop_back() {
    std::destroy_at(&back());
    --sz;
  }

  T pop_back_val() {
    T retVal = std::move(back());
    pop_back();
    return retVal;
  }

  iterator begin() { return arr; }
  iterator end() { return arr + sz; }
};
