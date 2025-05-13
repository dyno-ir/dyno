#pragma once

#include "support/Ranges.h"
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

  SmallVec(SmallVecImpl<T> &&o)
      : SmallVecImpl<T>(std::launder(reinterpret_cast<T *>(storage.storage)), N,
                        std::move(o)) {}

  SmallVec(SmallVec<T, N> &&o)
      : SmallVecImpl<T>(std::launder(reinterpret_cast<T *>(storage.storage)), N,
                        std::move(o)) {}

  SmallVec &operator=(SmallVec &&o) {
    this->SmallVecImpl<T>::operator=(std::move(o));
    return *this;
  }

  SmallVec(size_t size)
      : SmallVecImpl<T>(std::launder(reinterpret_cast<T *>(storage.storage)),
                        N) {
    this->resize(size);
  }

  SmallVec(size_t size, const T &templ)
      : SmallVecImpl<T>(std::launder(reinterpret_cast<T *>(storage.storage)),
                        N) {
    this->resize(size, templ);
  }

  void try_to_inline() {
    return this->SmallVecImpl<T>::try_to_inline(
        reinterpret_cast<T *>(storage.storage), N);
  }
};

template <typename T> class SmallVecImpl {
public:
  using size_type = unsigned;
  using iterator = T *;
  using const_iterator = const T *;
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

  struct SmallVectorAlignmentAndSize {
    alignas(SmallVecImpl<T>) char base[sizeof(SmallVecImpl<T>)];
    alignas(T) char inlineElemns[sizeof(T)];
  };
  void *getInlineArrPtr() const {
    return const_cast<void *>(reinterpret_cast<const void *>(
        reinterpret_cast<const char *>(this) +
        offsetof(SmallVectorAlignmentAndSize, inlineElemns)));
  }
  bool isInline() { return arr == getInlineArrPtr(); }

protected:
  SmallVecImpl(T *arr, size_type cap) : sz(0), cap(cap), arr(arr) {}

  ~SmallVecImpl() { destroy(); }

  SmallVecImpl(const SmallVecImpl &) = delete;
  SmallVecImpl &operator=(const SmallVecImpl &) = delete;

  SmallVecImpl(T *arr, size_type cap, SmallVecImpl &&o)
      : SmallVecImpl<T>(arr, cap) {
    (*this).operator=(std::move(o));
  }
  SmallVecImpl &operator=(SmallVecImpl &&o) {
    if (&o == this)
      return *this;

    if (!o.isInline()) {
      this->destroy();
      this->arr = o.arr;
      this->sz = o.sz;
      this->cap = o.cap;

      o.arr = nullptr;
    } else if (this->size() >= o.size()) {
      iterator it = this->begin();

      if (o.size() != 0)
        it = std::move(o.begin(), o.end(), it);

      std::destroy(it, this->end());
      sz = o.sz;
    } else {
      destroyElts();
      resize(o.size());
      std::move(o.begin(), o.end(), this->begin());
      sz = o.sz;
    }

    return *this;
  }

  void try_to_inline(T *inlinePtr, size_t inlineSize) {
    if (this->sz <= inlineSize && arr != inlinePtr) {
      std::move(begin(), end(), inlinePtr);
      destroyElts();
      ::operator delete[](arr);

      cap = inlineSize;
      arr = inlinePtr;
    }
  }

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

  void resize_no_init(size_type n) {
    if (n < sz) {
      std::destroy(begin() + n, end());
    } else if (n > sz) {
      reserve(n);
    }
    sz = n;
  }

  void resize(size_type n, const T &templ) {
    size_t oldSz = sz;
    resize_no_init(n);
    if (n > oldSz) {
      std::uninitialized_fill(begin() + oldSz, end(), templ);
    }
  }

  void resize(size_type n) {
    size_t oldSz = sz;
    resize_no_init(n);
    if (n > oldSz) {
      std::uninitialized_value_construct(begin() + oldSz, end());
    }
  }

  void reserve(size_type n) {
    if (n <= cap)
      return;
    assert(cap > 0);

    T *newArr = reinterpret_cast<T *>(::operator new[](n * sizeof(T)));
    std::uninitialized_move(begin(), end(), newArr);
    destroy();
    arr = newArr;
    cap = n;
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

  template <std::input_iterator It> void push_back_range(Range<It> range) {
    for (auto &item : range) {
      emplace_back(item);
    }
  }
  template <std::input_iterator It> void push_back_range(It begin, It end) {
    push_back_range(Range{begin, end});
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

  T *data() { return begin(); }

  iterator begin() { return arr; }
  iterator end() { return arr + sz; }

  const_iterator begin() const { return arr; }
  const_iterator end() const { return arr + sz; }
};
