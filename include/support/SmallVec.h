#pragma once

#include "support/ASAN.h"
#include "support/Ranges.h"
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <initializer_list>
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

  template <typename It> SmallVec(Range<It> range);

  SmallVec &operator=(SmallVec &&o) {
    // recover from moved-from state.
    if (this->arr == nullptr) [[unlikely]] {
      this->arr = std::launder(reinterpret_cast<T *>(storage.storage));
      this->cap = N;
      this->sz = 0;
      ASAN_UNPOISON_MEMORY_REGION(this->arr, N);
    }
    this->SmallVecImpl<T>::operator=(std::move(o));
    return *this;
  }

  SmallVec(const SmallVecImpl<T> &o)
      : SmallVecImpl<T>(std::launder(reinterpret_cast<T *>(storage.storage)), N,
                        o) {}
  SmallVec(const SmallVec<T, N> &o)
      : SmallVecImpl<T>(std::launder(reinterpret_cast<T *>(storage.storage)), N,
                        o) {}

  SmallVec &operator=(const SmallVec &o) {
    // recover from moved-from state.
    if (this->arr == nullptr) [[unlikely]] {
      this->arr = std::launder(reinterpret_cast<T *>(storage.storage));
      this->cap = N;
      this->sz = 0;
      ASAN_UNPOISON_MEMORY_REGION(this->arr, N);
    }
    this->SmallVecImpl<T>::operator=(o);
    return *this;
  }

  SmallVec(std::initializer_list<T> list);

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
  using value_type = T;
  using size_type = unsigned;
  using iterator = T *;
  using const_iterator = const T *;
  using param_type = T &;

protected:
  size_type sz;
  size_type cap;
  T *arr;

private:
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
    if (isSmall()) {
      ASAN_POISON_MEMORY_REGION(getInlineArrPtr(), cap);
      return;
    }
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

  ~SmallVecImpl() {
    destroy();
    ASAN_UNPOISON_MEMORY_REGION(getInlineArrPtr(), cap);
  }

  SmallVecImpl(T *arr, size_type cap, const SmallVecImpl &o)
      : SmallVecImpl(arr, cap) {
    (*this) = o;
  };
  SmallVecImpl &operator=(const SmallVecImpl &o) {
    this->resize_no_init(o.size());
    std::uninitialized_copy(o.begin(), o.end(), this->begin());
    return *this;
  };

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
      resize_no_init(o.size());
      std::uninitialized_move(o.begin(), o.end(), this->begin());
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

  void resize_no_init(size_type n) {
    if (n < sz) {
      std::destroy(begin() + n, end());
    } else if (n > sz) {
      reserve(n);
    }
    sz = n;
  }

public:
  T &operator[](size_type pos) {
    assert(pos < sz);
    return arr[pos];
  }
  const T &operator[](size_type pos) const {
    assert(pos < sz);
    return arr[pos];
  }

  template <typename... Args> T &emplace_back(Args &&...args) {
    grow(sz + 1);
    T *obj = std::construct_at<T>(end(), std::forward<Args>(args)...);
    ++sz;
    return *obj;
  }

  void push_back(const T &val) { emplace_back(val); }
  void push_back(T &&val) { emplace_back(std::move(val)); }

  void clear() {
    destroyElts();
    sz = 0;
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

  void downsize(size_type n) {
    assert(n <= sz);
    if (n < sz)
      std::destroy(begin() + n, end());
    sz = n;
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

  void reserve_safe(size_type n) { reserve(ceil_to_pow2(n)); }

  bool isSmall() { return arr == getInlineArrPtr(); }

  size_type size() const { return sz; }
  bool empty() const { return !sz; }

  T &back() {
    assert(sz > 0);
    return *(end() - 1);
  }

  const T &back() const {
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

  iterator insert(iterator it, T &&val) {
    assert(it <= end());
    if (it == end()) {
      emplace_back(val);
      return end() - 1;
    }
    size_t pos = it - begin();
    grow(sz + 1);
    std::construct_at(begin() + sz);
    std::move_backward(begin() + pos, end(), begin() + sz + 1);
    arr[pos] = std::move(val);
    ++sz;
    return begin() + pos;
  }
  iterator insert(iterator it, const T &val) { return insert(it, T{val}); }

  iterator erase(iterator it) {
    std::move(it + 1, end(), it);
    pop_back();
    return it;
  }

  template <typename It> void push_back_range(Range<It> range) {
    for (auto item : range) {
      emplace_back(item);
    }
  }
  template <typename It> void push_back_range(It begin, It end) {
    push_back_range(Range{begin, end});
  }

  T &front() {
    assert(sz > 0);
    return *arr;
  }

  const T &front() const {
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

template <typename T, unsigned N>
inline SmallVec<T, N>::SmallVec(std::initializer_list<T> list) : SmallVec() {
  this->reserve(list.size());
  for (auto &elem : list)
    this->emplace_back(elem);
}

template <typename T, unsigned N>
template <typename It>
inline SmallVec<T, N>::SmallVec(Range<It> range) : SmallVec() {
  this->template push_back_range<It>(range);
}

template <typename T, size_t NumInline> class CexprVec {
  using value_type = T;
  using size_type = uint32_t;
  using iterator = T *;
  using const_iterator = const T *;
  using param_type = T &;

  std::array<T, NumInline> arr = {};
  uint32_t sz = 0;

public:
  constexpr iterator begin() { return arr.begin(); }
  constexpr iterator end() { return arr.begin() + sz; }
  constexpr const_iterator begin() const { return arr.begin(); }
  constexpr const_iterator end() const { return arr.begin() + sz; }

  constexpr T &back() {
    assert(!empty());
    return *(end() - 1);
  }
  constexpr const T &back() const {
    assert(!empty());
    return *(end() - 1);
  }
  constexpr T &front() {
    assert(!empty());
    return *(begin());
  }
  constexpr const T &front() const {
    assert(!empty());
    return *(begin());
  }

  constexpr void pop_back() {
    assert(!empty());
    sz--;
  }

  constexpr T &operator[](size_type i) {
    assert(i < sz);
    return arr[i];
  }
  constexpr const T &operator[](size_type i) const {
    assert(i < sz);
    return arr[i];
  }

  constexpr void reserve(size_type) {}
  constexpr void resize(size_type newSize) {
    assert(newSize <= NumInline);
    sz = newSize;
  }
  constexpr size_t size() const { return sz; }
  constexpr bool empty() const { return size() == 0; }

  constexpr CexprVec() = default;
  constexpr CexprVec(size_type sz) : sz(sz) {}
};

template <typename T, size_t NumInline, typename SzT = uint32_t>
class StaticVec {
  using value_type = T;
  using size_type = SzT;
  using iterator = T *;
  using const_iterator = const T *;
  using param_type = T &;

  std::array<T, NumInline> arr;
  size_type sz = 0;

public:
  constexpr iterator begin() { return arr.begin(); }
  constexpr iterator end() { return arr.begin() + sz; }
  constexpr const_iterator begin() const { return arr.begin(); }
  constexpr const_iterator end() const { return arr.begin() + sz; }

  constexpr T &back() {
    assert(!empty());
    return *(end() - 1);
  }
  constexpr const T &back() const {
    assert(!empty());
    return *(end() - 1);
  }
  constexpr T &front() {
    assert(!empty());
    return *(begin());
  }
  constexpr const T &front() const {
    assert(!empty());
    return *(begin());
  }

  constexpr void pop_back() {
    assert(!empty());
    sz--;
  }

  constexpr T pop_back_val() {
    assert(!empty());
    T ret = std::move(arr[sz - 1]);
    sz--;
    return ret;
  }

  constexpr void push_back(T val) {
    assert(sz < NumInline);
    arr[sz] = val;
    ++sz;
  }

  bool erase_unordered(iterator it) {
    if (it != &back()) {
      *it = std::move(back());
      pop_back();
      return true;
    }
    pop_back();
    return false;
  }

  constexpr T &operator[](size_type i) {
    assert(i < sz);
    return arr[i];
  }
  constexpr const T &operator[](size_type i) const {
    assert(i < sz);
    return arr[i];
  }

  constexpr void reserve(size_type) {}
  constexpr void resize(size_type newSize) {
    assert(newSize <= NumInline);
    sz = newSize;
  }
  constexpr size_t size() const { return sz; }
  constexpr bool empty() const { return size() == 0; }

  constexpr void clear() { sz = 0; }

  constexpr StaticVec() = default;
  constexpr StaticVec(size_type sz) : sz(sz) {}
};
