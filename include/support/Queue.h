#pragma once

#include "support/SmallVec.h"
#include <algorithm>
#include <bit>
#include <cassert>
#include <type_traits>
#include <utility>

template <typename T, typename Container = SmallVec<T, 16>> class Queue {
  static_assert(std::is_same_v<typename Container::value_type, T>);

public:
  using value_type = T;
  using container_type = Container;
  using size_type = typename Container::size_type;

  static_assert(std::is_unsigned_v<size_type>);

private:
  Container arr;
  size_type head = 0;
  size_type tail = 0;

  size_type index(size_type counter) const {
    return counter & (capacity() - 1);
  }

  void grow(size_type minSz) {
    size_type cap = capacity();
    if (minSz <= cap)
      return;
    size_type newCap = std::max(size_type(1), size_type(2 * cap));
    assert(minSz <= newCap);
    reserve(newCap);
  }

public:
  Queue() { arr.resize(std::bit_floor(arr.capacity())); }
  Queue(const Queue &) = default;
  Queue &operator=(const Queue &) = default;

  Queue(Queue &&other)
      : arr(std::move(other.arr)), head(std::exchange(other.head, 0)),
        tail(std::exchange(other.tail, 0)) {
    other.arr.clear();
  }

  Queue &operator=(Queue &&other) {
    if (this == &other)
      return *this;
    arr = std::move(other.arr);
    head = std::exchange(other.head, 0);
    tail = std::exchange(other.tail, 0);
    other.arr.clear();
    return *this;
  }

  bool empty() const { return head == tail; }
  size_type size() const { return tail - head; }
  size_type capacity() const { return arr.size(); }

  T &front() {
    assert(!empty());
    return arr[index(head)];
  }
  const T &front() const {
    assert(!empty());
    return arr[index(head)];
  }
  T &back() {
    assert(!empty());
    return arr[index(tail - 1)];
  }
  const T &back() const {
    assert(!empty());
    return arr[index(tail - 1)];
  }

  void reserve(size_type newCap) {
    size_type cap = capacity();
    if (newCap <= cap)
      return;
    assert(std::has_single_bit(newCap));
    size_type sz = size();
    size_type start = cap ? index(head) : 0;
    arr.reserve(newCap);
    arr.resize(std::bit_floor(arr.capacity()));
    if (sz > cap - start) {
      size_type wrapped = sz - (cap - start);
      std::move(arr.begin(), arr.begin() + wrapped, arr.begin() + cap);
    }
    head = start;
    tail = head + sz;
  }

  template <typename... Args> T &emplace_back(Args &&...args) {
    if (size() == capacity())
      grow(size() + 1);
    size_type pos = index(tail);
    arr[pos] = T(std::forward<Args>(args)...);
    ++tail;
    return arr[pos];
  }

  void push_back(const T &value) { emplace_back(value); }
  void push_back(T &&value) { emplace_back(std::move(value)); }

  void pop_front() {
    assert(!empty());
    ++head;
  }

  T pop_front_val() {
    T value(std::move(front()));
    pop_front();
    return value;
  }

  void clear() {
    head = 0;
    tail = 0;
  }
};
