#pragma once

#include <cassert>
#include <utility>


template <std::unsigned_integral T> class PackedSet {
public:
  using iterator = Vec<T>::const_iterator;

  PackedSet() : sz(0) {}
  PackedSet(size_t universeSz) : sz(0), map(universeSz), stack(universeSz) {}

  size_t size() const { return sz; }

  bool empty() const { return size() == 0; }

  void clear() { sz = 0; }

  bool contains(T v) const {
    assert(v < map.size());
    auto idx = map[v];
    return idx < sz && stack[idx] == v;
  }

  bool erase(T v) {
    assert(v < map.size());
    if (!contains(v))
      return false;
    --sz;
    auto idx = map[v];
    auto moveVal = stack[sz];
    stack[idx] = moveVal;
    map[moveVal] = idx;
    return true;
  }

  bool insert(T v) {
    assert(v < map.size());
    if (contains(v))
      return false;
    map[v] = sz;
    stack[sz] = v;
    ++sz;
    return true;
  }

  iterator begin() const { return stack.begin(); }
  iterator end() const { return stack.begin() + sz; }

private:
  size_t sz;
  Vec<T> map;
  Vec<T> stack;
};
