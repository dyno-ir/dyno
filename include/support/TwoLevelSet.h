#pragma once
#include "support/DenseMapInfo.h"
#include "support/DenseMultimap.h"

template <typename T, typename KeyT = uint32_t,
          auto HashFunc = [](const T &t) { return std::hash<T>()(t); }>
class TwoLevelSet {
  DenseMultimap<Unhashed<KeyT>, T> map;

public:
  auto find(const T &t) {
    auto it = map.find(HashFunc(t));
    for (; it != map.end(); it = map.find_next(it)) {
      if (it.val() == t) [[likely]]
        return it;
    }
    return map.end();
  }
  auto insert(T &&t) { return map.insert(HashFunc(t), std::move(t)); }
  auto insert(const T &t) { return map.insert(HashFunc(t), T(t)); }

  auto begin() { return map.begin(); }
  auto end() { return map.end(); }

  auto begin() const { return map.begin(); }
  auto end() const { return map.end(); }

  auto erase(DenseMultimap<KeyT, T>::iterator it) { return map.erase(it); }
};

// Swiss-Table style map. This is for complex keys, use plain DenseMap for small
// POD keys.
template <typename K, typename T, typename KeyT = uint32_t> class TwoLevelMap {
  DenseMultimap<Unhashed<KeyT>, std::pair<const K, T>> map;
  static constexpr auto HashFunc = [](const K &t) { return std::hash<K>()(t); };

  auto find_raw(const K &k) {
    auto h = HashFunc(k);
    auto it = map.find(h);
    for (; it != map.end(); it = map.find_next(it)) {
      if (it.val().first == k) [[likely]]
        return std::make_pair(h, it);
    }
    return std::make_pair(h, map.end());
  }

public:
  class iterator
      : private DenseMultimap<Unhashed<KeyT>, std::pair<const K, T>>::iterator {
    using Base = DenseMultimap<Unhashed<KeyT>, std::pair<const K, T>>::iterator;
    friend class TwoLevelMap;

    using Base::Base;
    iterator(Base base) : Base(base) {}

  public:
    // prefer key()/val() for plain DenseMap compat
    std::pair<const K, T> &operator*() { return this->Base::val(); }
    std::pair<const K, T> *operator->() { return &(this->Base::val()); }

    const K &key() { return (*this)->first; }
    T &val() { return (*this)->second; }

    bool operator==(const iterator &o) const { return Base::operator==(o); }
    iterator &operator++() {
      this->Base::operator++();
      return *this;
    }
    iterator &operator++(int) { return iterator(this->Base::operator++(0)); }

    iterator() = default;
  };

  iterator find(const K &k) { return iterator(find_raw(k).second); }
  iterator insert(K &&k, T &&t) {
    return iterator(
        map.insert(HashFunc(k), std::make_pair(std::move(k), std::move(t))));
  }
  iterator insert(std::pair<K, T> &&pair) {
    return insert(std::move(pair.first), std::move(pair.second));
  }
  iterator insert(const K &k, const T &t) {
    return iterator(map.insert(HashFunc(k), std::make_pair(k, t)));
  }
  iterator insertOrAssign(const K &k, const T &t) {
    return findOrInsert(k, []() { return T{}; }).second;
  }
  auto findOrInsert(const K &k, auto &&func) {
    auto [h, it] = find_raw(k);
    if (it == map.end())
      return std::make_pair(
          false, iterator(map.insert(h, std::pair<K, T>(k, func()))));
    return std::make_pair(true, iterator(it));
  }
  T &operator[](const K &k) {
    return findOrInsert(k, []() { return T{}; }).second.val();
  }

  iterator begin() { return iterator(map.begin()); }
  iterator end() { return iterator(map.end()); }

  auto erase(iterator it) { return map.erase(it.base); }

  auto size() const { return map.size(); }
  void clear() { map.clear(); }
};
