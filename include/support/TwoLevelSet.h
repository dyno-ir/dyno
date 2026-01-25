#pragma once
#include "support/DenseMultimap.h"

template <typename T, typename KeyT = uint32_t,
          auto HashFunc = [](const T &t) { return std::hash<T>()(t); }>
class TwoLevelSet {
  DenseMultimap<KeyT, T> map;

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

template <typename K, typename T, typename KeyT = uint32_t,
          auto HashFunc = [](const T &t) { return std::hash<T>()(t); }>
class TwoLevelMap {
  DenseMultimap<KeyT, std::pair<K, T>> map;

public:
  auto find_raw(const K &k) {
    auto it = map.find(HashFunc(k));
    for (; it != map.end(); it = map.find_next(it)) {
      if (it.val() == k) [[likely]]
        return it->val();
    }
    return map.end();
  }
  auto insert(K &&k, T &&t) {
    return map.insert(HashFunc(t), std::make_pair(std::move(k), std::move(t)));
  }
  auto insert(const K &k, const T &t) {
    return map.insert(HashFunc(t), std::make_pair(k, t));
  }
  auto insertOrAssign(const K &k, const T &t) {
    return map.insertOrAssign(HashFunc(t), std::make_pair(k, t));
  }

  auto begin() { return map.begin(); }
  auto end() { return map.end(); }

  auto begin() const { return map.begin(); }
  auto end() const { return map.end(); }

  auto erase(DenseMultimap<KeyT, std::tuple<K, T>>::iterator it) {
    return map.erase(it);
  }
};
