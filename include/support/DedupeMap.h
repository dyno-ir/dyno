#pragma once

#include "support/DenseMap.h"
#include "support/DenseMultimap.h"
#include <cstdint>

template <typename T, typename Container = std::vector<T>,
          auto HashFunc = [](const T &t) { return std::hash<T>()(t); }>
class DedupeMap {
  DenseMultimap<uint32_t, uint32_t> map;

public:
  Container container;

  uint32_t getCanonicalIndex(const T &t) { return canonicalize(t).first; }

  std::pair<uint32_t, bool> canonicalize(const T &t) {
    auto hash = HashFunc(t);
    // we can't use empty/tombstone, so just remap to zero. This should
    // basically never fire so should be fine, otherwise we can go to 31 bit
    // hash.
    if (hash == DenseMapInfo<uint32_t>::getEmptyKey() ||
        hash == DenseMapInfo<uint32_t>::getTombstoneKey()) [[unlikely]] {
      hash = 0;
    }

    auto it = map.find(hash);
    for (; it != map.end(); it = map.find_next(it)) {
      if (container[it.val()] == t) [[likely]]
        return {it.val(), false};
    }
    auto rv = container.size();
    container.emplace_back(t);
    map.insert(hash, rv);
    return {rv, true};
  }

  const T &getCanonical(const T &t) { return container[getCanonicalIndex(t)]; }
  uint32_t size() { return map.size(); }
};
