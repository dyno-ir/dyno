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

  Container::size_type getCanonicalIndex(const T &t) {
    auto hash = HashFunc(t);
    // todo: remap the two invalid hashes to something else.
    auto it = map.find(hash);
    for (; it; it = map.find_next(it)) {
      if (container[it.val()] == t) [[likely]]
        return it.val();
    }
    auto rv = container.size();
    container.emplace_back(t);
    map.insert(hash, rv);
    return rv;
  }
  const T &getCanonical(const T &t) { return container[getCanonicalIndex(t)]; }
};
