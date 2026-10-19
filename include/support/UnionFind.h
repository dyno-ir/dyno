#pragma once

#include "support/DenseMapInfo.h"
#include <utility>

template <typename Key, template <typename> typename Map> class UnionFind {
  using KeyInfo = DenseMapInfo<Key>;

public:
  struct Node {
    Key parent = KeyInfo::getEmptyKey();
    unsigned rank = 0;
  };

  struct Merge {
    bool merged;
    Key parent;
    Key child;
  };

private:
  Map<Node> nodes;

  Node &get(Key key) {
    if constexpr (requires { nodes.get_ensure(key); })
      return nodes.get_ensure(key);
    else
      return nodes[key];
  }

public:
  Key find(Key key) {
    Key root = key;
    for (Key parent;; root = parent) {
      parent = get(root).parent;
      if (KeyInfo::isEqual(parent, KeyInfo::getEmptyKey()))
        break;
    }
    for (Key curr = key; !KeyInfo::isEqual(curr, root);)
      curr = std::exchange(get(curr).parent, root);
    return root;
  }

  Merge unite(Key a, Key b) {
    Key parent = find(a);
    Key child = find(b);
    if (KeyInfo::isEqual(parent, child))
      return {false, parent, child};
    unsigned parentRank = get(parent).rank;
    unsigned childRank = get(child).rank;
    if (parentRank < childRank)
      std::swap(parent, child);
    get(child).parent = parent;
    if (parentRank == childRank)
      ++get(parent).rank;
    return {true, parent, child};
  }
};
