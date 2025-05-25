#pragma once

#include "dyno/CFG.h"
#include <iostream>

namespace dyno {

class HierBlockRange {
  class iterator {
    HierBlockRange &parent;
    BlockRef_iterator<true> it;

  public:
    iterator &operator++() {
      auto isBlockRef = [](OperandRef opRef) {
        return opRef->template is<BlockRef>();
      };
      auto subIt = *std::find_if(it->def_begin(), it->def_end(), isBlockRef);

      if (subIt != it->def_end()) {
        parent.stack.emplace_back(
            std::make_pair(it, (subIt - it->def_begin()) + 1));
        it = subIt->as<BlockRef>().begin();
        return *this;
      }

      ++it;

      while (it == it.blockRef().end()) {
        if (parent.stack.size() <= 1)
          return *this;
        auto [parentIt, subIdx] = parent.stack.back();
        if (subIdx != 0) {
          auto subIt = *std::find_if(parentIt->def_begin() + subIdx,
                                         parentIt->def_end(), isBlockRef);
          if (subIt != parentIt->def_end()) {
            parent.stack.back().second = subIt - parentIt->def_begin() + 1;
            it = subIt->as<BlockRef>().begin();
            return *this;
          }
        }
        it = parentIt;
        parent.stack.pop_back();
        ++it;
      }

      return *this;
    }

    iterator operator++(int) {
      iterator tmp{*this};
      ++(*this);
      return tmp;
    }

    friend bool operator==(const iterator &lhs, const iterator &rhs) {
      return &lhs.parent == &rhs.parent && lhs.it == rhs.it;
    }

    auto &operator*() { return *it; }
    auto *operator->() { return it.BlockRef_iterator_base::operator->(); }

  public:
    explicit iterator(HierBlockRange &parent, BlockRef_iterator<true> it)
        : parent(parent), it(it) {}
  };

  SmallVec<std::pair<BlockRef_iterator<true>, uint16_t>, 4> stack;

public:
  HierBlockRange(BlockRef block) : stack{std::make_pair(block.begin(), 0)} {}

  iterator begin() {
    return iterator{*this, stack.front().first.blockRef().begin()};
  }
  iterator end() {
    return iterator{*this, stack.front().first.blockRef().end()};
  }
};

} // namespace dyno
