#pragma once

#include "dyno/CFG.h"
#include <iterator>

namespace dyno {

class StableBlockIterator {
private:
  CFG *cfg;
  InstrRef cur;

public:
  StableBlockIterator(CFG &cfg, InstrRef cur) : cfg(&cfg), cur(cur) {}
  StableBlockIterator(CFG &cfg) : cfg(&cfg), cur(nullref) {}
  StableBlockIterator(BlockRef_iterator_base it)
      : cfg(&it.blockRef().getCFG()),
        cur(it == it.blockRef().end() ? nullref : *it) {}

  using iterator_category = std::bidirectional_iterator_tag;
  using value_type = InstrRef;
  using pointer = value_type *;
  using reference = value_type &;
  using difference_type = ptrdiff_t;

  StableBlockIterator &operator++() {
    assert(cfg->contains(cur));
    auto it = std::next(BlockRef_iterator<true>{(*cfg)[cur]});
    if (it == it.blockRef().end())
      cur = nullref;
    else
      cur = *it;
    return *this;
  }
  StableBlockIterator &operator--() {
    assert(cfg->contains(cur));
    cur = *std::prev(BlockRef_iterator<true>{(*cfg)[cur]});
    return *this;
  }
  StableBlockIterator operator++(int) {
    auto temp(*this);
    ++(*this);
    return temp;
  }
  StableBlockIterator operator--(int) {
    auto temp(*this);
    --(*this);
    return temp;
  }

  InstrRef operator*() { return cur; }
  InstrRef *operator->() { return &cur; }

  friend bool operator==(StableBlockIterator lhs, StableBlockIterator rhs) {
    return lhs.cur == rhs.cur && lhs.cfg == rhs.cfg;
  }
  bool isEnd() { return cur == nullref; }

  CFG &getCFG() { return *cfg; }
  BlockRef blockRef() { return (*cfg)[cur].blockRef(); }
  BlockRef_iterator_base getBlockIter() { return (*cfg)[cur]; }
};

template <typename BlockIt> class HierBlockRangeIterBase {
  BlockIt it;
  uint32_t depth = 0;

public:
  HierBlockRangeIterBase(BlockIt it) : it(it) {}

  HierBlockRangeIterBase &operator++() {
    auto isNonEmptyBlockRef = [](Operand &opRef) {
      return opRef.template is<BlockRef>() && !opRef.as<BlockRef>().empty();
    };
    auto blockIt =
        std::find_if(*it->def_begin(), *it->def_end(), isNonEmptyBlockRef);
    if (blockIt != *it->def_end()) {
      depth++;
      it = BlockIt{blockIt->template as<BlockRef>().begin()};
      return *this;
    }

    auto prev = it++;

    while (it.isEnd() && depth != 0) {
      auto def = *prev.blockRef().def();
      auto instr = def.instr();

      it = BlockIt{prev.blockRef().getCFG()[instr]};
      auto blockIt =
          std::find_if(std::next(def), *it->def_end(), isNonEmptyBlockRef);
      if (blockIt != *it->def_end()) {
        it = BlockIt{blockIt->template as<BlockRef>().begin()};
        break;
      }

      depth--;
      prev = it++;
    }

    return *this;
  }

  HierBlockRangeIterBase operator++(int) {
    auto temp{*this};
    ++(*this);
    return temp;
  }

  InstrRef operator*() { return *it; }
  InstrRef *operator->() { return it.decltype(it)::operator->(); }

  friend bool operator==(HierBlockRangeIterBase lhs,
                         HierBlockRangeIterBase rhs) {
    assert(!(lhs.it.isEnd() && rhs.it.isEnd()) || (lhs.depth == rhs.depth));
    return lhs.it == rhs.it && lhs.depth == rhs.depth;
  }

  bool isEnd() { return depth == 0 && it.isEnd(); }
};

using HierBlockRangeIter = HierBlockRangeIterBase<BlockRef_iterator<true>>;
using StableHierBlockRangeIter = HierBlockRangeIterBase<StableBlockIterator>;

class HierBlockRange {
public:
  BlockRef block;
  HierBlockRange(BlockRef block) : block(block) {}
  HierBlockRangeIter begin() { return HierBlockRangeIter{block.begin()}; }
  HierBlockRangeIter end() { return HierBlockRangeIter{block.end()}; }
};

class StableHierBlockRange {
public:
  BlockRef block;
  StableHierBlockRange(BlockRef block) : block(block) {}
  StableHierBlockRangeIter begin() {
    return StableHierBlockRangeIter{block.begin()};
  }
  StableHierBlockRangeIter end() {
    return StableHierBlockRangeIter{block.end()};
  }
};

} // namespace dyno
