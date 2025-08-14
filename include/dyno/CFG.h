#pragma once

#include "dyno/Instr.h"
#include "dyno/NewDeleteObjStore.h"
#include "dyno/Obj.h"
#include "dyno/ObjMap.h"
#include "support/RTTI.h"
#include <cassert>

namespace dyno {

class BlockRef;
class BlockRef_iterator_base;
class CFG;

class Block {
  friend class BlockRef;
  friend class BlockRef_iterator_base;

  struct Node {
    uint32_t next;
    uint32_t prev;
    InstrRef ref;

    Node(InstrRef ref, ObjID next, ObjID prev)
        : next(next), prev(prev), ref(ref) {}
    Node() = default;
  };

  using iterator = Node *;

public:
  InstrDefUse defUse;

private:
  SmallVec<Node, 16> instrs;
  CFG *cfg;
  bool sorted;
  ObjRef<Block> ref;

public:
  Block(ObjRef<Block> ref, CFG &cfg) : cfg(&cfg), sorted(true), ref(ref) {
    instrs.emplace_back(InstrRef{nullref}, IDImpl<uint32_t>{0},
                        IDImpl<uint32_t>{0});
  }
};

template <> struct ObjTraits<Block> {
  // static constexpr DialectID dialect{DIALECT_CORE};
  static constexpr DialectType ty{CORE_BLOCK};
  using FatRefT = BlockRef;
};

using BlockStore = NewDeleteObjStore<Block>;

class CFG {
  friend class BlockRef;
  friend class BlockRef_iterator_base;

  struct Node {
    ObjRef<Block> block{ObjID::INVALID};
    uint32_t blockPos;
  };

  ObjMapVec<Instr, Node> map;

public:
  BlockStore blocks;

  bool contains(ObjRef<Instr> ref) {
    if (!map.inRange(ref))
      return false;
    return bool(map[ref].block);
  }

  BlockRef_iterator_base operator[](ObjRef<Instr> ref);
};

class BlockRef_iterator_base {
protected:
  Block *block;
  uint32_t pos;

public:
  using value_type = InstrRef;
  using difference_type = std::ptrdiff_t;
  using pointer = value_type *;
  using reference = value_type &;

  BlockRef_iterator_base() = default;
  BlockRef_iterator_base(BlockRef block, uint32_t pos);

  value_type &operator*() const { return entry().ref; }
  pointer operator->() const { return &entry().ref; }
  InstrRef instr() const { return entry().ref; }

  void erase() {
    block->sorted = false;
    entryOrderedPrev().next = entry().next;
    entryOrderedNext().prev = entry().prev;
    if (block->instrs.erase_unordered(block->instrs.begin() + pos)) {
      entryOrderedPrev().next = pos;
      entryOrderedNext().prev = pos;
      cfg().map[entry().ref].blockPos = pos;
    }
  }

  void replace(InstrRef ref) {
    auto &lval = cfg().map.get_ensure(ref);
    lval = cfg().map[entry().ref];
    cfg().map[entry().ref] = CFG::Node{nullref, 0};
    entry().ref = ref;
  }

  void insertPrev(InstrRef ref) {
    block->sorted = (pos == 0);
    uint32_t newID = block->instrs.size();
    block->instrs.emplace_back(ref, IDImpl<uint32_t>{pos},
                               IDImpl<uint32_t>{entry().prev});
    entryOrderedPrev().next = newID;
    entry().prev = newID;
    auto &cfgMapEntry = cfg().map.get_ensure(ref);
    cfgMapEntry.block = block->ref;
    cfgMapEntry.blockPos = newID;
  }

  auto blockRef() const;

protected:
  CFG &cfg() const { return *block->cfg; }
  Block::Node &entry() const { return block->instrs[pos]; }
  Block::Node &entryOrderedPrev() const { return block->instrs[entry().prev]; }
  Block::Node &entryOrderedNext() const { return block->instrs[entry().next]; }
};

template <bool Ordered>
class BlockRef_iterator : public BlockRef_iterator_base {

public:
  using iterator_category = std::bidirectional_iterator_tag;

  BlockRef_iterator() = default;
  BlockRef_iterator(BlockRef block, uint32_t pos);
  BlockRef_iterator(BlockRef_iterator_base it) : BlockRef_iterator_base(it) {}

  BlockRef_iterator &operator++() {
    if constexpr (Ordered) {
      pos = entry().next;
    } else {
      ++pos;
    }
    return *this;
  }

  BlockRef_iterator operator++(int) {
    BlockRef_iterator tmp(*this);
    ++(*this);
    return tmp;
  }

  BlockRef_iterator &operator--() {
    if constexpr (Ordered) {
      pos = entry().prev;
    } else {
      --pos;
    }
    return *this;
  }

  BlockRef_iterator operator--(int) {
    BlockRef_iterator tmp(*this);
    --(*this);
    return tmp;
  }

  BlockRef_iterator succ() {
    BlockRef_iterator tmp(*this);
    ++tmp;
    return tmp;
  }

  BlockRef_iterator pred() {
    BlockRef_iterator tmp(*this);
    --tmp;
    return tmp;
  }

  friend bool operator==(const BlockRef_iterator &a,
                         const BlockRef_iterator &b) {
    return a.block == b.block && a.pos == b.pos;
  }

  bool isEnd() const;
};
static_assert(std::bidirectional_iterator<BlockRef_iterator<true>>);

class BlockRef : public FatObjRef<Block> {

public:
  using iterator = BlockRef_iterator<true>;
  using iterator_unordered = BlockRef_iterator<false>;

public:
  using FatObjRef::FatObjRef;
  BlockRef(FatObjRef<Block> ref) : FatObjRef<Block>(ref) {}

  size_t size() { return ptr->instrs.size() - 1; }

  iterator begin() const { return {*this, ptr->instrs[0].next}; }
  iterator end() const { return {*this, 0}; }

  iterator_unordered begin_unordered() { return {*this, 1}; }
  iterator_unordered end_unordered() {
    return iterator{*this, uint32_t(ptr->instrs.size())};
  }

  Range<iterator_unordered> unordered() {
    return {begin_unordered(), end_unordered()};
  }

  auto def() { return ptr->defUse.getSingleDef(); }
  auto defI() { return ptr->defUse.getSingleDef()->instr(); }

  bool empty() { return size() == 0; }

  void clear_unsafe() {
    ptr->instrs.clear();
    ptr->instrs.emplace_back(InstrRef{nullref}, IDImpl<uint32_t>{0},
                             IDImpl<uint32_t>{0});
  }

  void reserve(size_t count) { (*this)->instrs.reserve(count + 1); }

  CFG &getCFG() { return *ptr->cfg; }

  void sort() {
    // think copy is faster than in place, just guess though.
    auto &instrsOld = (*this)->instrs;
    SmallVec<Block::Node, 16> instrsNew{instrsOld.size()};
    instrsNew[0] =
        Block::Node{InstrRef{nullref}, ObjID{1}, ObjID{instrsNew.size() - 1}};

    size_t idx = 1;
    for (auto instr : *this) {
      uint32_t &pos = (*this)->cfg->map[instr].blockPos;
      instrsNew[idx].ref = instrsOld[pos].ref;
      instrsNew[idx].prev = idx - 1;
      instrsNew[idx].next = idx + 1;
      pos = idx;
      idx++;
    }
    instrsNew.back().next = 0;

    (*this)->instrs = std::move(instrsNew);
    (*this)->sorted = 1;
  }

  void sort_inplace() {
    // might still be broken
    size_t idx = 1;
    auto &instrs = (*this)->instrs;
    for (auto instr : *this) {
      uint32_t &pos = (*this)->cfg->map[instr].blockPos;

      if (pos == idx) {
        idx++;
        continue;
      }

      uint32_t a = instrs[idx].prev;
      uint32_t b = instrs[idx].next;

      instrs[instrs[pos].prev].next = idx;
      instrs[instrs[pos].next].prev = idx;

      instrs[a].next = pos;
      instrs[b].prev = pos;

      std::swap(instrs[idx], instrs[pos]);

      pos = idx;
      idx++;
    }
    (*this)->sorted = 1;
  }
};

template <bool Ordered>
inline BlockRef_iterator<Ordered>::BlockRef_iterator(BlockRef block,
                                                     uint32_t pos)
    : BlockRef_iterator_base(block, pos) {}

inline BlockRef_iterator_base::BlockRef_iterator_base(BlockRef block,
                                                      uint32_t pos)
    : block(block.getPtr()), pos(pos) {}

inline BlockRef_iterator_base CFG::operator[](ObjRef<Instr> ref) {
  assert(contains(ref));
  auto &refEntry = map[ref];
  auto blockRef = BlockRef{refEntry.block, blocks[refEntry.block]};
  return {blockRef, refEntry.blockPos};
}

inline auto BlockRef_iterator_base::blockRef() const {
  return BlockRef{block->ref.getObjID(), block};
}

template <bool Ordered> bool BlockRef_iterator<Ordered>::isEnd() const {
  if constexpr (Ordered)
    return *this == this->blockRef().end();
  else
    return *this == this->blockRef().end_unordered();
};

} // namespace dyno
