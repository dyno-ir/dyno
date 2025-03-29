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
  };

  using iterator = Node *;

  InstrDefUse defUse;
  SmallVec<Node, 16> instrs;
  CFG *cfg;
  ObjRef<Block> ref;

public:
  Block(ObjRef<Block> ref, CFG &cfg) : cfg(&cfg), ref(ref) {
    instrs.emplace_back(InstrRef{nullref}, IDImpl<uint32_t>{0},
                        IDImpl<uint32_t>{0});
  }
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
  BlockRef_iterator_base(Block &block, uint32_t pos)
      : block(&block), pos(pos) {}

  value_type &operator*() const { return entry().ref; }
  pointer operator->() const { return &entry().ref; }

  void erase() {
    entryOrderedPrev().next = entry().next;
    entryOrderedNext().prev = entry().prev;
    if (block->instrs.erase_unordered(block->instrs.begin() + pos)) {
      entryOrderedPrev().next = pos;
      entryOrderedNext().prev = pos;
      cfg().map[entry().ref].blockPos = pos;
    }
  }

  void insertPrev(InstrRef ref) {
    uint32_t newID = block->instrs.size();
    block->instrs.emplace_back(ref, IDImpl<uint32_t>{pos},
                               IDImpl<uint32_t>{entry().prev});
    entryOrderedPrev().next = newID;
    entry().prev = newID;
    auto &cfgMapEntry = cfg().map.get_ensure(ref);
    cfgMapEntry.block = block->ref;
    cfgMapEntry.blockPos = newID;
  }

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
  BlockRef_iterator(Block &block, uint32_t pos)
      : BlockRef_iterator_base(block, pos) {}
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

  friend bool operator==(const BlockRef_iterator &a,
                         const BlockRef_iterator &b) {
    return a.block == b.block && a.pos == b.pos;
  }
};
static_assert(std::bidirectional_iterator<BlockRef_iterator<true>>);

class BlockRef : public FatObjRef<Block> {
public:
  using iterator = BlockRef_iterator<true>;
  using iterator_unordered = BlockRef_iterator<false>;

public:
  BlockRef(ObjRef<Block> obj, Block *block) : FatObjRef<Block>(obj, block) {}
  BlockRef(ObjRef<Block> obj, Block &block) : FatObjRef<Block>(obj, block) {}
  BlockRef(const FatObjRef<Block> &ref) : FatObjRef<Block>(ref) {}

  // these constructors are needed for casting impl (maybe we can somehow get rid of them...)
  BlockRef(ObjID obj, void *ptr) : FatObjRef<Block>(obj, ptr) {}
  BlockRef(nullref_t) : FatObjRef<Block>(nullref) {}

  size_t size() { return ptr->instrs.size() - 1; }

  iterator begin() { return {*ptr, ptr->instrs[0].next}; }

  iterator end() { return {*ptr, 0}; }

  iterator_unordered begin_unordered() { return {*ptr, ptr->instrs[0].next}; }

  iterator_unordered end_unordered() { return iterator{*ptr, 0}; }

  Range<iterator_unordered> unordered() {
    return {begin_unordered(), end_unordered()};
  }
};

inline BlockRef_iterator_base CFG::operator[](ObjRef<Instr> ref) {
  assert(contains(ref));
  auto &refEntry = map[ref];
  return {blocks[refEntry.block], refEntry.blockPos};
}

template <> struct ObjTraits<Block> {
  static constexpr DialectID dialect{DIALECT_CORE};
  static constexpr TyID ty{CORE_BLOCK};
  using RefT = BlockRef;
};

} // namespace dyno
