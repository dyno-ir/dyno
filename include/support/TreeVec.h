#include "support/PointerVariant.h"
#include "support/SmallVec.h"
#include "support/Utility.h"
#include <iterator>
#include <memory>
class SmallTreeVec {
  constexpr static uint32_t LeafSize = 4;
  using T = int;
  struct Leaf;
  struct Inner;
  class Ptr : public PointersIntsVariant<Inner *, Leaf *> {
  public:
    using PointersIntsVariant::PointersIntsVariant;

    auto &parent() {
      if (auto asLeaf = dyn_as<Leaf *>()) {
        return (*asLeaf)->parent;
      } else if (auto asInner = dyn_as<Inner *>()) {
        return (*asInner)->parent;
      } else
        unreachable();
    }
    auto &pos() {
      if (auto asLeaf = dyn_as<Leaf *>()) {
        return (*asLeaf)->pos;
      } else if (auto asInner = dyn_as<Inner *>()) {
        return (*asInner)->pos;
      } else
        unreachable();
    }
  };

  struct Leaf {
    Inner *parent;
    uint32_t pos;
    SmallVec<T, LeafSize> elements;
  };
  struct Inner {
    Inner *parent;
    uint32_t pos;
    SmallVec<Ptr, LeafSize> elements;

    void updateLinks() {
      for (uint32_t i = 0; i < elements.size(); i++) {

        if (auto asLeaf = elements[i].dyn_as<Leaf *>()) {
          (*asLeaf)->parent = this;
          (*asLeaf)->pos = i;
        } else if (auto asInner = elements[i].dyn_as<Inner *>()) {
          (*asInner)->parent = this;
          (*asInner)->pos = i;
        }
      }
    }
  };

  union {
    Leaf inlineLeaf;
    Inner inlineInner;
  };
  Ptr ptr;

public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = T;
  using reference = T &;
  using pointer = T *;

  SmallTreeVec() : inlineLeaf(), ptr(&inlineLeaf) {}
  ~SmallTreeVec() {
    if (auto asLeaf = ptr.dyn_as<Leaf *>()) {
      std::destroy_at(*asLeaf);
    } else if (auto asInner = ptr.dyn_as<Inner *>()) {
      std::destroy_at(*asInner);
    } else
      unreachable();
  }
  class iterator {
    friend class SmallTreeVec;

    Leaf *ptr;
    uint32_t idx;

  public:
    using iterator_category = std::forward_iterator_tag;
    using value_type = T;
    using reference = T &;
    using pointer = T *;
    using difference_type = int;

    iterator(Leaf *ptr, uint32_t idx) : ptr(ptr), idx(idx) {}

    T &operator*() const { return ptr->elements[idx]; }
    T *operator->() const { return &ptr->elements[idx]; }

    iterator &operator++() {
      idx++;
      if (idx < ptr->elements.size()) [[likely]]
        return *this;

      idx = ptr->pos;
      Inner *inner = ptr->parent;

      while (inner) {
        ++idx;
        if (idx < inner->elements.size()) {
          while (1) {
            Ptr elem = inner->elements[idx];
            if (auto asLeaf = elem.template dyn_as<Leaf *>()) {
              idx = 0;
              ptr = *asLeaf;
              return *this;
            }
            inner = elem.template as<Inner *>();
            idx = 0;
          }
          unreachable();
        }

        idx = inner->pos;
        inner = inner->parent;
      }

      ptr = nullptr;
      idx = 0;
      return *this;
    }

    bool operator==(const iterator &o) const {
      return ptr == o.ptr && idx == o.idx;
    }
  };

  iterator begin() {
    Ptr p = ptr;
    while (!p.template is<Leaf *>())
      p = p.template as<Inner *>()->elements[0];
    auto asLeaf = p.template as<Leaf *>();
    if (asLeaf->elements.empty())
      return end();
    return iterator(asLeaf, 0);
  }
  iterator end() { return iterator(nullptr, 0); }

  void insert(iterator it, T &&val) {
    if (it == end()) {
      if (begin() == end()) {
        inlineLeaf.elements.push_back(std::move(val));
        return;
      }
      dyno_unreachable("todo");
    }

    if (it.ptr->elements.size() < LeafSize) [[likely]] {
      it.ptr->elements.insert(it.ptr->elements.begin() + it.idx,
                              std::move(val));
      return;
    }
    constexpr uint32_t HalfLeafSize = round_up_div(LeafSize, 2u);
    Leaf *left = it.ptr;

    // if we were inline copy left out of line and build inline
    if (left == &inlineLeaf) {
      left = new Leaf(std::move(*left));
      std::destroy_at(&inlineLeaf);
      std::construct_at(&inlineInner);
      left->parent = &inlineInner;
      left->pos = 0;
      inlineInner.elements.push_back(left);
      ptr = &inlineInner;
    }

    Leaf *right = new Leaf;
    right->elements.push_back_range(
        Range{left->elements.begin() + HalfLeafSize, left->elements.end()});
    left->elements.resize(HalfLeafSize);
    if (it.idx < HalfLeafSize) {
      left->elements.insert(left->elements.begin() + it.idx, std::move(val));
    } else {
      right->elements.insert(right->elements.begin() + it.idx - HalfLeafSize,
                             std::move(val));
    }

    Ptr toInsert = right;
    auto insertPos = left->pos + 1;
    auto *parent = left->parent;

    while (1) {
      // Parent inner has enough space for new leaf
      if (parent->elements.size() < LeafSize) [[likely]] {

        toInsert.parent() = parent;

        parent->elements.insert(parent->elements.begin() + insertPos, toInsert);
        for (auto [i, ptr] :
             Range{parent->elements.begin() + insertPos, parent->elements.end()}
                 .enumerate()) {
          ptr.pos() = i + insertPos;
        }
        return;
      }

      // Not enough space, create new inner
      Inner *innerLeft = parent;
      Inner *innerRight = new Inner();

      if (innerLeft == &inlineInner) {
        innerLeft = new Inner(*innerLeft);
        innerLeft->updateLinks();
        inlineInner.elements.clear();
        inlineInner.elements.push_back(innerLeft);
        innerLeft->parent = &inlineInner;
        innerLeft->pos = 0;
      }

      innerRight->elements.push_back_range(
          Range{innerLeft->elements.begin() + HalfLeafSize,
                innerLeft->elements.end()});
      innerLeft->elements.resize(HalfLeafSize);

      if (insertPos < HalfLeafSize) {
        innerLeft->elements.insert(innerLeft->elements.begin() + insertPos,
                                   toInsert);
        toInsert.parent() = innerLeft;
        for (uint32_t i = insertPos; i < innerLeft->elements.size(); i++) {
          innerLeft->elements[i].pos() = i;
        }
      } else {
        innerRight->elements.insert(
            innerRight->elements.begin() + insertPos - HalfLeafSize, toInsert);
      }
      innerRight->updateLinks();
      // insert right into parent

      parent = innerLeft->parent;
      insertPos = innerLeft->pos + 1;
      toInsert = innerRight;
    }
  }
};
