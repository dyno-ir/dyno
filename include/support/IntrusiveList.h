#pragma once

#include <cassert>
#include <cstddef>
#include <iterator>
#include <type_traits>

template <typename Derived> class IntrusiveList;

template <typename Derived> class IntrusiveListTraits {};

template <typename DerivedList> class IntrusiveListNode {
  friend class IntrusiveList<DerivedList>;
  using Traits = IntrusiveListTraits<DerivedList>;
  using DerivedINode = Traits::DerivedINode;
  using IList = IntrusiveList<DerivedList>;

public:
  IntrusiveListNode() = default;
  IntrusiveListNode(IList *parent, IntrusiveListNode *prev,
                    IntrusiveListNode *next)
      : parent(parent), prev(prev), next(next) {}
  ~IntrusiveListNode() { assert(!isLinked()); }

  auto &getNext() {
    assert(next);
    assert(next->isLinked());
    return *static_cast<DerivedINode *>(next);
  }

  auto &getPrev() {
    assert(prev);
    assert(prev->isLinked());
    return *static_cast<DerivedINode *>(prev);
  }

  bool hasNext() { return next && next->next; }
  bool hasPrev() { return prev && prev->prev; }

  IntrusiveListNode &getNextNode() {
    assert(next);
    return *next;
  }

  IntrusiveListNode &getPrevNode() {
    assert(prev);
    return *prev;
  }

  DerivedList &getParent() {
    assert(parent);
    return *static_cast<DerivedList *>(parent);
  }

  bool isLinked() { return parent && next && prev; }

  void unlink() {
    linkNeighbors();
    parent = nullptr;
    next = nullptr;
    prev = nullptr;
  }

  void insertNext(DerivedINode &o) {
    assert(!o.isLinked() && next && parent);
    o.parent = parent;
    o.next = next;
    o.prev = this;
    next->prev = &o;
    next = &o;
  }

  void insertPrev(DerivedINode &o) {
    assert(!o.isLinked() && prev && parent);
    o.parent = parent;
    o.next = this;
    o.prev = prev;
    prev->next = &o;
    prev = &o;
  }

  void deleteThis()
    requires Traits::OwnsNodes
  {
    linkNeighbors();
    delete static_cast<DerivedINode *>(this);
  }

protected:
  IntrusiveListNode(const IntrusiveListNode &o) = delete;
  IntrusiveListNode &operator=(const IntrusiveListNode &o) = delete;

  void linkNeighbors() {
    if (prev)
      prev->next = next;
    if (next)
      next->prev = prev;
  }

private:
  IList *parent = nullptr;
  IntrusiveListNode *prev = nullptr;
  IntrusiveListNode *next = nullptr;
};

template <typename Derived> class IntrusiveList {
public:
  using Traits = IntrusiveListTraits<Derived>;
  using DerivedINode = Traits::DerivedINode;
  using IList = IntrusiveList<Derived>;
  using INode = IntrusiveListNode<Derived>;

  IntrusiveList()
      : sentryBegin(this, nullptr, &sentryEnd),
        sentryEnd(this, &sentryBegin, nullptr) {}

  void insertBegin(DerivedINode &o) { sentryBegin.insertNext(o); }
  void insertEnd(DerivedINode &o) { sentryEnd.insertPrev(o); }

  void unlinkAll() {
    for (auto *node = sentryBegin.next; node != &sentryEnd;) {
      auto *tmp = node;
      node = node->next;
      tmp->parent = nullptr;
      tmp->next = nullptr;
      tmp->prev = nullptr;
    }
    sentryBegin.next = &sentryEnd;
    sentryEnd.prev = &sentryBegin;
  }

  void deleteAll()
    requires Traits::OwnsNodes
  {
    for (auto *node = sentryBegin.next; node != &sentryEnd;) {
      auto *tmp = node;
      node = node->next;
      tmp->parent = nullptr;
      delete static_cast<DerivedINode *>(tmp);
    }
    sentryBegin.next = &sentryEnd;
    sentryEnd.prev = &sentryBegin;
  }

  auto &getFirst() { return getSentryBegin().getNext(); }
  auto &getLast() { return getSentryEnd().getPrev(); }

  INode &getSentryBegin() { return sentryBegin; }
  INode &getSentryEnd() { return sentryEnd; }
  INode &getFirstSentry() { return getSentryBegin().getNextNode(); }

  class iterator {
  public:
    using iterator_category = std::bidirectional_iterator_tag;
    using value_type = DerivedINode;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type *;
    using reference = value_type &;

    iterator() = default;
    iterator(INode &ref) : mPtr(&ref) {}

    reference operator*() const { return static_cast<reference>(*mPtr); }
    pointer operator->() const { return static_cast<pointer>(mPtr); }

    iterator &operator++() {
      mPtr = &mPtr->getNextNode();
      return *this;
    }

    iterator operator++(int) {
      iterator tmp(*this);
      ++(*this);
      return tmp;
    }

    iterator &operator--() {
      mPtr = &mPtr->getPrevNode();
      return *this;
    }

    iterator operator--(int) {
      iterator tmp(*this);
      --(*this);
      return tmp;
    }

    friend bool operator==(const iterator &a, const iterator &b) {
      return a.mPtr == b.mPtr;
    }

    friend bool operator!=(const iterator &a, const iterator &b) {
      return a.mPtr != b.mPtr;
    }

  private:
    INode *mPtr = nullptr;
  };

  static_assert(std::bidirectional_iterator<iterator>);

  iterator begin() { return iterator(*sentryBegin.next); }
  iterator end() { return iterator(sentryEnd); }

  // auto rbegin() { return std::make_reverse_iterator(end()); }
  // auto rend() { return std::make_reverse_iterator(begin()); }

  bool empty() { return begin() == end(); }

protected:
  IntrusiveList(const IntrusiveList &o) = delete;
  IntrusiveList &operator=(const IntrusiveList &o) = delete;
  ~IntrusiveList() {
    if constexpr (Traits::OwnsNodes) {
      deleteAll();
    } else {
      unlinkAll();
    }
  }

private:
  INode sentryBegin;
  INode sentryEnd;
};
