#pragma once

#include "support/Ranges.h"
#include <cassert>
#include <concepts>
#include <memory>
#include <memory_resource>
#include <vector>

template <typename T> class ObjectPoolTraits {
public:
  using id_type = unsigned;
  using vec_type = std::vector<T *>;
  static constexpr size_t alloc_size = sizeof(T);
  static constexpr size_t alloc_align = alignof(T);
};

template <typename T> class ObjectPool {
public:
  using Traits = ObjectPoolTraits<T>;
  using value_type = T;
  using id_type = Traits::id_type;

  struct FreeNode {
    FreeNode *next;
    id_type id;

    FreeNode(FreeNode *next, id_type id) : next(next), id(id) {}
  };
  static_assert(Traits::alloc_size >= sizeof(FreeNode));
  static_assert(Traits::alloc_align >= alignof(FreeNode));

private:
  FreeNode *freeStack = nullptr;
  std::pmr::monotonic_buffer_resource mem;
  Traits::vec_type elements;

  std::pair<T *, id_type> allocate() {
    if (freeStack) {
      FreeNode *node = freeStack;
      freeStack = node->next;
      id_type id = node->id;
      std::destroy_at(node);
      T *p = reinterpret_cast<T *>(node);
      elements[id] = p;
      return {p, id};
    }
    T *p = reinterpret_cast<T *>(
        mem.allocate(Traits::alloc_size, Traits::alloc_align));
    id_type id = elements.size();
    elements.push_back(p);
    return {p, id};
  }

  void shrinkToFit() {
    while (!elements.empty() && elements.back() == nullptr) {
      elements.pop_back();
    }
  }

  void deallocate(T *p, id_type id) {
    freeStack = std::construct_at(p, freeStack, id);
    assert(elements.size() > 0);
    if (id < elements.size() - 1) {
      elements[id] = nullptr;
    } else {
      elements.pop_back();
      shrinkToFit();
    }
  }

public:
  class iterator {
  public:
    using iterator_category = std::forward_iterator_tag;
    using value_type = T::value_type;
    using pointer = T::pointer;
    using reference = T::reference;
    using difference_type = T::difference_type;

    iterator() = default;
    iterator(T it, T itEnd) : it(it), itEnd(itEnd) {}

    reference operator*() { return **it; }

    iterator &operator++() {
      do {
        ++it;
      } while (it != itEnd && *it == nullptr);
      return *this;
    }

    iterator operator++(int) {
      iterator tmp(*this);
      ++(*this);
      return tmp;
    }

    friend bool operator==(const iterator &a, const iterator &b) {
      return a.it == b.it;
    }

    friend bool operator!=(const iterator &a, const iterator &b) {
      return a.it != b.it;
    }

  private:
    Traits::vec_type::iterator it;
    Traits::vec_type::iterator itEnd;
  };

  ObjectPool() {}

  ~ObjectPool() {
    for (auto *p : elements) {
      if (!p)
        continue;
      std::destroy_at(p);
    }
  }

  template <typename... Args> T &new_object(Args... args) {
    auto [p, id] = allocate();
    std::construct_at(p, id, args...);
    return *p;
  }

  void delete_object(T &o) {
    static_assert(std::same_as<decltype(((T *)nullptr)->getID()), id_type>);
    T *p = &o;
    id_type id = o.getID();
    std::destroy_at(&o);
    deallocate(p, id);
  }

  auto begin() { return iterator(elements.begin(), elements.end()); }
  auto end() { return iterator(elements.end(), elements.end()); }

  ObjectPool(const ObjectPool &) = delete;
  ObjectPool(ObjectPool &&) = delete;
  ObjectPool &operator=(const ObjectPool &) = delete;
  ObjectPool &operator=(ObjectPool &&) = delete;
};
