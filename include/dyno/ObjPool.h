#pragma once

#include <dyno/Obj.h>
#include <memory>
#include <memory_resource>

namespace dyno {

template <typename T> class ObjPool {
public:
  using Traits = ObjTraits<T>;
  using value_type = T;
  using vec_type = Traits::template vec_type<T *>;

  struct FreeNode {
    FreeNode *next;
    ObjID id;

    FreeNode(FreeNode *next, ObjID id) : next(next), id(id) {}
  };
  static_assert(Traits::alloc_size >= sizeof(FreeNode));
  static_assert(Traits::alloc_align >= alignof(FreeNode));

private:
  FreeNode *freeStack = nullptr;
  std::pmr::monotonic_buffer_resource mem;
  vec_type elements;

  std::pair<T *, ObjID> allocate() {
    if (freeStack) {
      FreeNode *node = freeStack;
      freeStack = node->next;
      ObjID id = node->id;
      std::destroy_at(node);
      T *p = reinterpret_cast<T *>(node);
      elements[id] = p;
      return {p, id};
    }
    T *p = reinterpret_cast<T *>(
        mem.allocate(Traits::alloc_size, Traits::alloc_align));
    ObjID id{elements.size()};
    elements.push_back(p);
    return {p, id};
  }

  void shrinkToFit() {
    while (!elements.empty() && elements.back() == nullptr) {
      elements.pop_back();
    }
  }

  void deallocate(T *p, ObjID id) {
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

  ObjPool() {}

  ~ObjPool() {
    for (auto *p : elements) {
      if (!p)
        continue;
      std::destroy_at(p);
    }
  }

  template <typename... Args> T &new_object(Args &&...args) {
    auto [p, id] = allocate();
    std::construct_at(p, id, std::forward<Args>(args)...);
    return *p;
  }

  void delete_object(T &o, ObjID id) {
    T *p = &o;
    std::destroy_at(&o);
    deallocate(p, id);
  }

  T *operator[](ObjID id) { return elements[id.num]; }

  auto begin() { return iterator(elements.begin(), elements.end()); }
  auto end() { return iterator(elements.end(), elements.end()); }

  size_t capacity() { return elements.size(); }

  ObjPool(const ObjPool &) = delete;
  ObjPool(ObjPool &&) = delete;
  ObjPool &operator=(const ObjPool &) = delete;
  ObjPool &operator=(ObjPool &&) = delete;
};

template <typename T, typename Derived> class ObjPoolStore {
private:
  ObjPool<T> pool;
  using Traits = ObjTraits<T>;

public:
  template <typename... Args> FatDynObjRef<T> &create(Args &&...args) {
    return pool.new_object(std::forward<Args>(args)...);
  }

  T &get(DynObjRef ref) {
    assert(ref.dialect == Traits::dialect);
    assert(ref.ty == Traits::ty);
    return pool[ref.obj];
  }

  void destroy(DynObjRef ref) { pool.delete_object(get(ref), ref.obj); }
  void destroy(FatDynObjRef<T> ref) { pool.delete_object(*ref, ref.thin().obj); }

  auto begin() { return pool.begin(); }
  auto end() { return pool.end(); }
};

} // namespace dyno
