#pragma once

#include "dyno/Obj.h"
#include <cassert>
#include <support/Ranges.h>
#include <vector>

namespace dyno {

template <typename K, typename Container> class ObjMap {
  using Traits = ObjTraits<K>;

public:
  Container elements;
  using V = Container::value_type;

  void ensure(ObjRef<K> ref) {
    if (inRange(ref)) [[likely]] {
      return;
    }
    resize(ref.getObjID().num + 1);
  }

  bool inRange(ObjRef<K> ref) { return ref.getObjID() < elements.size(); }

  void reserve(size_t sz) { elements.reserve(sz); }

  void resize(size_t sz) { elements.resize(sz); }

  void resize(size_t sz, const V &v) { elements.resize(sz, v); }

  void clear() { elements.clear(); }

  size_t size() { return elements.size(); }

  /*template <typename VV> void sync(ObjMapVec<K, VV> &o) { resize(o.size());
   * }*/
  using value_reference = decltype(elements)::reference;

  value_reference operator[](ObjRef<K> ref) {
    assert(inRange(ref));
    return elements[ref.getObjID()];
  }

  value_reference get_ensure(ObjRef<K> ref) {
    ensure(ref);
    return this->operator[](ref);
  }

  class iterator {
    Container::iterator ptr;
    ObjID::num_t idx;

  public:
    using value_type =
        std::pair<ObjRef<K>, typename decltype(elements)::value_type>;
    using difference_type = std::ptrdiff_t;
    using reference =
        std::pair<ObjRef<K>, typename decltype(elements)::reference>;
    using pointer = std::pair<ObjRef<K>, typename decltype(elements)::pointer>;
    using iterator_category = std::random_access_iterator_tag;

    explicit iterator(Container::iterator ptr, ObjID::num_t idx)
        : ptr(ptr), idx(idx) {}

    iterator &operator++() {
      ++ptr;
      ++idx;
      return *this;
    }
    iterator operator++(int) {
      auto tmp{*this};
      ++ptr;
      ++idx;
      return tmp;
    }

    iterator &operator--() {
      --ptr;
      --idx;
      return *this;
    }
    iterator operator--(int) {
      auto tmp{*this};
      --ptr;
      --idx;
      return tmp;
    }

    iterator &operator+=(difference_type n) {
      ptr += n;
      idx += n;
      return *this;
    }
    iterator &operator-=(difference_type n) {
      ptr -= n;
      idx -= n;
      return *this;
    }

    reference operator*() const {
      return reference(ObjRef<K>{ObjID{idx}}, *ptr);
    }
    pointer operator->() const { return ptr; }

    iterator operator+(difference_type n) const {
      return iterator{ptr + n, idx + n};
    }
    iterator operator-(difference_type n) const {
      return iterator{ptr - n, idx - n};
    }

    difference_type operator-(const iterator &other) const {
      return ptr - other.ptr;
    }

    reference operator[](difference_type n) const { return *(ptr + n); }
    bool operator==(const iterator &other) const { return ptr == other.ptr; }
    auto operator<=>(const iterator &other) const { return ptr <=> other.ptr; }
  };

  auto begin() { return iterator{elements.begin(), 0}; }
  auto end() { return iterator{elements.end(), (uint32_t)elements.size()}; }
};

template <typename K, typename V> using ObjMapVec = ObjMap<K, std::vector<V>>;

} // namespace dyno
