#pragma once
#include "support/DenseMapInfo.h"
#include "support/Utility.h"
#include <array>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <utility>

// todo: factor out into Base, Small and Big
template <typename K, typename V> class DenseMap {
  using size_type = uint32_t;
  static constexpr size_type entriesPerBucket = 8;
  static constexpr K emptyKey = DenseMapInfo<K>::getEmptyKey();
  static constexpr K tombstoneKey = DenseMapInfo<K>::getTombstoneKey();

  struct Bucket {
    // buckets are searched linearly
    // keys are contiguous for SIMD compare
    // values are still here for better locality though
    std::array<K, entriesPerBucket> keys;
    std::array<V, entriesPerBucket> values;

    size_type getNextValid(size_type cur) {
      // todo: vectorize manually
      for (size_type i = cur + 1; i < keys.size(); i++) {
        if (keys[i] != emptyKey && keys[i] != tombstoneKey)
          return i;
      }
      // invalid
      return entriesPerBucket;
    }

    size_type find(const K &k, size_type cur = ~0) {
      // todo: vectorize manually
      for (size_type i = cur + 1; i < keys.size(); i++) {
        if (DenseMapInfo<K>::isEqual(keys[i], k))
          return i;
      }
      return entriesPerBucket;
    }

    Bucket() { std::fill(keys.begin(), keys.end(), emptyKey); }
  };

  struct iterator {
    // could store index in alignment bits
    Bucket *bucket;
    size_type rem;
    size_type idx;

    void next() {
      if (size_type next = bucket->getNextValid(idx); next != entriesPerBucket)
          [[likely]] {
        idx = next;
        return;
      }

      while (rem != 0) {
        bucket++;
        rem--;

        if (size_type next = bucket->getNextValid(~0);
            next != entriesPerBucket) {
          idx = next;
          return;
        }
      }

      idx = 0;
    }

  public:
    using iterator_category = std::forward_iterator_tag;
    using value_type = std::pair<K &, V &>;

    iterator erase() {
      bucket->values[idx].~V();
      bucket->keys[idx].~K();
      bucket->keys[idx] = tombstoneKey;

      iterator rv{*this};
      rv.next();
      return rv;
    }

    iterator &operator++() {
      next();
      return *this;
    }
    iterator operator++(int) {
      iterator tmp{*this};
      ++(*this);
      return tmp;
    }

    friend bool operator==(const iterator &lhs, const iterator &rhs) {
      if (lhs.bucket != rhs.bucket)
        return false;
      assert(lhs.rem == rhs.rem);
      return rhs.idx == lhs.idx;
    }

    value_type operator*() {
      return std::pair<K &, V &>(bucket->keys[idx], bucket->values[idx]);
    }
    // can't support this with non-contiguous key/val (maybe w proxy object)
    value_type operator->() = delete;

    const K &key() { return (**this).first; }
    V &val() { return (**this).second; }
  };

  Bucket *buckets;
  size_type cap;
  size_type sz;

  auto findImpl(const K &k) {
    assert(!DenseMapInfo<K>::isEqual(k, emptyKey) &&
           !DenseMapInfo<K>::isEqual(k, tombstoneKey));
    size_type bucketIndex = DenseMapInfo<K>::getHashValue(k) & (cap - 1);
    size_type offset = 1;
    while (true) {
      if (auto entryIndex = buckets[bucketIndex].find(k);
          entryIndex != entriesPerBucket) [[likely]] {
        return std::make_pair(true, iterator{&buckets[bucketIndex],
                                             cap - bucketIndex, entryIndex});
      }
      if (auto emptyIndex = buckets[bucketIndex].find(emptyKey);
          emptyIndex != entriesPerBucket) {
        return std::make_pair(false, iterator{&buckets[bucketIndex],
                                              cap - bucketIndex, emptyIndex});
      }

      bucketIndex = (bucketIndex + offset) & (cap - 1);
      offset += 1;
    }
    dyno_unreachable("full hash map");
  }

  void grow() {
    auto oldBuckets = buckets;
    auto oldCap = cap;

    buckets = new Bucket[cap *= 2]();
    sz = 0;

    for (size_t i = 0; i < oldCap; i++) {
      size_type j = ~0;
      while ((j = oldBuckets[i].getNextValid(j)) != entriesPerBucket) {
        insert(std::move(oldBuckets[i].keys[j]),
               std::move(oldBuckets[i].values[j]));
      }
    }

    delete[] oldBuckets;
  }

  bool growIfOversized() {
    auto max = cap * entriesPerBucket;
    if (sz > (max / 2) + (max / 4)) {
      grow();
      return true;
    }
    return false;
  }

public:
  // todo: copy/move construct
  DenseMap() : cap(1), sz(0) { buckets = new Bucket[1](); }
  ~DenseMap() { delete[] buckets; }

  iterator begin() {
    // maybe cache the ptr
    if (sz == 0)
      return end();
    auto it = iterator{buckets, cap, ~0U};
    it.next();
    return it;
  }
  iterator end() { return iterator{buckets + cap, 0, 0}; }

  iterator find(const K &k) {
    auto [found, iter] = findImpl(k);
    return found ? iter : end();
  }
  bool contains(const K &k) { return find(k) != end(); }

  iterator insert(const K &k, V &&v) {
    auto [found, iter] = findImpl(k);
    if (found)
      return end();

    if (growIfOversized())
      return insert(k, std::move(v));

    sz++;
    (*iter).first = k;
    (*iter).second = std::move(v);
    return iter;
  }
  iterator insert(const K &k, const V &v) { return insert(k, V{v}); }

  iterator insertOrAssign(const K &k, V &&v) {
    auto [found, iter] = findImpl(k);
    if (!found) {

      if (growIfOversized())
        return insert(k, std::move(v));

      sz++;
      (*iter).first = k;
    }
    (*iter).second = std::move(v);
    return iter;
  }
  iterator insertOrAssign(const K &k, const V &v) {
    return insertOrAssign(k, V{v});
  }

  V &operator[](const K &k) {
    auto [found, it] = findImpl(k);
    if (!found) {

      if (growIfOversized())
        return (*this)[k];

      sz++;
      (*it).first = k;
      (*it).second = V{};
    }
    return (*it).second;
  }

  void clear() {
    auto it = begin();
    while (it != end()) {
      it = it.erase();
    }
  }

  size_t size() const { return sz; }
};
