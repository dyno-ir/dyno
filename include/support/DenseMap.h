#pragma once
#include "support/DenseMapInfo.h"
#include "support/InlineStorage.h"
#include "support/Ranges.h"
#include "support/Utility.h"
#include <array>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <memory>
#include <new>
#include <utility>

template <typename Derived, typename K, typename Bucket, typename Iterator>
class DenseSetMapBase {
public:
  using size_type = uint32_t;
  using iterator = Iterator;
  size_type cap;
  size_type sz;

  Bucket *&getBuckets() const {
    return static_cast<const Derived *>(this)->getBuckets();
  }
  Derived &self() { return *static_cast<Derived *>(this); }

  DenseSetMapBase(size_type cap, size_type sz) : cap(cap), sz(sz) {}

  auto findImpl(const K &k) const {
    assert(!DenseMapInfo<K>::isEqual(k, DenseMapInfo<K>::getEmptyKey()) &&
           !DenseMapInfo<K>::isEqual(k, DenseMapInfo<K>::getTombstoneKey()));
    size_type bucketIndex = DenseMapInfo<K>::getHashValue(k) & (cap - 1);
    size_type offset = 1;
    while (true) {
      if (auto entryIndex = getBuckets()[bucketIndex].find(k);
          entryIndex != Bucket::entriesPerBucket) {
        return std::make_pair(true,
                              iterator{&getBuckets()[bucketIndex],
                                       cap - bucketIndex, entryIndex, offset});
      }
      if (auto emptyIndex =
              getBuckets()[bucketIndex].find(DenseMapInfo<K>::getEmptyKey());
          emptyIndex != Bucket::entriesPerBucket) {
        return std::make_pair(false,
                              iterator{&getBuckets()[bucketIndex],
                                       cap - bucketIndex, emptyIndex, offset});
      }

      bucketIndex = (bucketIndex + offset) & (cap - 1);
      offset += 1;
    }
    dyno_unreachable("full hash map");
  }

  iterator findEmptyImpl(const K &k, bool assertNotExist = true) const {
    assert(!DenseMapInfo<K>::isEqual(k, DenseMapInfo<K>::getEmptyKey()) &&
           !DenseMapInfo<K>::isEqual(k, DenseMapInfo<K>::getTombstoneKey()));
    size_type bucketIndex = DenseMapInfo<K>::getHashValue(k) & (cap - 1);
    size_type offset = 1;
    while (true) {

      if (assertNotExist)
        if (auto entryIndex = getBuckets()[bucketIndex].find(k);
            entryIndex != Bucket::entriesPerBucket) {
          dyno_unreachable("key exists");
        }

      if (auto emptyIndex = getBuckets()[bucketIndex].findEmptyOrTombstone();
          emptyIndex != Bucket::entriesPerBucket) {
        return iterator{&getBuckets()[bucketIndex], cap - bucketIndex,
                        emptyIndex, offset};
      }

      bucketIndex = (bucketIndex + offset) & (cap - 1);
      offset += 1;
    }
    dyno_unreachable("full hash map");
  }

  template <std::invocable<Bucket *, size_type> T> void grow(T &&reinsertFunc) {
    auto oldBuckets = getBuckets();
    auto oldCap = cap;

    getBuckets() = new Bucket[cap *= 2]();
    sz = 0;

    for (size_t i = 0; i < oldCap; i++) {
      size_type j = ~0;
      while ((j = oldBuckets[i].getNextValid(j)) != Bucket::entriesPerBucket) {
        reinsertFunc(&oldBuckets[i], j);
      }
    }
    self().deleteArr(oldBuckets);
  }

  template <std::invocable<Bucket *, size_type> T>
  bool growIfOversized(T &&reinsertFunc) {
    auto max = cap * Bucket::entriesPerBucket;
    if (sz > (max / 2) + (max / 4)) {
      grow(reinsertFunc);
      return true;
    }
    return false;
  }

public:
  iterator begin() const {
    // maybe cache the ptr
    if (sz == 0)
      return end();
    auto it = iterator{getBuckets(), cap, ~0U};
    it.next();
    return it;
  }
  iterator end() const { return iterator{getBuckets() + cap, 0, 0}; }

  iterator find(const K &k) const {
    auto [found, iter] = findImpl(k);
    return found ? iter : end();
  }
  bool contains(const K &k) { return find(k) != end(); }

public:
  size_t size() const { return sz; }
  bool empty() const { return sz == 0; }
};

template <typename Bucket, typename K, typename size_type = uint32_t>
struct DenseSetMapIteratorBase {
  // could store index in alignment bits
  Bucket *bucket;
  size_type rem;
  size_type idx;

  void next() {
    if (size_type next = bucket->getNextValid(idx);
        next != Bucket::entriesPerBucket) [[likely]] {
      idx = next;
      return;
    }

    while (rem != 0) {
      bucket++;
      if (--rem == 0)
        break;

      if (size_type next = bucket->getNextValid(~0);
          next != Bucket::entriesPerBucket) {
        idx = next;
        return;
      }
    }

    idx = 0;
  }

public:
  static_assert(std::is_trivially_destructible_v<K>);

  DenseSetMapIteratorBase(const DenseSetMapIteratorBase &) = default;
  DenseSetMapIteratorBase(DenseSetMapIteratorBase &&) = default;
  DenseSetMapIteratorBase &operator=(const DenseSetMapIteratorBase &) = default;
  DenseSetMapIteratorBase &operator=(DenseSetMapIteratorBase &&) = default;
  DenseSetMapIteratorBase() = default;

  DenseSetMapIteratorBase(Bucket *bucket, size_type rem, size_type idx)
      : bucket(bucket), rem(rem), idx(idx) {}
  DenseSetMapIteratorBase(Bucket *bucket, size_type rem, size_type idx,
                          size_type)
      : bucket(bucket), rem(rem), idx(idx) {}

  DenseSetMapIteratorBase &operator++() {
    next();
    return *this;
  }
  DenseSetMapIteratorBase operator++(int) {
    DenseSetMapIteratorBase tmp{*this};
    ++(*this);
    return tmp;
  }

  friend bool operator==(const DenseSetMapIteratorBase &lhs,
                         const DenseSetMapIteratorBase &rhs) {
    if (lhs.bucket != rhs.bucket)
      return false;
    assert(lhs.rem == rhs.rem);
    return rhs.idx == lhs.idx;
  }

  explicit operator bool() { return rem != 0; }

  const K &key() { return bucket->keys[idx]; }
  K &keyMut() { return bucket->keys[idx]; }
};

template <typename Bucket, typename K, typename V,
          typename size_type = uint32_t>
struct DenseMapIterator : public DenseSetMapIteratorBase<Bucket, K, size_type> {
public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = std::pair<const K &, V &>;
  using difference_type = int;
  using pointer = const K *;
  using reference = const K &;

  using Base = DenseSetMapIteratorBase<Bucket, K, size_type>;
  using Base::Base;
  using Base::bucket;
  using Base::idx;
  using Base::rem;

  DenseMapIterator erase() {
    bucket->values[idx].~V();
    bucket->keys[idx].~K();
    bucket->keys[idx] = DenseMapInfo<K>::getTombstoneKey();

    DenseMapIterator rv{*this};
    rv.next();
    return rv;
  }

  value_type operator*() {
    return std::pair<const K &, V &>(bucket->keys[idx], bucket->values[idx]);
  }
  // can't support this with non-contiguous key/val (maybe w proxy object)
  auto operator->() = delete;

  V &val() { return (**this).second; }
};

template <typename Bucket, typename K, typename size_type = uint32_t>
struct DenseSetIterator : public DenseSetMapIteratorBase<Bucket, K, size_type> {
public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = const K &;
  using difference_type = int;
  using pointer = const K *;
  using reference = const K &;

  using Base = DenseSetMapIteratorBase<Bucket, K, size_type>;
  using Base::Base;
  using Base::bucket;
  using Base::idx;
  using Base::rem;

  DenseSetIterator erase() {
    bucket->values[idx].~V();
    bucket->keys[idx].~K();
    bucket->keys[idx] = DenseMapInfo<K>::getTombstoneKey();

    DenseSetIterator rv{*this};
    rv.next();
    return rv;
  }

  value_type operator*() { return bucket->keys[idx]; }
  pointer operator->() { return &bucket->keys[idx]; };
};

template <typename Derived, typename K, typename V, typename Bucket,
          typename Iterator = DenseMapIterator<Bucket, K, V>>
class DenseMapBase : public DenseSetMapBase<Derived, K, Bucket, Iterator> {
  using Base = DenseSetMapBase<Derived, K, Bucket, Iterator>;

  bool growIfOversized() {
    return Base::growIfOversized([&](Bucket *bucket, size_type j) {
      insert(std::move(bucket->keys[j]), std::move(bucket->values[j]));
      std::destroy_at(&bucket->values[j]);
    });
  }

protected:
  using Base::cap;
  using Base::getBuckets;
  using Base::self;
  using Base::sz;

public:
  using iterator = Base::iterator;
  using size_type = Base::size_type;
  using bucket_type = Bucket;
  using Base::begin;
  using Base::end;

  iterator insert(const K &k, V &&v) {
    growIfOversized();
    auto iter = Base::findEmptyImpl(k);
    sz++;
    iter.keyMut() = k;
    (*iter).second = std::move(v);
    return iter;
  }
  iterator insert(const K &k, const V &v) { return insert(k, V{v}); }

  iterator insertOrAssign(const K &k, V &&v) {
    auto [found, iter] = Base::findImpl(k);
    if (!found) {

      if (growIfOversized())
        return insert(k, std::move(v));

      sz++;
      iter.keyMut() = k;
    }
    (*iter).second = std::move(v);
    return iter;
  }
  iterator insertOrAssign(const K &k, const V &v) {
    return insertOrAssign(k, V{v});
  }

  template <std::invocable T> auto findOrInsert(const K &k, T &&func) {
    auto [found, iter] = Base::findImpl(k);
    if (found)
      return std::make_pair(true, iter);

    if (growIfOversized())
      return std::make_pair(false, insert(k, func()));

    sz++;
    iter.keyMut() = k;
    (*iter).second = func();
    return std::make_pair(false, iter);
  }

  auto findOrInsert(const K &k, const V &&newVal) {
    return findOrInsert(k, [&] { return std::move(newVal); });
  }

  auto findOrInsert(const K &k, const V &newVal) {
    return findOrInsert(k, V{newVal});
  }

  V &operator[](const K &k) {
    auto [found, it] = Base::findImpl(k);
    if (!found) {

      if (growIfOversized())
        return (*this)[k];

      sz++;
      it.keyMut() = k;
      std::construct_at(&(*it).second);
    }
    return (*it).second;
  }

  void clear() {
    if (sz == 0)
      return;
    if constexpr (std::is_trivially_destructible_v<V>) {
      for (size_t i = 0; i < cap; i++)
        getBuckets()[i].setKeysEmpty();
      sz = 0;
      return;
    }

    auto it = begin();
    while (it != end()) {
      it = it.erase();
    }
    sz = 0;
  }

protected:
  void clearDelete() {
    if constexpr (std::is_trivially_destructible_v<V>)
      return;

    auto it = begin();
    while (it != end()) {
      it = it.erase();
    }
  }

  DenseMapBase(size_type cap, size_type sz) : Base(cap, sz) {}
};

template <typename Derived, typename K, typename Bucket>
class DenseSetBase
    : public DenseSetMapBase<Derived, K, Bucket, DenseSetIterator<Bucket, K>> {
  using Base = DenseSetMapBase<Derived, K, Bucket, DenseSetIterator<Bucket, K>>;

protected:
  using Base::cap;
  using Base::getBuckets;
  using Base::self;
  using Base::sz;

public:
  using iterator = Base::iterator;
  using size_type = Base::size_type;
  using bucket_type = Bucket;
  using Base::begin;
  using Base::end;

  bool growIfOversized() {
    return Base::growIfOversized([&](Bucket *bucket, size_type j) {
      insert(std::move(bucket->keys[j]));
    });
  }

  iterator insert(const K &k) {
    growIfOversized();
    auto iter = Base::findEmptyImpl(k);
    sz++;
    iter.keyMut() = k;
    return iter;
  }

  template <typename It> void insert(Range<It> arr) {
    for (const K &elem : arr) {
      insert(elem);
    }
  }

  auto findOrInsert(const K &k) {
    auto [found, iter] = Base::findImpl(k);
    if (found)
      return std::make_pair(true, iter);

    if (growIfOversized())
      return std::make_pair(false, insert(k));

    sz++;
    iter.keyMut() = k;
    return std::make_pair(false, iter);
  }

  template <typename It> void findOrInsert(Range<It> arr) {
    for (const K &elem : arr) {
      findOrInsert(elem);
    }
  }

  void clear() {
    if (sz == 0)
      return;
    for (size_t i = 0; i < cap; i++)
      getBuckets()[i].setKeysEmpty();
  }
  DenseSetBase(size_type cap, size_type sz) : Base(cap, sz) {}
};

template <typename K, typename size_type = uint32_t> struct DenseSetBucket {
  static constexpr size_type entriesPerBucket = 8;
  // buckets are searched linearly
  // keys are contiguous for SIMD compare
  // values are still here for better locality though
  std::array<K, entriesPerBucket> keys;

  size_type getNextValid(size_type cur) {
    // todo: vectorize manually
    for (size_type i = cur + 1; i < keys.size(); i++) {
      if (!DenseMapInfo<K>::isEqual(keys[i], DenseMapInfo<K>::getEmptyKey()) &&
          !DenseMapInfo<K>::isEqual(keys[i],
                                    DenseMapInfo<K>::getTombstoneKey()))
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

  size_type findEmptyOrTombstone(size_type cur = ~0) {
    // todo: vectorize manually
    for (size_type i = cur + 1; i < keys.size(); i++) {
      if (DenseMapInfo<K>::isEqual(keys[i], DenseMapInfo<K>::getEmptyKey()) ||
          DenseMapInfo<K>::isEqual(keys[i], DenseMapInfo<K>::getTombstoneKey()))
        return i;
    }
    return entriesPerBucket;
  }

  void setKeysEmpty() {
    std::fill(keys.begin(), keys.end(), DenseMapInfo<K>::getEmptyKey());
  }

  DenseSetBucket() { setKeysEmpty(); }
};

template <typename K, typename V, typename size_type = uint32_t>
struct DenseMapBucket : DenseSetBucket<K, size_type> {
  using Base = DenseSetBucket<K, size_type>;
  std::array<V, Base::entriesPerBucket> values;
};

template <typename Base> class LargeSetMap : public Base {
  using Bucket = Base::bucket_type;
  Bucket *buckets;

public:
  Bucket *&getBuckets() const { return const_cast<Bucket *&>(buckets); }
  void deleteArr(Bucket *buckets) { ::operator delete[](buckets); }

public:
  // todo: copy/move construct
  LargeSetMap() : Base(1, 0) {
    buckets = new Bucket[1]();
  }
  ~LargeSetMap() {
    this->Base::clearDelete();
    ::operator delete[](buckets);
  }
};

template <typename Base, size_t InlineBuckets> class SmallSetMap : public Base {
  using Bucket = Base::bucket_type;
  Bucket *buckets;
  InlineStorageArr<Bucket, InlineBuckets> arr;

public:
  Bucket *&getBuckets() const { return const_cast<Bucket *&>(buckets); }
  void deleteArr(Bucket *buckets) {
    if (buckets == *arr)
      return;
    ::operator delete[](buckets);
  }

public:
  bool isSmall() { return buckets == *arr; }

  SmallSetMap() : Base(InlineBuckets, 0), arr() {
    std::uninitialized_default_construct_n(*arr, InlineBuckets);
    buckets = *arr;
  }
  ~SmallSetMap() {
    if (isSmall())
      return;
    ::operator delete[](buckets);
  }
};

template <typename K, typename V>
class DenseMap : public LargeSetMap<
                     DenseMapBase<DenseMap<K, V>, K, V, DenseMapBucket<K, V>>> {
  using Base =
      LargeSetMap<DenseMapBase<DenseMap<K, V>, K, V, DenseMapBucket<K, V>>>;
  using Base::Base;
};

template <typename K, typename V, size_t InlineBuckets>
class SmallDenseMap
    : public SmallSetMap<DenseMapBase<SmallDenseMap<K, V, InlineBuckets>, K, V,
                                      DenseMapBucket<K, V>>,
                         InlineBuckets> {
  using Base = SmallSetMap<DenseMapBase<SmallDenseMap<K, V, InlineBuckets>, K,
                                        V, DenseMapBucket<K, V>>,
                           InlineBuckets>;
  using Base::Base;
};

template <typename K>
class DenseSet
    : public LargeSetMap<DenseSetBase<DenseSet<K>, K, DenseSetBucket<K>>> {
  using Base = LargeSetMap<DenseSetBase<DenseSet<K>, K, DenseSetBucket<K>>>;
  using Base::Base;
};

template <typename K, size_t InlineBuckets>
class SmallDenseSet
    : public SmallSetMap<
          DenseSetBase<SmallDenseSet<K, InlineBuckets>, K, DenseSetBucket<K>>,
          InlineBuckets> {
  using Base = SmallSetMap<
      DenseSetBase<SmallDenseSet<K, InlineBuckets>, K, DenseSetBucket<K>>,
      InlineBuckets>;
  using Base::Base;
};
