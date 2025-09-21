#pragma once
#include "support/ASAN.h"
#include "support/Bits.h"
#include "support/DenseMapInfo.h"
#include "support/InlineStorage.h"
#include "support/Ranges.h"
#include "support/TemplateUtil.h"
#include "support/Utility.h"
#include <array>
#include <bit>
#include <concepts>
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
    size_type startBucketIndex = bucketIndex;
    size_type offset = 1;

    bool sawTombstone = false;
    size_type seenTombstoneIdx;
    size_type seenTombstoneBucket;

    while (true) {
      if (auto entryIndex = getBuckets()[bucketIndex].find(k);
          entryIndex != Bucket::entriesPerBucket) {
        return std::make_pair(true,
                              iterator{&getBuckets()[bucketIndex],
                                       cap - bucketIndex, entryIndex, offset});
      }
      if (!sawTombstone) {
        if (auto tombstoneIndex = getBuckets()[bucketIndex].find(
                DenseMapInfo<K>::getTombstoneKey());
            tombstoneIndex != Bucket::entriesPerBucket) {
          sawTombstone = true;
          seenTombstoneIdx = tombstoneIndex;
          seenTombstoneBucket = bucketIndex;
        }
      }
      if (auto emptyIndex =
              getBuckets()[bucketIndex].find(DenseMapInfo<K>::getEmptyKey());
          emptyIndex != Bucket::entriesPerBucket) {
        // if we already saw a tombstone, break to return that.
        if (sawTombstone)
          break;
        return std::make_pair(false,
                              iterator{&getBuckets()[bucketIndex],
                                       cap - bucketIndex, emptyIndex, offset});
      }

      bucketIndex = (bucketIndex + offset) & (cap - 1);
      offset += 1;
      if (bucketIndex == startBucketIndex) [[unlikely]]
        break;
    }
    assert(sawTombstone && "hash map full?");
    return std::make_pair(false, iterator{&getBuckets()[bucketIndex],
                                          cap - seenTombstoneBucket,
                                          seenTombstoneIdx, offset});
  }

  iterator findEmptyImpl(const K &k, bool assertNotExist = true) const {
    assert(!DenseMapInfo<K>::isEqual(k, DenseMapInfo<K>::getEmptyKey()) &&
           !DenseMapInfo<K>::isEqual(k, DenseMapInfo<K>::getTombstoneKey()));
    size_type bucketIndex = DenseMapInfo<K>::getHashValue(k) & (cap - 1);
    size_type offset = 1;
    while (true) {

#ifdef _DEBUG_
      if (assertNotExist)
        if (auto entryIndex = getBuckets()[bucketIndex].find(k);
            entryIndex != Bucket::entriesPerBucket) {
          dyno_unreachable("key exists");
        }
#endif

      if (auto emptyIndex = getBuckets()[bucketIndex].findEmptyOrTombstone();
          emptyIndex != Bucket::entriesPerBucket) {
        return iterator{&getBuckets()[bucketIndex], cap - bucketIndex,
                        emptyIndex, offset};
      }

      bucketIndex = (bucketIndex + offset) & (cap - 1);
      offset += 1;
    }
  }

  template <std::invocable<Bucket *, size_type> T> void grow(T &&reinsertFunc) {
    auto oldBuckets = getBuckets();
    auto oldCap = cap;

    getBuckets() = (Bucket *)::operator new[](
        sizeof(Bucket) * (cap *= 2), std::align_val_t(alignof(Bucket)));
    std::uninitialized_default_construct_n(getBuckets(), cap);

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
    auto pct = (max / 2) + (max / 4) + (max / 8) + (max / 16);
    if (sz >= pct) {
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

  // DenseMapIterator erase() {
  //   bucket->values[idx].~V();
  //   bucket->keys[idx].~K();
  //   bucket->keys[idx] = DenseMapInfo<K>::getTombstoneKey();

  //   DenseMapIterator rv{*this};
  //   rv.next();
  //   return rv;
  // }

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

  // private:
  //   DenseSetIterator erase() {
  //     bucket->keys[idx].~K();
  //     bucket->keys[idx] = DenseMapInfo<K>::getTombstoneKey();

  //     DenseSetIterator rv{*this};
  //     rv.next();
  //     return rv;
  //   }

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
    std::construct_at(&(*iter).second, std::move(v));
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
      std::construct_at(&(*iter).second, std::move(v));
    } else
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
    std::construct_at(&(*iter).second, std::move(func()));
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
      it = erase(it);
    }
    sz = 0;
  }

  iterator erase(iterator it) {
    assert(it != end());
    auto rv = std::next(it);
    it.keyMut() = DenseMapInfo<K>::getTombstoneKey();
    std::destroy_at(&it.val());
    --sz;
    return rv;
  }

protected:
  void clearDelete() {
    if constexpr (std::is_trivially_destructible_v<V>)
      return;

    auto it = begin();
    while (it != end()) {
      it = erase(it);
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
  iterator erase(iterator it) {
    assert(it != end());
    auto rv = std::next(it);
    it.keyMut() = DenseMapInfo<K>::getTombstoneKey();
    --sz;
    return rv;
  }

  DenseSetBase(size_type cap, size_type sz) : Base(cap, sz) {}
};

template <typename K, typename size_type = uint32_t> struct DenseSetBucket {
#define SIMD_DENSE_MAP 1
#if defined(__clang__) && SIMD_DENSE_MAP
#ifdef __AVX512F__
  static constexpr size_type simdWidth = 64;
#elifdef __AVX__
  static constexpr size_type simdWidth = 32;
#else
  static constexpr size_type simdWidth = 16;
#endif
  static constexpr size_type entriesPerBucket =
      std::max(simdWidth / size_type(sizeof(K)), size_type(1));

  static constexpr size_t vector_len =
      std::min(std::max(simdWidth / size_type(sizeof(K)), size_type(1)),
               entriesPerBucket);

  // buckets are searched linearly
  // keys are contiguous for SIMD compare
  // values are still here for better locality though
  std::array<K, entriesPerBucket> keys alignas(vector_len * sizeof(K));

  template <bool Inverse, typename... Args>
  size_type findVec(size_type cur = ~0, Args... k) {
    using key_unsigned = uint_of_size<sizeof(K)>::type;
    typedef key_unsigned key_vec __attribute__((ext_vector_type(vector_len)));
    typedef bool bool_vec __attribute__((ext_vector_type(vector_len)));
    using mask_unsigned = uint_of_size<sizeof(bool_vec)>::type;
    using key_vec_ptr = key_vec *;
    key_vec_ptr keys_arr = reinterpret_cast<key_vec_ptr>(keys.data());

    assert(entriesPerBucket % vector_len == 0);
    assert(vector_len <= 64);
    cur++;

    for (size_t i = cur / vector_len; i < entriesPerBucket / vector_len; i++) {
      bool_vec mask =
          (__builtin_convertvector(
               (keys_arr[i] == ((key_vec)std::bit_cast<key_unsigned>(k))),
               bool_vec) |
           ...);
      if constexpr (Inverse)
        mask = ~mask;
      mask_unsigned uns = std::bit_cast<mask_unsigned>(mask);
      uns &= bit_mask_zeros<mask_unsigned>(cur % vector_len);
      if (uns != 0) [[likely]]
        return __builtin_ctzl(uns);
      cur = 0;
    }
    return entriesPerBucket;
  }
#else
  static constexpr size_type entriesPerBucket = 1;
  std::array<K, entriesPerBucket> keys;
#endif

  size_type getNextValid(size_type cur) {
#if defined(__clang__) && SIMD_DENSE_MAP
    return findVec<true>(cur, DenseMapInfo<K>::getEmptyKey(),
                         DenseMapInfo<K>::getTombstoneKey());
#else
    // todo: vectorize manually
    for (size_type i = cur + 1; i < keys.size(); i++) {
      if (!DenseMapInfo<K>::isEqual(keys[i], DenseMapInfo<K>::getEmptyKey()) &&
          !DenseMapInfo<K>::isEqual(keys[i],
                                    DenseMapInfo<K>::getTombstoneKey()))
        return i;
    }
    // invalid
    return entriesPerBucket;
#endif
  }

  size_type find(const K &k, size_type cur = ~0) {
#if defined(__clang__) && SIMD_DENSE_MAP
    return findVec<false>(cur, k);
#else
    // todo: vectorize manually
    for (size_type i = cur + 1; i < keys.size(); i++) {
      if (DenseMapInfo<K>::isEqual(keys[i], k))
        return i;
    }
    return entriesPerBucket;
#endif
  }

  size_type findEmptyOrTombstone(size_type cur = ~0) {
#if defined(__clang__) && SIMD_DENSE_MAP
    return findVec<false>(cur, DenseMapInfo<K>::getEmptyKey(),
                          DenseMapInfo<K>::getTombstoneKey());
#else
    // todo: vectorize manually
    for (size_type i = cur + 1; i < keys.size(); i++) {
      if (DenseMapInfo<K>::isEqual(keys[i], DenseMapInfo<K>::getEmptyKey()) ||
          DenseMapInfo<K>::isEqual(keys[i], DenseMapInfo<K>::getTombstoneKey()))
        return i;
    }
    return entriesPerBucket;
#endif
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
  void deleteArr(Bucket *buckets) {
    ::operator delete[](buckets, std::align_val_t(alignof(Bucket)));
  }

public:
  LargeSetMap() : Base(1, 0) {
    // asan does not support mixing of placement and regular new/delete
    buckets = (Bucket *)::operator new[](sizeof(Bucket) * 1,
                                         std::align_val_t(alignof(Bucket)));
    std::uninitialized_default_construct_n(buckets, 1);
  }

  LargeSetMap(const LargeSetMap &other) : Base(1, 0) {
    buckets = (Bucket *)::operator new[](sizeof(Bucket) * other.cap,
                                         std::align_val_t(alignof(Bucket)));
    std::uninitialized_copy_n(other.buckets, other.cap, buckets);
    this->cap = other.cap;
    this->sz = other.sz;
  }
  LargeSetMap(LargeSetMap &&other) : Base(1, 0) {
    this->buckets = other.buckets;
    this->cap = other.cap;
    this->sz = other.sz;

    other.buckets = nullptr;
    other.sz = 0;
    other.cap = 0;
  }
  LargeSetMap &operator=(const LargeSetMap &other) {
    std::destroy_at(this);
    std::construct_at(this, other);
    return *this;
  }
  LargeSetMap &operator=(LargeSetMap &&other) {
    if (this->buckets)
      std::destroy_at(this);
    std::construct_at(this, std::move(other));
    return *this;
  }

  ~LargeSetMap() {
    this->Base::clearDelete();
    ::operator delete[](buckets, std::align_val_t(alignof(Bucket)));
  }
};

template <typename Base, size_t InlineBuckets> class SmallSetMap : public Base {
  using Bucket = Base::bucket_type;
  Bucket *buckets;
  InlineStorageArr<Bucket, InlineBuckets> arr;

public:
  Bucket *&getBuckets() const { return const_cast<Bucket *&>(buckets); }
  void deleteArr(Bucket *buckets) {
    if (buckets == *arr) {
      ASAN_POISON_MEMORY_REGION(*arr, sizeof(arr));
      return;
    }
    ::operator delete[](buckets, std::align_val_t(alignof(Bucket)));
  }

public:
  bool isSmall() { return buckets == *arr; }

  SmallSetMap() : Base(InlineBuckets, 0), arr() {
    std::uninitialized_default_construct_n(*arr, InlineBuckets);
    buckets = *arr;
  }
  SmallSetMap(const SmallSetMap &other) : Base(InlineBuckets, 0), arr() {
    if (other.cap > InlineBuckets) {
      buckets = (Bucket *)::operator new[](sizeof(Bucket) * other.cap,
                                           std::align_val_t(alignof(Bucket)));
    } else {
      buckets = *arr;
    }
    std::uninitialized_copy_n(other.buckets, other.cap, buckets);
    this->cap = other.cap;
    this->sz = other.sz;
  }
  SmallSetMap(SmallSetMap &&other) : Base(InlineBuckets, 0), arr() {
    if (other.isSmall()) {
      buckets = *arr;
      std::uninitialized_copy_n(other.buckets, other.cap, buckets);
    } else {
      this->buckets = other.buckets;
    }

    this->cap = other.cap;
    this->sz = other.sz;

    other.buckets = nullptr;
    other.sz = 0;
    other.cap = 0;
  }
  SmallSetMap &operator=(const SmallSetMap &other) {
    std::destroy_at(this);
    std::construct_at(this, other);
    return *this;
  }
  SmallSetMap &operator=(SmallSetMap &&other) {
    if (!this->buckets) {
      // moved-from state
      std::construct_at(this);
    }

    if (other.isSmall()) {
      std::uninitialized_copy_n(other.buckets, other.cap, buckets);
    } else {
      deleteArr(buckets);
      this->buckets = other.buckets;
    }

    this->cap = other.cap;
    this->sz = other.sz;

    other.buckets = nullptr;
    other.sz = 0;
    other.cap = 0;
    return *this;
  }

  ~SmallSetMap() {
    if (isSmall())
      return;
    ASAN_UNPOISON_MEMORY_REGION(*arr, sizeof(arr));
    ::operator delete[](buckets, std::align_val_t(alignof(Bucket)));
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
