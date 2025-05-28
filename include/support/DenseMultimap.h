#pragma once
#include "support/DenseMap.h"

template <typename Bucket, typename K, typename V,
          typename size_type = uint32_t>
struct DenseMultimapIterator
    : public DenseMapIterator<Bucket, K, V, size_type> {
  using Base = DenseMapIterator<Bucket, K, V>;
  using Base::Base;

  size_type probeOffset = 0;
  DenseMultimapIterator(Bucket *bucket, size_type rem, size_type idx,
                        size_type offs)
      : Base(bucket, rem, idx, offs), probeOffset(offs) {}

  DenseMultimapIterator(Base base) : Base(base) {}
};

template <typename Derived, typename K, typename V, typename Bucket>
class DenseMultimapBase
    : public DenseMapBase<Derived, K, V, Bucket,
                          DenseMultimapIterator<Bucket, K, V>> {
  using Base =
      DenseMapBase<Derived, K, V, Bucket, DenseMultimapIterator<Bucket, K, V>>;
  using iterator = Base::iterator;
  using size_type = Base::size_type;

protected:
  using Base::cap;
  using Base::getBuckets;
  using Base::self;
  using Base::sz;

  bool growIfOversized() {
    return this->DenseSetMapBase<Derived, K, Bucket,
                                 DenseMultimapIterator<Bucket, K, V>>::
        growIfOversized([&](Bucket *bucket, size_type j) {
          insert(std::move(bucket->keys[j]), std::move(bucket->values[j]));
          std::destroy_at(&bucket->values[j]);
        });
  }

  auto findNextImpl(iterator prev) const {
    auto k = prev.key();
    assert(!DenseMapInfo<K>::isEqual(k, DenseMapInfo<K>::getEmptyKey()) &&
           !DenseMapInfo<K>::isEqual(k, DenseMapInfo<K>::getTombstoneKey()));

    size_type bucketIndex = prev.bucket - getBuckets();
    size_type offset = prev.probeOffset;
    size_type start = prev.idx;

    assert(offset != 0 && "cannot find next on this iterator");

    while (true) {
      if (auto entryIndex = getBuckets()[bucketIndex].find(k, start);
          entryIndex != Bucket::entriesPerBucket) {
        return std::make_pair(true,
                              iterator{&getBuckets()[bucketIndex],
                                       cap - bucketIndex, entryIndex, offset});
      }
      if (auto emptyIndex = getBuckets()[bucketIndex].find(
              DenseMapInfo<K>::getEmptyKey(), start);
          emptyIndex != Bucket::entriesPerBucket) {
        return std::make_pair(false,
                              iterator{&getBuckets()[bucketIndex],
                                       cap - bucketIndex, emptyIndex, offset});
      }

      bucketIndex = (bucketIndex + offset) & (cap - 1);
      offset += 1;
      start = ~0;
    }
    dyno_unreachable("full hash map");
  }

public:
  using Base::begin;
  using Base::contains;
  using Base::end;
  using Base::find;

  iterator insertOrAssign(const K &k, V &&v) = delete;
  iterator insertOrAssign(const K &k, const V &v) = delete;
  template <std::invocable T> auto findOrInsert(const K &k, T &&func) = delete;
  auto findOrInsert(const K &k, const V &&newVal) = delete;
  auto findOrInsert(const K &k, const V &newVal) = delete;
  V &operator[](const K &k) = delete;

  iterator insert(const K &k, V &&v) {
    growIfOversized();
    auto iter = Base::findEmptyImpl(k, false);
    sz++;
    iter.keyMut() = k;
    (*iter).second = std::move(v);
    return iter;
  }
  iterator insert(const K &k, const V &v) { return insert(k, V{v}); }

  iterator find_next(iterator prev) {
    auto [found, it] = findNextImpl(prev);
    if (!found)
      return end();
    return it;
  }

  using Base::clear;

protected:
  using Base::clearDelete;

  DenseMultimapBase(size_type cap, size_type sz) : Base(cap, sz) {}
};

template <typename K, typename V>
class DenseMultimap
    : public LargeSetMap<
          DenseMultimapBase<DenseMultimap<K, V>, K, V, DenseMapBucket<K, V>>> {
  using Base = LargeSetMap<
      DenseMultimapBase<DenseMultimap<K, V>, K, V, DenseMapBucket<K, V>>>;
  using Base::Base;
};
