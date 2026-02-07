#pragma once
#include "support/Bits.h"
#include "support/SmallVec.h"
#include <cstddef>
#include <exception>
#include <memory>

template <typename SizeT = uint32_t, SizeT slab_size = 8 * 4096>
class SlabAllocatorBase {
public:
  using size_type = SizeT;

protected:
  const size_type elemsPerSlab;
  const size_type elemSize;
  size_type remElems;
  SmallVec<char *, 4> slabs;

  // maybe put this ool
  void makeSlab() {
    auto *ptr = malloc(slab_size);
    if (!ptr) [[unlikely]]
      std::terminate();
    slabs.emplace_back(reinterpret_cast<char *>(ptr));
    remElems = elemsPerSlab;
  }

public:
  void *operator[](SizeT i) {
    return slabs[i / elemsPerSlab] + elemSize * (i % elemsPerSlab);
  }

  void *allocate() {
    if (remElems == 0) [[unlikely]]
      makeSlab();
    auto rv = slabs.back() + elemSize * (elemsPerSlab - remElems);
    remElems--;
    return rv;
  }

  size_type size() const { return slabs.size() * elemsPerSlab - remElems; }

  SlabAllocatorBase(SizeT elemSize)
      : elemsPerSlab(slab_size / elemSize), elemSize(elemSize), remElems(0) {}

  ~SlabAllocatorBase() {
    for (auto *ptr : slabs)
      free(ptr);
  }

  void clear() {
    for (auto *ptr : slabs)
      free(ptr);
    slabs.clear();
    remElems = 0;
  }
};

template <typename T> class SlabAllocator {
  SlabAllocatorBase<> base;

public:
  using size_type = SlabAllocatorBase<>::size_type;
  template <typename... Args> T *allocate(Args... args) {
    T *ptr = reinterpret_cast<T *>(base.allocate());
    std::construct_at(ptr, std::forward<Args>(args)...);
    return ptr;
  }

  template <typename... Args> T &emplace_back(Args... args) {
    return *allocate(std::forward<Args>(args)...);
  }

  T &operator[](size_type i) { return *reinterpret_cast<T *>(base[i]); }

  size_type size() const { return base.size(); }

  ~SlabAllocator() {
    for (size_type i = 0; i < base.size(); i++) {
      std::destroy_at(reinterpret_cast<T *>(base[i]));
    }
  }

  void clear() {
    for (size_type i = 0; i < base.size(); i++) {
      std::destroy_at(reinterpret_cast<T *>(base[i]));
    }
    base.clear();
  }

  SlabAllocator() : base(sizeof(T)) {}
};

template <typename SizeT = uint32_t, SizeT slab_size = 8 * 4096>
class MixedSizeSlabAllocator : private SlabAllocatorBase<SizeT, slab_size> {
public:
  using Base = SlabAllocatorBase<SizeT, slab_size>;
  using size_type = SizeT;

protected:
  using Base::elemSize;
  using Base::elemsPerSlab;
  using Base::makeSlab;
  using Base::remElems;
  using Base::slabs;

  // allocated sizes are a multiple of elemSizeMult. May be one, just to improve
  // address space for uint32_t indices.
public:
  MixedSizeSlabAllocator(SizeT elemSizeMult = alignof(std::max_align_t))
      : Base(elemSizeMult) {}

  void *allocate() = delete;

  // allocate a number of words. returns idx, resolvable to ptr via resolve
  size_type allocate(size_type sz) {
    assert(sz && "empty alloc?");
    if (remElems < sz) [[unlikely]]
      makeSlab();
    auto rv = size();
    // auto ptr = slabs.back() + elemSize * (elemsPerSlab - remElems);
    remElems -= sz;
    return rv;
  }

  // allocate a number of bytes. returns idx, resolvable to ptr via resolve
  size_type allocate_nbytes(size_type nbytes) {
    return allocate(round_up_div(nbytes, elemSize));
  }

  void *resolve(size_type idx) { return Base::operator[](idx); }

  size_type size() { return this->Base::size(); }

  using Base::clear;
};

// slab allocator for mixed size objects. no longer supports fast indexing
// anyways, so are slabs linked-list based.
// template <typename size_type = uint32_t, size_type slab_size = 8 * 4096>
// class MixedSizeSlabAllocator {
//   constexpr static size_type payload_size = slab_size - sizeof(uintptr_t);
//   struct Slab {
//     uint8_t payload[payload_size];
//     Slab *prev;
//   };
//   Slab *cur;
//   size_type pos;

//   void makeSlab() {
//     auto *ptr = malloc(slab_size);
//     if (!ptr) [[unlikely]]
//       std::terminate();
//     pos = 0;
//     cur = ptr;
//   }

//   void allocate(size_type size, size_type align) {
//     assert(align <= slab_size && "can't align larger than slab size");
//     assert(size <= payload_size && "can't alloc larger than payload size");
//     assert(std::popcount(align) == 1 && "non pow2 align");

//     // align
//     if (pos & (align - 1)) {
//       pos &= ~(align - 1);
//       pos += align;
//     }

//     if (pos + size > payload_size)
//       makeSlab();
//   }

//   MixedSizeSlabAllocator() { makeSlab(); }
// };
