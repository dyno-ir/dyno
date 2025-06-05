#pragma once
#include "support/SmallVec.h"
#include <cstddef>
#include <memory>

template <typename size_type = uint32_t, size_type slab_size = 8 * 4096>
class SlabAllocatorBase {
  const size_type elemsPerSlab;
  const size_type elemSize;
  size_type numElems;
  size_type remElems;
  SmallVec<char *, 4> slabs;

  void makeSlab() {
    auto *ptr = malloc(slab_size);
    if (!ptr) [[unlikely]]
      std::terminate();
    slabs.emplace_back(reinterpret_cast<char *>(ptr));
    remElems = elemsPerSlab;
  }

public:
  void *operator[](size_t i) {
    return slabs[i / elemsPerSlab] + elemSize * (i % elemsPerSlab);
  }

  void *allocate() {
    if (remElems == 0) [[unlikely]]
      makeSlab();
    auto rv = slabs.back() + elemSize * (elemsPerSlab - remElems);
    remElems--;
    numElems++;
    return rv;
  }

  size_type size() { return numElems; }

  SlabAllocatorBase(size_t elemSize)
      : elemsPerSlab(slab_size / elemSize), elemSize(elemSize), numElems(0),
        remElems(0) {}

  ~SlabAllocatorBase() {
    for (auto *ptr : slabs)
      free(ptr);
  }
};

template <typename T> class SlabAllocator {
  SlabAllocatorBase<> base;

public:
  template <typename... Args> T *allocate(Args... args) {
    T *ptr = reinterpret_cast<T *>(base.allocate());
    std::construct_at(ptr, args...);
    return ptr;
  }

  ~SlabAllocator() {
    for (size_t i = 0; i < base.size(); i++) {
      std::destroy_at(reinterpret_cast<T *>(base[i]));
    }
  }

  SlabAllocator() : base(sizeof(T)) {}
};
