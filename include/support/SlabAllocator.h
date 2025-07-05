#pragma once
#include "support/SmallVec.h"
#include <cstddef>
#include <memory>

template <typename SizeT = uint32_t, SizeT slab_size = 8 * 4096>
class SlabAllocatorBase {
public:
  using size_type = SizeT;

private:
  const size_type elemsPerSlab;
  const size_type elemSize;
  size_type numElems;
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

  size_type size() const { return numElems; }

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
    for (size_t i = 0; i < base.size(); i++) {
      std::destroy_at(reinterpret_cast<T *>(base[i]));
    }
  }

  SlabAllocator() : base(sizeof(T)) {}
};
