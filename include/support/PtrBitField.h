#pragma once

#include "support/Bits.h"

template <typename T, unsigned N> class PtrBitField {
  static_assert(clog2(alignof(T)) >= N);
public:
  using Field = BitField<uintptr_t, N, 0>;

  uintptr_t num;

  PtrBitField(nullptr_t) : num(0) {}
  PtrBitField(T *ptr, unsigned val) { set(ptr, val); }
  explicit PtrBitField(uintptr_t val) : num(val) {}

  Field field() { return Field(num); }

  void set(T *ptr, unsigned val) {
    uintptr_t ptrVal = reinterpret_cast<uintptr_t>(ptr);
    num = ptrVal | Field::getSet(0, val);
  }

  void setPtr(T *ptr) { set(ptr, field()); }
  T *getPtr() { return reinterpret_cast<T *>(field().getNumClr()); }

  T &operator*() { return *getPtr(); }
  T *operator->() { return getPtr(); }
  PtrBitField &operator=(T *ptr) { setPtr(ptr); }

  explicit operator bool() { return getPtr(); }
};
