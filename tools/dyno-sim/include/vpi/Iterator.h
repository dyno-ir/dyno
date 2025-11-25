#pragma once
#include "dyno/Obj.h"
#include "vpi/IDs.h"

namespace dyno {

class Iterator {
public:
  std::function<DynObjRef()> next;

  Iterator(DynObjRef, std::function<DynObjRef()> &&func)
      : next(std::move(func)) {}
};

template <> struct dyno::ObjTraits<Iterator> {
  static constexpr DialectType ty{VPI_ITERATOR};
  using FatRefT = FatObjRef<Iterator>;
};

}; // namespace dyno