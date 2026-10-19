#pragma once

#include "dyno/Obj.h"
#include "support/SmallVec.h"
#include <cassert>

namespace dyno {

template <typename T> class IDObjStore {
  using FatRefT = ObjTraits<T>::FatRefT;

  SmallVec<ObjID, 32> freeIds;
  ObjID::num_t nextId = 0;

public:
  using value_type = T;
  static constexpr DialectType ty{ObjTraits<T>::ty};

  FatRefT create() {
    if (!freeIds.empty()) [[unlikely]]
      return resolve(ObjRef<T>(freeIds.pop_back_val()));
    return resolve(ObjRef<T>(ObjID(nextId++)));
  }

  FatRefT create(const FatObjRef<T> &) { return create(); }
  FatRefT create(FatObjRef<T> &&) { return create(); }

  FatRefT resolve(ObjRef<T> ref) {
    return FatRefT{ref, static_cast<T *>(nullptr)};
  }

  void destroy(FatRefT ref) {
    assert(ref.getObjID() < nextId);
    freeIds.push_back(ref.getObjID());
  }

  ObjID::num_t getNumIDs() const { return nextId; }

  void reset() {
    freeIds.clear();
    nextId = 0;
  }
};
} // namespace dyno
