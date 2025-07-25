#pragma once
#include "dyno/ObjMap.h"

namespace dyno {

template <typename Object> class DestroyMap {
  // could auto switch to hashmap when very sparse
  ObjMapVec<Object, bool> destroyVec;

  using RefT = ObjTraits<Object>::FatRefT;
  using ThinRefT = ObjRef<RefT>;

public:
  void resize(size_t sz) { destroyVec.resize(sz); }
  void mark(RefT ref) { destroyVec[ref] = 1; }
  bool isMarked(RefT ref) const { return destroyVec[ref]; }

  template <typename StoreT>
  void apply(StoreT &store, std::invocable<RefT> auto destroyFunc) {
    for (auto [obj, destroy] : destroyVec) {
      if (!destroy || !store.exists(obj))
        continue;
      destroyFunc(store.resolve(obj));
    }
  }
};

}; // namespace dyno
