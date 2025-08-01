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
  void ensureUnmarked(RefT ref) { destroyVec.get_ensure(ref) = 0; }

  template <typename StoreT>
  void apply(StoreT &store, std::invocable<RefT> auto destroyFunc) {
    for (auto [obj, destroy] : destroyVec) {
      if (!destroy || !store.exists(obj))
        continue;
      destroyFunc(store.resolve(obj));
    }
  }
  void clear() { destroyVec.clear(); }

  auto getCreateHook() {
    return [&](RefT ref) { this->ensureUnmarked(ref); };
  }

  template <typename StoreT>
  [[nodiscard]] auto registerCreateHook(StoreT &store) {
    store.createHooks.emplace_back(getCreateHook());
    return PopHookToken<StoreT>{store};
  }

  template <typename StoreT> struct PopHookToken {
  private:
    StoreT &store;

  public:
    ~PopHookToken() { store.createHooks.pop_back(); }
    PopHookToken(StoreT &store) : store(store) {}
  };
};

}; // namespace dyno
