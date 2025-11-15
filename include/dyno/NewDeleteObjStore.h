#pragma once

#include "support/SmallVec.h"
#include <cassert>
#include <dyno/Obj.h>
#include <dyno/ObjMap.h>
#include <optional>
#include <support/Ranges.h>
#include <utility>
#include <vector>

namespace dyno {
// todo: mechanism to downsize map
// todo: hooks (e.g delete -> remove from CSE structural) first delete only,
// create via builder
template <typename T> class NewDeleteObjStore {
private:
  using Traits = ObjTraits<T>;
  using FatRefT = Traits::FatRefT;
  using CreateHookT = void (*)(FatRefT);
  using DestroyHookT = void (*)(FatRefT);

  std::vector<ObjID> freeIds;
  ObjMapVec<T, T *> map;

public:
  SmallVec<std::function<void(FatRefT)>, 4> createHooks;
  SmallVec<std::function<void(FatRefT)>, 4> destroyHooks;

private:
  ObjRef<T> createRef() {
    if (!freeIds.empty()) {
      ObjID id = freeIds.back();
      freeIds.pop_back();
      return ObjRef<T>{id};
    }
    ObjID id(map.size());
    map.elements.emplace_back();
    return ObjRef<T>{id};
  }

  auto objs() {
    return Range{map.elements}
        .transform([](auto i, auto *ptr) {
          return ptr ? std::optional{FatRefT{ObjRef<T>(ObjID(i)), *ptr}}
                     : std::nullopt;
        })
        .discard_optional();
  }

public:
  using value_type = T;
  NewDeleteObjStore() {}
  NewDeleteObjStore(const NewDeleteObjStore &) = delete;
  NewDeleteObjStore(NewDeleteObjStore &&) = default;
  NewDeleteObjStore &operator=(const NewDeleteObjStore &) = delete;
  NewDeleteObjStore &operator=(NewDeleteObjStore &&) = default;
  ~NewDeleteObjStore() {
    for (auto *ptr : map.elements) {
      if (!ptr)
        continue;
      ptr->~T();
      free(ptr);
    }
  }

  template <typename... Args>
  FatRefT create(Args &&...args)
    requires(!TrailingObj<T>)
  {
    auto ref = createRef();
    void *alloc = malloc(sizeof(T));
    T *ptr = new (alloc) T(ref, std::forward<Args>(args)...);
    map[ref] = ptr;
    FatRefT rv{ref, *ptr};
    for (auto hook : createHooks)
      hook(rv);
    return rv;
  }

  template <typename... Args>
  FatRefT create(size_t sz, Args &&...args)
    requires TrailingObj<T>
  {
    auto ref = createRef();
    void *alloc = malloc(T::getAllocSize(sz));
    T *ptr = new (alloc) T(ref, sz, std::forward<Args>(args)...);
    map[ref] = ptr;
    FatRefT rv{ref, *ptr};
    for (auto hook : createHooks)
      hook(rv);
    return rv;
  }

  T &operator[](ObjRef<T> ref) { return *map[ref]; }

  void destroy(FatObjRef<T> ref) {
    assert(map[ref] && "Invalid ref");
    for (auto hook : destroyHooks)
      hook(FatRefT{ref});
    freeIds.push_back(ref.getObjID());
    map[ref] = nullptr;
    ref.getPtr()->~T();
    free(ref.getPtr());
  }

  void destroyIfExists(FatObjRef<T> ref) {
    if (!map[ref])
      return;
    destroy(ref);
  }

  bool exists(ObjRef<T> ref) { return !!map[ref]; }

  FatRefT resolve(ObjRef<T> ref) { return FatRefT{ref, map[ref]}; }

  auto begin() { return objs().begin(); }
  auto end() { return objs().end(); }

  ObjID::num_t numIDs() { return map.size(); }
};

} // namespace dyno
