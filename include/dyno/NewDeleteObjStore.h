#pragma once

#include "dyno/Type.h"
#include "support/SmallVec.h"
#include <cassert>
#include <dyno/Obj.h>
#include <dyno/ObjMap.h>
#include <memory>
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
  static constexpr DialectType ty{Traits::ty};

  NewDeleteObjStore() {}
  NewDeleteObjStore(const NewDeleteObjStore &) = delete;
  NewDeleteObjStore(NewDeleteObjStore &&) = default;
  NewDeleteObjStore &operator=(const NewDeleteObjStore &) = delete;
  NewDeleteObjStore &operator=(NewDeleteObjStore &&) = default;

  void destroyAll() {
    for (auto *ptr : map.elements) {
      if (!ptr)
        continue;
      ptr->~T();
      free(ptr);
    }
  }
  ~NewDeleteObjStore() { destroyAll(); }

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
    auto rv = _create_no_hook(sz, std::forward<Args>(args)...);
    _call_create_hooks(rv);
    return rv;
  }

  // For resizable allocs. Caller must call _call_create_hooks once resizing
  // completed.
  template <typename... Args>
  FatRefT _create_no_hook(size_t sz, Args &&...args)
    requires TrailingObj<T>
  {
    auto ref = createRef();
    void *alloc = malloc(T::getAllocSize(sz));
    T *ptr = new (alloc) T(ref, sz, std::forward<Args>(args)...);
    map[ref] = ptr;
    FatRefT rv{ref, *ptr};
    return rv;
  }

  void _call_create_hooks(FatRefT ref) {
    for (auto hook : createHooks)
      hook(ref);
  }

  template <typename TrailingType>
  FatRefT realloc(FatRefT old, size_t sz)
    requires TrailingObj<T>
  {
    T *ptr;

    void *alloc = malloc(T::getAllocSize(sz));
    ptr =
        std::construct_at<T>(reinterpret_cast<T *>(alloc), std::move(*old), sz);
    auto ntrailing = (old->getAllocSize() - sizeof(T)) / sizeof(TrailingType);
    std::uninitialized_move_n(reinterpret_cast<TrailingType *>(&*old + 1),
                              std::min(ntrailing, sz),
                              reinterpret_cast<TrailingType *>(ptr + 1));

    map[old] = ptr;
    FatRefT rv{old.thin(), ptr};
    return rv;
  }

  T &operator[](ObjRef<T> ref) { return *map[ref]; }

  void _destroy_no_hook(FatRefT ref) {
    assert(map[ref] && "Invalid ref");
    freeIds.push_back(ref.getObjID());
    map[ref] = nullptr;
    ref.getPtr()->~T();
    free(ref.getPtr());
  }

  void _call_destroy_hooks(FatRefT ref) {
    for (auto hook : destroyHooks)
      hook(ref);
  }

  void destroy(FatRefT ref) {
    _call_destroy_hooks(ref);
    _destroy_no_hook(ref);
  }

  void destroyIfExists(FatRefT ref) {
    if (!map[ref])
      return;
    destroy(ref);
  }

  bool exists(ObjRef<T> ref) { return !!map[ref]; }

  FatRefT resolve(ObjRef<T> ref) { return FatRefT{ref, map[ref]}; }
  FatDynObjRef<> resolveGeneric(DynObjRef ref) {
    // Only used for Context resolver.
    // if need we can also autogenerate this method from
    // BindMethod<...::resolve>
    return resolve(ref.as<ObjRef<T>>());
  }

  auto begin() { return objs().begin(); }
  auto end() { return objs().end(); }

  ObjID::num_t numIDs() { return map.size(); }

  void reset() {
    destroyAll();
    freeIds.clear();
    map.clear();
    createHooks.clear();
    destroyHooks.clear();
  }
};

} // namespace dyno
