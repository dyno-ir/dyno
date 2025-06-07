#pragma once

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
  std::vector<ObjID> freeIds;
  ObjMapVec<T, T *> map;

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
          return ptr ? std::optional{typename Traits::FatRefT{ObjRef<T>(ObjID(i)), *ptr}}
                     : std::nullopt;
        })
        .discard_optional();
  }

public:
  using value_type = T;
  NewDeleteObjStore() {}
  NewDeleteObjStore(const NewDeleteObjStore &) = delete;
  NewDeleteObjStore(NewDeleteObjStore &&) = delete;
  NewDeleteObjStore &operator=(const NewDeleteObjStore &) = delete;
  NewDeleteObjStore &operator=(NewDeleteObjStore &&) = delete;
  ~NewDeleteObjStore() {
    for (auto *ptr : map.elements) {
      if (!ptr)
        continue;
      ptr->~T();
      free(ptr);
    }
  }

  template <typename... Args>
  Traits::FatRefT create(Args &&...args)
    requires(!TrailingObj<T>)
  {
    auto ref = createRef();
    void *alloc = malloc(sizeof(T));
    T *ptr = new (alloc) T(ref, std::forward<Args>(args)...);
    map[ref] = ptr;
    return {ref, *ptr};
  }

  template <typename... Args>
  Traits::FatRefT create(size_t sz, Args &&...args)
    requires TrailingObj<T>
  {
    auto ref = createRef();
    void *alloc = malloc(T::getAllocSize(sz));
    T *ptr = new (alloc) T(ref, sz, std::forward<Args>(args)...);
    map[ref] = ptr;
    return {ref, *ptr};
  }

  T &operator[](ObjRef<T> ref) { return *map[ref]; }

  void destroy(FatObjRef<T> ref) {
    assert(map[ref] && "Invalid ref");
    freeIds.push_back(ref.getObjID());
    map[ref] = nullptr;
    ref.getPtr()->~T();
    free(ref.getPtr());
  }

  void destroyIfExists(FatObjRef<T> ref) {
    if (!map[ref])
      return;
    freeIds.push_back(ref.getObjID());
    map[ref] = nullptr;
    ref.getPtr()->~T();
    free(ref.getPtr());
  }

  Traits::FatRefT resolve(ObjRef<T> ref) {
    return typename Traits::FatRefT{ref, map[ref]};
  }

  auto begin() { return objs().begin(); }
  auto end() { return objs().end(); }

  ObjID::num_t numIDs() { return map.size(); }
};

} // namespace dyno
