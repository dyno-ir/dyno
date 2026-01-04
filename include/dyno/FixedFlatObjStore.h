#pragma once

#include "dyno/IDImpl.h"
#include "dyno/Obj.h"
#include "support/Bits.h"
#include "support/DynBitSet.h"
#include "support/FlatAddressSpace.h"
#include "support/SmallVec.h"
#include <memory>
#include <optional>
#include <type_traits>
namespace dyno {

template <typename T> class FixedFlatObjStore {
private:
  using Traits = ObjTraits<T>;
  using FatRefT = Traits::FatRefT;
  using CreateHookT = void (*)(FatRefT);
  using DestroyHookT = void (*)(FatRefT);

  std::vector<ObjID> freeIds;
  FlatAddressSpace<T, 1ULL << bit_mask_sz<ObjID::num_t>> space;
  uint32_t sz = 0;

  using BitFieldT = UnsizedSymbSet<std::vector<uint64_t>, 1>;
  struct Empty {};
  std::conditional_t<UninitObj<T>, Empty, BitFieldT> valid;

  void setValid(FatObjRef<T> obj) {
    if constexpr (UninitObj<T>) {
      ; // assume constructed object is in valid state
    } else {
      valid.at(obj.getObjID()) = true;
    }
  }

  void setInvalid(FatObjRef<T> obj) {
    if constexpr (UninitObj<T>) {
      T::setUninitialized(std::launder(obj.getPtr()));
    } else {
      valid.at(obj.getObjID()) = false;
    }
  }

  bool getIsValid(FatObjRef<T> obj) {
    if constexpr (UninitObj<T>) {
      return T::isInitialized(obj.getPtr());
    } else {
      return valid.at(obj.getObjID());
    }
  }

  auto objs() {
    return Range{space}
        .transform([&](size_t i, auto &elem) {
          auto ref = FatRefT{ObjRef<T>{ObjID::num_t(i)}, &elem};
          if (getIsValid(ref))
            return std::make_optional(ref);
          return std::nullopt;
        })
        .drop_optional();
  }

public:
  SmallVec<std::function<void(FatRefT)>, 4> createHooks;
  SmallVec<std::function<void(FatRefT)>, 4> destroyHooks;

  uint32_t numIDs() { return sz; }

  template <typename... Args>
  FatRefT create(Args &&...args)
    requires(!TrailingObj<T>)
  {
    ObjID id;
    if (!freeIds.empty()) {
      id = freeIds.back();
      freeIds.pop_back();
    } else {
      id = (ObjID)sz++;
    }
    T *ptr = &space[id];
    auto ref = ObjRef<T>{id};
    std::construct_at(ptr, ref, std::forward<Args>(args)...);

    FatRefT rv{ref, *ptr};
    setValid(rv);

    for (auto hook : createHooks)
      hook(rv);
    return rv;
  }

  void destroy(FatObjRef<T> ref) {
    for (auto hook : destroyHooks)
      hook(FatRefT{ref});
    std::destroy_at(ref.getPtr());
    setInvalid(ref);
    if (ref.getObjID() + 1 == sz) {
      sz--;
    } else {
      freeIds.push_back(ref.getObjID());
    }
  }

  auto begin() { return objs().begin(); }
  auto end() { return objs().end(); }
  bool exists(ObjRef<T> ref) { return getIsValid(resolve(ref)); }
  FatRefT resolve(ObjRef<T> ref) { return FatRefT{ref, space[ref.getObjID()]}; }
};

}; // namespace dyno
