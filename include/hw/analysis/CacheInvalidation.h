#pragma once

#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/Pointer.h"
#include "hw/Wire.h"
namespace dyno {

template <typename Derived> class CacheInvalidation {
  auto &self() { return *static_cast<Derived *>(this); }
  auto &cache() { return static_cast<Derived *>(this)->cache; }

public:
  void clearCache() { cache().clearAll(); }
  void recomputeAt(FatDynObjRef<> root) {
    SmallVec<FatDynObjRef<>, 32> stack{root};

    auto pushUsesOfDef = [&](FatDynObjRef<InstrDefUse> obj) {
      for (auto use : obj->uses())
        for (auto def : use.instr().defs())
          // we only care about wire and pointer dataflow in HW dialect
          if (def->fat().getType() == Any{HW_WIRE, HW_POINTER})
            stack.emplace_back(def->fat());
    };

    while (!stack.empty()) {
      FatDynObjRef<> ref = stack.pop_back_val();
      if (auto wire = ref.dyn_as<WireRef>()) {
        auto it = cache().raw().find(wire);
        if (it == cache().raw().end())
          continue;
        auto val = std::move(it.val());
        cache().raw().erase(it);

        auto result = self().get(wire);
        if (val == result)
          continue;

        pushUsesOfDef(wire);
      } else if (auto ptr = ref.dyn_as<PointerRef>()) {
        // pointer is never cached itself but uses of it may be
        pushUsesOfDef(ptr);
      }
    }
  }
  void recomputeUses(WireRef wire) {
    for (auto use : wire.as<WireRef>().uses())
      recomputeAt(use->as<WireRef>());
  }

  void replaceAt(WireRef oldRef, HWValue newRef) {
    auto it = cache().raw().find(oldRef);
    if (it == cache().raw().end())
      return;
    cache().raw().erase(it);
  }
};

}; // namespace dyno
