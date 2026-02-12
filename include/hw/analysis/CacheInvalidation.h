#pragma once

#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/Wire.h"
namespace dyno {

template <typename Derived> class CacheInvalidation {
  auto &self() { return *static_cast<Derived *>(this); }
  auto &cache() { return static_cast<Derived *>(this)->cache; }

public:
  void clearCache() { cache().clearAll(); }
  void recomputeAt(HWValue root) {
    if (!root.is<WireRef>())
      return;
    SmallVec<WireRef, 32> stack{root.as<WireRef>()};

    while (!stack.empty()) {
      WireRef wire = stack.pop_back_val();
      auto it = cache().raw().find(wire);
      if (it == cache().raw().end())
        continue;
      auto val = std::move(it.val());
      cache().raw().erase(it);

      auto result = self().get(wire);
      if (val == result)
        continue;

      for (auto use : wire.uses()) {
        auto instr = use.instr();
        for (auto def : instr.defs()) {
          if (!def->is<WireRef>())
            continue;
          stack.emplace_back(def->as<WireRef>());
        }
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
