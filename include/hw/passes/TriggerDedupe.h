#pragma once

#include "dyno/Obj.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
namespace dyno {

class TriggerDedupePass {
  HWContext &ctx;

  static bool triggerDeepEqual(TriggerIRef lhs, TriggerIRef rhs) {
    if (lhs.getNumOthers() != rhs.getNumOthers())
      return false;
    for (uint i = 0; i < lhs.getNumOthers(); i++)
      if (lhs.other(i)->thin() != rhs.other(i)->thin())
        return false;
    if (lhs.oref()->modesRaw() != rhs.oref()->modesRaw())
      return false;
    return true;
  }

  // not many different triggers so linear search
  SmallVec<TriggerIRef, 4> canonical;

  void runOnModule(ModuleIRef mod) {
    SmallVec<TriggerIRef, 128> destroyList;
    for (auto trigger : mod.triggers()) {
      auto it = std::find_if(
          canonical.begin(), canonical.end(),
          [&](TriggerIRef rhs) { return triggerDeepEqual(trigger, rhs); });
      if (it == canonical.end()) {
        canonical.emplace_back(trigger);
        continue;
      }

      trigger.oref()->defUse.replaceAllUsesWith(it->oref());
      destroyList.emplace_back(trigger);
    }

    HWInstrBuilder build{ctx};
    for (auto instr : destroyList)
      build.destroyInstr(instr);
  }

public:
  void run() {
    for (auto mod : ctx.getModules()) {
      runOnModule(mod.iref());
    }
  }

public:
  explicit TriggerDedupePass(HWContext &ctx) : ctx(ctx) {}
};

}; // namespace dyno
