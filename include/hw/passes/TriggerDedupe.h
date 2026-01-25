#pragma once

#include "dyno/Context.h"
#include "dyno/Obj.h"
#include "dyno/Pass.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
namespace dyno {

class TriggerDedupePass : public Pass<TriggerDedupePass> {
  Context &ctx;

  static bool triggerDeepEqual(TriggerIRef lhs, TriggerIRef rhs) {
    if (lhs.getNumOthers() != rhs.getNumOthers())
      return false;
    for (unsigned i = 0; i < lhs.getNumOthers(); i++)
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
      if (trigger.oref()->defUse.getNumUses() == 0) {
        destroyList.emplace_back(trigger);
        continue;
      }

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
    for (auto mod : ctx.getCtx<HWDialectContext>().activeModules()) {
      runOnModule(mod.iref());
    }
  }

public:
  auto make(Context &ctx) { return TriggerDedupePass(ctx); }
  explicit TriggerDedupePass(Context &ctx) : ctx(ctx) {}
};

}; // namespace dyno
