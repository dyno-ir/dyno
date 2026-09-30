#pragma once

#include "dyno/CFG.h"
#include "dyno/Context.h"
#include "dyno/DeepCopy.h"
#include "dyno/Pass.h"
#include "hw/HWContext.h"
#include "hw/Module.h"
namespace dyno {

class InactiveCopyPass : public Pass<InactiveCopyPass> {
  Context &ctx;

  void runOnModule(ModuleIRef mod) {
    DeepCopier copier{ctx};
    auto copy = copier.copyInstr(mod, BlockRef_iterator<true>::invalid());
    copy.as<ModuleIRef>().mod()->ignore = true;
    copy.as<ModuleIRef>().mod()->name += "__inactive";

    // this should really be done in register copy method. maybe expose it
    // as a fully free method for SoA type objects.
    auto &regNameInfo = ctx.getCtx<HWDialectContext>().regNameInfo;
    auto &regInitValue = ctx.getCtx<HWDialectContext>().regResetValue;
    auto &regTypeInfo = ctx.getCtx<HWDialectContext>().regTypeInfo;
    for (auto [oldRef, newRef] : copier.oldToNewMap) {
      if (auto oldReg = oldRef.dyn_as<ObjRef<Register>>()) {
        regNameInfo.copyNames(oldReg, newRef.as<ObjRef<Register>>());
        if (auto val = regInitValue.find(oldReg))
          regInitValue.get_ensure(newRef.as<ObjRef<Register>>()) = *val;
        regTypeInfo.copyType(oldReg, newRef.as<ObjRef<Register>>());
      }
    }
  }

public:
  void runWrapper(auto &&runFunc) { runFunc(); }

  void runModule(ModuleIRef mod) {
    runWrapper([&] { runOnModule(mod); });
  }

  void run() {
    runWrapper([&] {
      for (auto mod : ctx.getCtx<HWDialectContext>().activeModules())
        runOnModule(mod.iref());
    });
  }

  static constexpr auto runFuncs =
      mk_tuple(&InactiveCopyPass::run, &InactiveCopyPass::runModule);

  explicit InactiveCopyPass(Context &ctx) : ctx(ctx) {}
  auto make(Context &ctx) { return InactiveCopyPass{ctx}; }
};
}; // namespace dyno
