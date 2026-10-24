#pragma once
#include "dyno/Pass.h"
#include "hw/HWContext.h"
#include "hw/Module.h"
#include "op/Function.h"
#include "op/OpContext.h"
namespace dyno {
// Populate inline caches, for use after parsing.
class PopulateInlineCachesPass : public Pass<PopulateInlineCachesPass> {
  Context &ctx;

public:
  void runModule(ModuleIRef mod) { mod.rebuildCache(); }
  void runFunction(FunctionIRef func) { func.rebuildCache(); }

  void run() {
    // add others if rebuilding cache ever required
    for (auto mod : ctx.getStore<Module>()) {
      runModule(mod.iref());
    }
    for (auto func : ctx.getStore<Function>()) {
      runFunction(func.iref());
    }
  }

  static constexpr auto runFuncs = mk_tuple(
      &PopulateInlineCachesPass::run, &PopulateInlineCachesPass::runModule);

  explicit PopulateInlineCachesPass(Context &ctx) : ctx(ctx) {}
  auto make(Context &ctx) { return PopulateInlineCachesPass{ctx}; }
};
}; // namespace dyno
