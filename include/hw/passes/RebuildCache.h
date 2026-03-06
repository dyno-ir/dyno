#pragma once

#include "dyno/Pass.h"
#include "hw/HWContext.h"
#include "hw/Module.h"
namespace dyno {
// Populate inline caches, for use after parsing.
class PopulateInlineCachesPass : public Pass<PopulateInlineCachesPass> {
  Context &ctx;

public:
  void runModule(ModuleIRef mod) { mod.rebuildCache(); }

  void run() {
    // add others if rebuilding cache ever required
    for (auto mod : ctx.getStore<Module>()) {
      runModule(mod.iref());
    }
  }

  static constexpr auto runFuncs = std::make_tuple(
      &PopulateInlineCachesPass::run, &PopulateInlineCachesPass::runModule);

  explicit PopulateInlineCachesPass(Context &ctx) : ctx(ctx) {}
  auto make(Context &ctx) { return PopulateInlineCachesPass{ctx}; }
};
}; // namespace dyno
