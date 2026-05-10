#pragma once

#include "dyno/Context.h"
#include "dyno/Pass.h"
#include "hw/HWContext.h"
#include "hw/Module.h"
namespace dyno {

class MyPassEndsWithPass : public Pass<MyPassEndsWithPass> {
  Context &ctx;

  void runOnModule(ModuleIRef mod) {}

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
      mk_tuple(&MyPassEndsWithPass::run, &MyPassEndsWithPass::runModule);

  explicit MyPassEndsWithPass(Context &ctx) : ctx(ctx) {}
  auto make(Context &ctx) { return MyPassEndsWithPass{ctx}; }
};
}; // namespace dyno
