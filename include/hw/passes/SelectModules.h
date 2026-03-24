#pragma once

#include "dyno/Context.h"
#include "dyno/Pass.h"
#include "hw/HWContext.h"
#include "hw/Module.h"
#include <regex>
namespace dyno {

class SelectModulesPass : public Pass<SelectModulesPass> {
  Context &ctx;

public:
#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM)                                      \
  FIELD(bool, unselect, false)                                                 \
  FIELD(std::string, regex, ".*")
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
  Config config;

private:
  void runOnModule(ModuleIRef mod) {
    std::regex regex(config.regex);
    mod.mod()->ignore =
        !(std::regex_match(mod.mod()->name, regex) ^ config.unselect);
    std::print(dbgs(), "mod {} selected {} (rgx {})\n", mod.mod()->name,
               !mod.mod()->ignore, config.regex);
  }

public:
  void runWrapper(auto &&runFunc) { runFunc(); }

  void runModule(ModuleIRef mod) {
    runWrapper([&] { runOnModule(mod); });
  }

  void run() {
    runWrapper([&] {
      for (auto mod : ctx.getStore<Module>())
        runOnModule(mod.iref());
    });
  }

  static constexpr auto runFuncs =
      mk_tuple(&SelectModulesPass::run, &SelectModulesPass::runModule);

  explicit SelectModulesPass(Context &ctx) : ctx(ctx) {}
  auto make(Context &ctx) { return SelectModulesPass{ctx}; }
};
}; // namespace dyno
