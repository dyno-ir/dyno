#pragma once

#include "hw/HWContext.h"
#include "hw/Module.h"

#include "dyno/Context.h"
#include "dyno/Pass.h"
#include "support/TwoLevelSet.h"

namespace dyno {

// Link modules by name, like ResolveImports for symbols.
// todo: find a way to combine both (requires generic store iteration though),
// or just emit symbols in scenarios where we need to re-link.
class ResolveModulesPass : public Pass<ResolveModulesPass> {
  Context &ctx;

public:
#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM) FIELD(bool, onlyLinkToInactive, true)
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
  Config config;

  void run() {
    TwoLevelMap<SSOStringRef, ObjRef<Module>> strings;
    for (auto module : ctx.getStore<Module>()) {
      if (module->name.empty())
        continue;
      assert(module->defUse.getNumDefs() <= 1);
      if (module->defUse.getNumDefs() == 1) {
        if (config.onlyLinkToInactive && !module->ignore)
          continue;
        auto it = strings.insertOrAssign(module->name, module);
        if (it.val() != module)
          it.val() = nullref;
      }
    }

    for (auto module : ctx.getStore<Module>()) {
      if (module->name.empty())
        continue;
      if (module->defUse.getNumDefs() == 0) {
        auto it = strings.find(module->name);
        if (it == strings.end())
          continue;
        module.replaceAllUsesWith(ctx.resolve(it.val()));
        ctx.destroy(module);
      }
    }
  }

  static constexpr auto runFuncs = mk_tuple(&ResolveModulesPass::run);

  explicit ResolveModulesPass(Context &ctx) : ctx(ctx) {}
  auto make(Context &ctx) { return ResolveModulesPass{ctx}; }
};
}; // namespace dyno
