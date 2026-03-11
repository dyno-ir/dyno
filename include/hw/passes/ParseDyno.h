#pragma once
#include "dyno/Context.h"
#include "dyno/DialectInfo.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Pass.h"
#include "hw/HWContext.h"
#include "hw/HWParser.h"
#include "hw/HWPrinter.h"
#include "support/ErrorRecovery.h"
#include "support/MMap.h"
#include <ostream>

namespace dyno {

class ParseDynoPass : public Pass<ParseDynoPass> {
  Context &ctx;

public:
#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM)                                      \
  FIELD(std::string, fileName, "")                                             \
  FIELD(bool, newModulesActive, false)
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
  Config config;

  auto make(Context &ctx) { return ParseDynoPass(ctx); }
  explicit ParseDynoPass(Context &ctx) : ctx(ctx) {}

  void run() {
    MMap mmap{config.fileName};
    if (mmap.size() == 0)
      report_fatal_error(("could not open file: " + config.fileName).c_str());
    HWParser parser{ctx};
    ctx.getStore<Module>().createHooks.emplace_back(
        [&](ModuleRef ref) { ref->ignore = !config.newModulesActive; });
    parser.parse(mmap, config.fileName);
  }

  constexpr static auto runFuncs = mk_tuple(&ParseDynoPass::run);
};
}; // namespace dyno
