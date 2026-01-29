#pragma once

#include "dyno/Context.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Pass.h"
#include "hw/HWPrinter.h"
#include <fstream>
namespace dyno {
class DumpPass : public Pass<DumpPass> {
  Context &ctx;

#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM)                                      \
  FIELD(std::string, path, "/dev/stdout")
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
  Config config;

public:
  void run() {
    std::ofstream str{config.path};
    // todo: generic printer
    HWPrinter print{str};
    print.printCtx(ctx);
  }
  explicit DumpPass(Context &ctx) : ctx(ctx) {}
  static DumpPass make(Context &ctx) { return DumpPass(ctx); }
};
}; // namespace dyno
