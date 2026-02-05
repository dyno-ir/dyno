#pragma once

#include "dyno/Context.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Obj.h"
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
  void runInstr(InstrRef instr) {
    std::ofstream str{config.path};
    // todo: generic printer
    HWPrinter print{str};
    print.printInstr(instr, ctx);
  }
  void runObject(FatDynObjRef<> obj) {
    std::ofstream str{config.path};
    // todo: generic printer
    HWPrinter print{str};
    print.printDef(obj);
    str << "\n";
  }

  static constexpr auto runFuncs = std::make_tuple(
      &DumpPass::runInstr, &DumpPass::runObject, &DumpPass::run);

  explicit DumpPass(Context &ctx) : ctx(ctx) {}
  static DumpPass make(Context &ctx) { return DumpPass(ctx); }
};
}; // namespace dyno
