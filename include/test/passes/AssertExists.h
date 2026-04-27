#pragma once

#include <regex>

#include "dyno/Context.h"
#include "dyno/IDImpl.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Pass.h"
#include "op/PrintParse.h"
#include "support/Utility.h"
#include "test/IDs.h"

namespace dyno {

template <typename PrinterT =
              ContextPrinterWrapper<CoreDialectPrinter, OpDialectPrinter>>
class AssertExistsPass : public Pass<AssertExistsPass<PrinterT>> {
  Context &ctx;

public:
  static constexpr DialectID dialect{DIALECT_TEST};
#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM)                                      \
  FIELD(const char *, regex, "")                                               \
  FIELD(uint32_t, num, 1)                                                      \
  ENUM(mode, EQ, EQ, LE, GE)
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
  Config config;
  PrinterT printer;
  uint32_t num = 0;

  void runOnInstr(InstrRef instr) {
    // ignore test dialect to not find the key itself
    if (instr.getDialect() == DIALECT_TEST)
      return;
    auto str = printer.toString(instr);
    std::regex regex(config.regex);
    for (auto it = std::sregex_iterator(str.begin(), str.end(), regex);
         it != std::sregex_iterator(); ++it)
      num++;
  }
  bool passOrFail() {
    switch (config.mode) {
    case Config::EQ:
      return num == config.num;
    case Config::LE:
      return num <= config.num;
    case Config::GE:
      return num >= config.num;
    default:
      unreachable();
    }
  }

public:
  void runWrapper(auto &&runFunc) { runFunc(); }

  bool runInstr(InstrRef instr) {
    runOnInstr(instr);
    return passOrFail();
  }

  bool run() {
    for (auto instr : ctx.getStore<Instr>()) {
      if (!ctx.getCFG().contains(instr)) {
        runOnInstr(instr);
      }
    }
    return passOrFail();
  }

  static constexpr auto runFuncs =
      mk_tuple(&AssertExistsPass::runInstr, &AssertExistsPass::run);

  explicit AssertExistsPass(Context &ctx)
      : ctx(ctx), printer(ctx, OStreamWrapper{}) {}
  auto make(Context &ctx) { return AssertExistsPass{ctx}; }
};
}; // namespace dyno
