#pragma once

#include "Frontend.h"
#include "dyno/Context.h"
#include "dyno/Pass.h"
#include "hw/Module.h"
#include "slang/driver/Driver.h"
#include "support/ErrorRecovery.h"
#include <expected>

namespace dyno {

class ParseVerilogPass : public Pass<ParseVerilogPass> {
  Context &ctx;

public:
  // no argument lists currently, so hack around it.
  static constexpr size_t NumArgs = 8;
#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM)                                      \
  FIELD(StringRef, path, "")                                                   \
  FIELD(StringRef, arg0, "")                                                   \
  FIELD(StringRef, arg1, "")                                                   \
  FIELD(StringRef, arg2, "")                                                   \
  FIELD(StringRef, arg3, "")                                                   \
  FIELD(StringRef, arg4, "")                                                   \
  FIELD(StringRef, arg5, "")                                                   \
  FIELD(StringRef, arg6, "")                                                   \
  FIELD(StringRef, arg7, "")
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
  Config config;
  MutArrayRef<StringRef> configArgsArr() { return {&config.arg0, NumArgs}; };

public:
  std::expected<void, std::string> parse(ArrayRef<StringRef> args) {
    slang::driver::Driver driver;
    driver.addStandardArgs();

    auto numArgs = Range{args}.find(StringRef::emptyRef()) - args.begin();
    args = ArrayRef<StringRef>(args.data(), numArgs);

    std::array<std::string, NumArgs + 1> argStore{"dyno-sv"};
    std::array<char *, NumArgs + 2> argv{argStore[0].data()};
    for (auto [i, str] : Range{args}.enumerate()) {
      argStore[i + 1] = std::string(str.begin(), str.end());
      argv[i + 1] = argStore[i + 1].data();
    }

    if (!driver.parseCommandLine(1 + numArgs, argv.data()))
      return std::unexpected("slang: failed to parse args");

    if (!driver.processOptions())
      return std::unexpected("slang: failed to process options");

    std::unique_ptr<slang::ast::Compilation> compilation;

    bool compilation_ok;
    compilation_ok = driver.parseAllSources();
    compilation = driver.createCompilation();
    driver.reportCompilation(*compilation, false);
    auto diag = compilation->getSemanticDiagnostics();

    if (!compilation_ok) {
      return std::unexpected("slang: errors found during compilation");
    }

    if (!driver.reportDiagnostics(true))
      return std::unexpected("slang: errors found during compilation");

    VisitorAST visitor{ctx, driver.sourceManager};
    compilation->getRoot().visit(visitor);
    visitor.handle_modules();

    return {};
  }

public:
  void runWrapper(auto &&runFunc) { runFunc(); }

  void run() {
    runWrapper([&] {
      auto res = parse(configArgsArr());
      if (!res) {
        report_fatal_error("{}", res.error());
      }
    });
  }

  static constexpr auto runFuncs = mk_tuple(&ParseVerilogPass::run);

  explicit ParseVerilogPass(Context &ctx) : ctx(ctx) {}
  auto make(Context &ctx) { return ParseVerilogPass{ctx}; }
};
}; // namespace dyno
