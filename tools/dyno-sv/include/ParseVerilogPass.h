#pragma once

#include "Frontend.h"
#include "dyno/Context.h"
#include "dyno/DialectInfo.h"
#include "dyno/Pass.h"
#include "hw/Module.h"
#include "slang/driver/Driver.h"
#include "slang/text/SourceLocation.h"
#include "slang/util/SmallVector.h"
#include "support/ErrorRecovery.h"
#include <expected>

namespace dyno {

class ParseVerilogPass : public Pass<ParseVerilogPass> {
  Context &ctx;

public:
  static constexpr DialectID dialect{DIALECT_HW};
  // no argument lists currently, so hack around it.
  static constexpr size_t NumArgs = 8;
#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM)                                      \
  FIELD(StringRef, path, "")                                                   \
  FIELD(StringRef, code, "")                                                   \
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

    SmallVec<std::string, 16> argStore{"dyno-sv"};
    argStore.reserve(args.size() + 1);
    SmallVec<char *, 16> argv{argStore[0].data()};
    argv.reserve(args.size() + 1);
    for (auto str : args) {
      auto &elem = argStore.emplace_back(std::string(str.begin(), str.end()));
      argv.emplace_back(elem.data());
    }

    if (!driver.parseCommandLine(1 + numArgs, argv.data()))
      return std::unexpected("slang: failed to parse args");

    // allow inluding files via config.path, config.code (verbatim) or just
    // args.
    if (!config.path.empty())
      driver.sourceLoader.addFiles(config.path);
    if (!config.code.empty()) {
      slang::SmallVector<char> buffer(config.code.size() + 1,
                                      slang::UninitializedTag{});
      buffer.append_range(config.code);
      buffer.emplace_back(0);
      auto buf =
          driver.sourceManager.assignBuffer("config.code", std::move(buffer));
      driver.sourceLoader.addBuffer(buf);
    }

    if (!driver.processOptions())
      return std::unexpected("slang: failed to process options");

    std::unique_ptr<slang::ast::Compilation> compilation;

    bool compilation_ok;
    compilation_ok = driver.parseAllSources();
    compilation = driver.createCompilation();
    driver.reportCompilation(*compilation, true);
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
