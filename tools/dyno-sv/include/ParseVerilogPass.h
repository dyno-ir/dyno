#pragma once

#include "dyno/Context.h"
#include "dyno/DialectInfo.h"
#include "dyno/Pass.h"
#include "hw/Module.h"
#include "support/ErrorRecovery.h"
#include <expected>

namespace dyno {

class ParseVerilogPass : public Pass<ParseVerilogPass> {
  Context &ctx;

public:
  static constexpr DialectID dialect{DIALECT_HW};
  // no argument lists currently, so hack around it.
  static constexpr size_t NumArgs = 16;
#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM)                                      \
  FIELD(bool, strictXZ, true)                                                  \
  FIELD(bool, ignoreSimOnly, false)                                            \
  FIELD(StringRef, path, "")                                                   \
  FIELD(StringRef, code, "")                                                   \
  FIELD(StringRef, arg0, "")                                                   \
  FIELD(StringRef, arg1, "")                                                   \
  FIELD(StringRef, arg2, "")                                                   \
  FIELD(StringRef, arg3, "")                                                   \
  FIELD(StringRef, arg4, "")                                                   \
  FIELD(StringRef, arg5, "")                                                   \
  FIELD(StringRef, arg6, "")                                                   \
  FIELD(StringRef, arg7, "")                                                   \
  FIELD(StringRef, arg8, "")                                                   \
  FIELD(StringRef, arg9, "")                                                   \
  FIELD(StringRef, arg10, "")                                                  \
  FIELD(StringRef, arg11, "")                                                  \
  FIELD(StringRef, arg12, "")                                                  \
  FIELD(StringRef, arg13, "")                                                  \
  FIELD(StringRef, arg14, "")                                                  \
  FIELD(StringRef, arg15, "")
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
  Config config;
  MutArrayRef<StringRef> configArgsArr() {
    return MutArrayRef<StringRef>(&config.arg0, NumArgs);
  };

public:
  std::expected<void, std::string> parse(ArrayRef<StringRef> args);

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
