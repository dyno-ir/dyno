#pragma once

#include "dyno/Context.h"
#include "dyno/Pass.h"
#include "support/Bits.h"
#include "support/Debug.h"
#include "support/Tokenizer.h"

namespace dyno {

class EnableDebugPass : public Pass<EnableDebugPass> {
  Context &ctx;

public:
#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM)                                      \
  FIELD(bool, enable, true)                                                    \
  FIELD(std::string, passes, "")
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
  Config config;

  void run() {
#if DYNO_ENABLE_DEBUG
    if (config.enable) {
      dbg_enable_all();
      dbg_disable_for_id(128); // known bits
      dbg_disable_for_id(129); // hwinterp
    } else
      dbg_disable_all();

    SmallVec<StringRef, 4> enablePasses, disablePasses;
    for (auto pass : Tokenizer{config.passes, ",;: "}) {
      auto tok = Tokenizer{pass, "="};
      auto toks = SmallVec<std::string_view, 2>(Range{tok});

      if (toks.size() == 1)
        enablePasses.emplace_back(toks[0]);
      else {
        if (toks.size() != 2 || toks[1] != Any{"0", "1", "true", "false"})
          report_fatal_error("invalid per-pass debug enable string");
        (toks[1] == Any{"1", "true"} ? enablePasses : disablePasses)
            .emplace_back(toks[0]);
      }

      ctx.getPassRegistry().setDebugEnForPasses(disablePasses, false);
      ctx.getPassRegistry().setDebugEnForPasses(enablePasses, true);
    }
#else
    std::cout << "EnableDebugPass: not a debug build, ignoring\n";
#endif
  }

  static constexpr auto runFuncs = mk_tuple(&EnableDebugPass::run);

  explicit EnableDebugPass(Context &ctx) : ctx(ctx) {}
  auto make(Context &ctx) { return EnableDebugPass{ctx}; }
};
}; // namespace dyno
