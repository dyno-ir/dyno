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
  struct Config {
    std::string fileName;
  };
  Context &ctx;

public:
  auto make(Context &ctx) { return ParseDynoPass(ctx); }
  explicit ParseDynoPass(Context &ctx) : ctx(ctx) {}
  Config config;

  void run() {
    MMap mmap{config.fileName};
    if (mmap.size() == 0)
      report_fatal_error(("could not open file: " + config.fileName).c_str());
    HWParser parser{ctx};
    parser.parse(mmap, config.fileName);
  }
};
}; // namespace dyno
