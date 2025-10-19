#pragma once
#include "dyno/DialectInfo.h"
#include "dyno/IDImpl.h"
#include "dyno/Obj.h"
#include "dyno/Opcode.h"
#include "dyno/Parser.h"
#include "hw/HWContext.h"
#include "hw/HWParser.h"
#include "hw/HWPrinter.h"
#include "support/ArrayRef.h"
#include "support/DenseMap.h"
#include "support/Lexer.h"
#include "support/MMap.h"
#include "support/TempBind.h"
#include "support/VectorLUT.h"

namespace dyno {

class ParseDynoPass {
  struct Config {
    std::string fileName;
  };
  HWContext &ctx;

public:
  ParseDynoPass(HWContext &ctx)
      : ctx(ctx) {}
  Config config;

  void run() {
    MMap mmap{config.fileName};
    HWParser parser{ctx};
    parser.parse(mmap, config.fileName);
  }
};
}; // namespace dyno
