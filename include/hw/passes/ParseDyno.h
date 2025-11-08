#pragma once
#include "dyno/DialectInfo.h"
#include "dyno/IDImpl.h"
#include "dyno/Obj.h"
#include "dyno/Opcode.h"
#include "dyno/Parser.h"
#include "hw/HWContext.h"
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
  Config config;

  void run() {
    MMap mmap{config.fileName};
    auto tok = lexer.emplace(mmap, std::string(config.fileName));
  }
};
}; // namespace dyno
