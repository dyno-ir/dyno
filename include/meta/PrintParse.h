#pragma once
#include "dyno/DialectInfo.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Parser.h"
#include "meta/IDs.h"

namespace dyno {

class MetaDialectPrinter {
public:
  static constexpr DialectID dialect{DIALECT_META};

  MetaDialectPrinter(PrinterBase *) {}
};

class MetaDialectParser {
  ParserBase *base;

public:
  static constexpr DialectID dialect{DIALECT_META};
  explicit MetaDialectParser(ParserBase *base) : base(base) {}
};
}; // namespace dyno
