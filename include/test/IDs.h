#pragma once

#include "dyno/IDImpl.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Parser.h"
namespace dyno {
constexpr DialectID DIALECT_TEST{7}; // fixme: dialect ID assignment

class TestDialectPrinter {
public:
  static constexpr DialectID dialect{DIALECT_TEST};

  TestDialectPrinter(PrinterBase *) {}
};

class TestDialectParser {

public:
  static constexpr DialectID dialect{DIALECT_TEST};
  explicit TestDialectParser(ParserBase *) {}
};

class TestDialectContext {
public:
  static constexpr DialectID dialect{DIALECT_TEST};
};
template <> struct DialectContext<DialectID{DIALECT_TEST}> {
  using t = TestDialectContext;
};

constexpr DialectOpcode TEST_TEST_CASE{DIALECT_TEST, 0};
constexpr DialectOpcode TEST_TEST_EQUIVALENCE{DIALECT_TEST, 1};

// in general to define dialect/type/opcode info specialize this struct or void
// registerDialect<>
template <> struct DialectTraits<DIALECT_TEST> {
  constexpr static DialectInfo info{"test"};
  constexpr static std::array<TyInfo, 0> tyInfo = {};
  constexpr static OpcodeInfo opcInfo[] = {OpcodeInfo{"TEST_CASE"},
                                           OpcodeInfo{"TEST_EQUIVALENCE"}};
};
}; // namespace dyno
