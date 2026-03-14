#pragma once

#include "dyno/BlockCompare.h"
#include "dyno/Context.h"
#include "dyno/DeepCopy.h"
#include "meta/PassPipelineInterpreter.h"
#include "test/IDs.h"
#include <format>
namespace dyno {
class TestInterpreter {
  Context &ctx;
  PrinterBase &print;
  std::ostream &os;

public:
  bool execTestCase(InstrRef instr, bool verbose) {
    StringRef name = instr.def(0)->as<StringObjRef>()->data;
    auto pre = instr.def(1)->as<BlockRef>();
    auto post = instr.def(2)->as<BlockRef>();
    auto passes = instr.def(3)->as<BlockRef>();

    std::array<void *, 1> ctorArgs = {reinterpret_cast<void *>(&ctx)};
    MetaPassPipelineInterpreter pipeline{ctx, ctorArgs};

    for (auto instr : pre) {
      FatDynObjRef<> ref{instr};
      std::array<void *, 1> args = {reinterpret_cast<void *>(&ref)};
      pipeline.interpretPassPipeline(passes, args);
    }

    if (verbose)
      print.printInstr(instr);

    BlockCompare compare{};
    if (auto diff = compare.compareBlocks(pre, post)) {
      std::print(os, "failed test: \"{}\"\n", name);
      std::print(os, "actual  : {}\n",
                 diff->first ? print.toString(diff->first) : "<none>");
      std::print(os, "expected: {}\n\n",
                 diff->second ? print.toString(diff->second) : "<none>");

      return false;
    }

    std::print(os, "passed test: \"{}\"\n", name);
    return true;
  }

  bool execTestEquiv(InstrRef instr, bool verbose) {
    StringRef name = instr.def(0)->as<StringObjRef>()->data;
    auto pre = instr.def(1)->as<BlockRef>();
    auto passes = instr.def(2)->as<BlockRef>();

    std::array<void *, 1> ctorArgs = {reinterpret_cast<void *>(&ctx)};
    MetaPassPipelineInterpreter pipeline{ctx, ctorArgs};

    if (pre.size() != 1)
      report_fatal_error("expected single Instr in body of test \"{}\"", name);

    // duplicate the instr to be tested
    DeepCopier copier{ctx};
    copier.copyInstr(*pre.begin(), pre.end());

    for (auto instr : Range{pre}.drop_back()) {
      FatDynObjRef<> ref{instr};
      std::array<void *, 1> args = {reinterpret_cast<void *>(&ref)};
      pipeline.interpretPassPipeline(passes, args);
    }

    std::print(os, "passed test: \"{}\"\n", name);
    return true;
  }

  bool exec(InstrRef instr, bool verbose) {
    switch (*instr.getDialectOpcode()) {
    case *TEST_TEST_CASE: {
      return execTestCase(instr, verbose);
    }
    case *TEST_TEST_EQUIVALENCE: {
      return execTestEquiv(instr, verbose);
    }

    default: {
      report_fatal_error("unknown test case: ",
                         print.toString(instr.getDialectOpcode()));
    }
    }
  }

  TestInterpreter(Context &ctx, PrinterBase &print)
      : ctx(ctx), print(print), os(print.str) {}

  void reset() { print.reset(); }
};

}; // namespace dyno
