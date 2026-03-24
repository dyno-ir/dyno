#pragma once

#include "dyno/BlockCompare.h"
#include "dyno/Context.h"
#include "dyno/DeepCopy.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "meta/PassPipelineInterpreter.h"
#include "support/Any.h"
#include "support/ErrorRecovery.h"
#include "support/Format.h"
#include "test/IDs.h"
#include <expected>
#include <format>
#include <sstream>
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

  std::expected<void, Format> execTestScript(InstrRef instr, bool verbose) {
    StringRef name = instr.def(0)->as<StringObjRef>()->data;
    BlockRef expected = nullref;
    BlockRef passes;
    if (instr.getNumDefs() == 2) {
      passes = instr.def(1)->as<BlockRef>();
    } else if (instr.getNumDefs() == 3) {
      expected = instr.def(1)->as<BlockRef>();
      passes = instr.def(2)->as<BlockRef>();
    } else
      return std::unexpected("a");

    std::array<void *, 1> ctorArgs = {reinterpret_cast<void *>(&ctx)};
    MetaPassPipelineInterpreter pipeline{ctx, ctorArgs};

    FatDynObjRef<> n = nullref;
    std::array<void *, 1> args = {reinterpret_cast<void *>(&n)};
    if (!pipeline.interpretPassPipeline(passes, args))
      return std::unexpected("failed to run passes");

    if (instr.getNumDefs() == 3) {
      if (expected.size() != 1)
        return std::unexpected("expected single instr in expected");
      auto expectedI = *expected.begin();
      auto expectedO = expectedI.def()->fat();
      if (expectedO.getObjID() != Any{0U, 1U})
        return std::unexpected("more objects than expected");
      auto other = DynObjRef{expectedO.getDialectID(), expectedO.getTyID(),
                             ObjID{1U - expectedO.getObjID()}, 0};
      auto otherO = ctx.resolve(other);
      auto otherDef = otherO.as<FatDynObjRef<InstrDefUse>>()->getSingleDef();
      if (!otherDef)
        return std::unexpected("expected single def");
      auto otherI = otherDef->instr();

      if (auto diff = BlockCompare{}.compareInstrs(expectedI, otherI)) {
        std::stringstream str;
        std::print(str, "failed test: \"{}\"\n", name);
        std::print(str, "actual  : {}\n",
                   diff->first ? print.toString(diff->first) : "<none>");
        std::print(str, "expected: {}\n\n",
                   diff->second ? print.toString(diff->second) : "<none>");
        return std::unexpected(Format("{}", std::move(str).str()));
      }
    }

    std::print(os, "passed test: \"{}\"\n", name);
    return {};
  }

  bool exec(InstrRef instr, bool verbose) {
    switch (*instr.getDialectOpcode()) {
    case *TEST_TEST_CASE: {
      return execTestCase(instr, verbose);
    }
    case *TEST_TEST_EQUIVALENCE: {
      return execTestEquiv(instr, verbose);
    }
    case *TEST_TEST_SCRIPT: {
      if (auto rv = execTestScript(instr, verbose); !rv) {
        os << rv.error() << "\n";
        return false;
      }
      return true;
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
