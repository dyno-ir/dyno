#include "aig/AIGContext.h"
#include "aig/PrintParse.h"
#include "dyno/Context.h"
#include "dyno/DialectInfo.h"
#include "dyno/IDImpl.h"
#include "dyno/Instr.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Obj.h"
#include "dyno/Parser.h"
#include "hw/PrintParse.h"
#include "meta/MetaContext.h"
#include "meta/PassPipelineInterpreter.h"
#include "op/OpContext.h"
#include "op/StringObj.h"
#include "support/ArrayRef.h"
#include "support/CmdLineArgs.h"
#include "support/DenseMap.h"
#include "support/ErrorRecovery.h"
#include "support/MMap.h"
#include <array>
#include <string>
using namespace dyno;

CmdLineArg<std::string_view> argFileName{
    std::nullopt, "input file", "Input Dyno-IR file path.",
    CmdLineArgFlags::POSITIONAL | CmdLineArgFlags::MANDATORY};

CmdLineArg<bool> argDumpAfterAll{std::nullopt, "dump-after-all",
                                 "Dump IR into ./dumps after every pass.", 0,
                                 false};
CmdLineArg<bool> argPrintAfterAll{'p', "print-after-all",
                                  "Print IR after every pass.", 0, false};

CmdLineArg<bool> argCheckAfterAll{'c', "check-after-all",
                                  "Check IR after every pass.", 0,
#ifdef DYNO_ENABLE_DEBUG
                                  1
#else
                                  0
#endif
};

constexpr DialectID DIALECT_TEST{7}; // fixme: dialect ID assignment

// create a local dialect with test case opcodes
class TestDialectPrinter {
public:
  static constexpr DialectID dialect{DIALECT_TEST};

  TestDialectPrinter(PrinterBase *) {}
};

class TestDialectParser {
  ParserBase *base;

public:
  static constexpr DialectID dialect{DIALECT_TEST};
  explicit TestDialectParser(ParserBase *base) : base(base) {}
};

class TestDialectContext {
public:
  static constexpr DialectID dialect{DIALECT_TEST};
};

constexpr DialectOpcode TEST_TEST_CASE{DIALECT_TEST, 0};

// in general to define dialect/type/opcode info specialize this struct or void
// registerDialect<>
template <> struct DialectTraits<DIALECT_TEST> {
  constexpr static DialectInfo info{"test"};
  constexpr static std::array<TyInfo, 0> tyInfo = {};
  constexpr static OpcodeInfo opcInfo[] = {OpcodeInfo{"TEST_CASE"}};
};

using DynoTestParser =
    Parser<CoreDialectParser, MetaDialectParser, OpDialectParser,
           HWDialectParser, AIGDialectParser, TestDialectParser>;

class DynoTestPrinter
    : public ContextPrinterWrapper<CoreDialectPrinter, MetaDialectPrinter,
                                   OpDialectPrinter, HWDialectPrinter,
                                   AIGDialectPrinter, TestDialectPrinter> {
public:
  DynoTestPrinter(Context &ctx, std::ostream &os)
      : ContextPrinterWrapper(ctx, os) {}
};

class BlockCompare {
  DenseMap<DynObjRef, DynObjRef> translateMap;

private:
  std::optional<std::pair<InstrRef, InstrRef>> compareBlocksImpl(BlockRef lhs,
                                                                 BlockRef rhs) {

    auto lhsIt = lhs.begin();
    auto rhsIt = rhs.begin();

#define FAIL_CUR return std::make_pair(*lhsIt, *rhsIt)

    while (lhsIt != lhs.end() && rhsIt != rhs.end()) {
      auto instrL = *lhsIt;
      auto instrR = *rhsIt;

      if (instrL.getDialectOpcode() != instrR.getDialectOpcode() ||
          instrL.getNumOperands() != instrR.getNumOperands() ||
          instrL.getNumDefs() != instrR.getNumDefs())
        FAIL_CUR;

      for (auto [opA, opB] : Range{instrL.defs()}.zip(instrR.defs())) {
        if (opA->thin().getType() != opB->thin().getType())
          FAIL_CUR;
        translateMap.insert(opB->thin(), opA->thin());

        if (opA->thin().getType() == CORE_BLOCK) {
          if (auto res =
                  compareBlocksImpl(opA->as<BlockRef>(), opB->as<BlockRef>()))
            return res;
        }
      }

      for (auto [opA, opB] : Range{instrL.others()}.zip(instrR.others())) {
        if (opA->thin().getType() != opB->thin().getType())
          FAIL_CUR;
        auto bToA = translateMap.find(opB->thin());

        if (opA->thin() !=
            (bToA != translateMap.end() ? bToA.val() : opB->thin()))
          FAIL_CUR;
      }

      ++lhsIt;
      ++rhsIt;
    }

    if (lhsIt == lhs.end() && rhsIt == rhs.end())
      return std::nullopt;
    FAIL_CUR;

#undef FAIL_CUR
  }

public:
  auto compareBlocks(BlockRef lhs, BlockRef rhs) {
    translateMap.clear();
    return compareBlocksImpl(lhs, rhs);
  }

  BlockCompare() = default;
};

class DynoTestInterpreter {
  Context &ctx;
  DynoTestPrinter print;
  std::ostream &os;

public:
  bool execTestCase(InstrRef instr, bool verbose) {
    std::string_view name = instr.def(0)->as<StringObjRef>()->data;
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
      std::print(os, "actual  : {}\n", print.toString(diff->first));
      std::print(os, "expected: {}\n\n", print.toString(diff->second));

      return true;
    }
    return false;
  }

  bool exec(BlockRef block) {
    bool allSuccess = true;
    for (auto instr : block) {
      switch (*instr.getDialectOpcode()) {
      case *TEST_TEST_CASE: {
        allSuccess &= execTestCase(instr, true);
        break;
      }

      default: {
        report_fatal_error("unknown test case: ",
                           print.toString(instr.getDialectOpcode()));
      }
      }
    }

    return allSuccess;
  }

  DynoTestInterpreter(Context &ctx, std::ostream &os)
      : ctx(ctx), print(ctx, os), os(os) {}
};

int main(int argc, char **argv) {
  Context ctx;
  HWDialectContext hwContext;
  CoreDialectContext coreContext;
  MetaDialectContext metaContext;
  OpDialectContext opContext;
  AIGDialectContext aigContext;
  TestDialectContext testContext;
  ctx.registerDialect(coreContext);
  ctx.registerDialect(hwContext);
  ctx.registerDialect(opContext);
  ctx.registerDialect(aigContext);
  ctx.registerDialect(testContext);
  // meta must be registered last
  ctx.registerDialect(metaContext);

  CmdLineArgHandler cmdLineArgHandler;
  cmdLineArgHandler.registerArg(argFileName);
  cmdLineArgHandler.registerArg(argDumpAfterAll);
  cmdLineArgHandler.registerArg(argPrintAfterAll);
  cmdLineArgHandler.registerArg(argCheckAfterAll);
  cmdLineArgHandler.parse(argc, argv);

  DynoTestParser parser{ctx};

  std::string fileName{*argFileName};
  MMap mmap{fileName};
  if (!mmap)
    report_fatal_error("failed to open file: {}", fileName);
  parser.parse(mmap, fileName);

  BlockRef block = ctx.getCtx<CoreDialectContext>().createBlock();
  parser.parse(mmap, std::move(fileName), block.end());

  DynoTestInterpreter interp{ctx, std::cout};
  auto res = interp.exec(block);

  return res ? 0 : -1;
}
