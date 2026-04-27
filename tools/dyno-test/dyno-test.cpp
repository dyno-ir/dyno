#include "aig/AIGContext.h"
#include "aig/PrintParse.h"
#include "dyno/BlockCompare.h"
#include "dyno/Context.h"
#include "dyno/DeepCopy.h"
#include "dyno/DialectInfo.h"
#include "dyno/FatContext.h"
#include "dyno/IDImpl.h"
#include "dyno/Instr.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Lexer.h"
#include "dyno/Obj.h"
#include "dyno/Parser.h"
#include "dyno/passes/ResolveImports.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/PrintParse.h"
#include "hw/passes/HWDialectPasses.h"
#include "meta/MetaContext.h"
#include "meta/PassPipelineInterpreter.h"
#include "op/OpContext.h"
#include "op/StringObj.h"
#include "support/ArrayRef.h"
#include "support/CmdLineArgs.h"
#include "support/DenseMap.h"
#include "support/ErrorRecovery.h"
#include "support/MMap.h"
#include "test/IDs.h"
#include "test/TestInterpreter.h"
#include <array>
#include <string>
using namespace dyno;

CmdLineArg<Vec<StringRef>> argFileName{
    std::nullopt, "input file", "Input Dyno-IR file(s) path.",
    CmdLineArgFlags::POSITIONAL | CmdLineArgFlags::MANDATORY |
        CmdLineArgFlags::MULTIPLE};

CmdLineArg<bool> argDumpAfterAll{std::nullopt, "dump-after-all",
                                 "Dump IR into ./dumps after every pass.", 0,
                                 false};
CmdLineArg<bool> argPrintAfterAll{'p', "print-after-all",
                                  "Print IR after every pass.", 0, false};

CmdLineArg<bool> argDebug{
    'd', "debug", "Run in debug mode (only has effect for debug builds).", 0,
    false};

CmdLineArg<Vec<StringRef>> argTestOnly{
    std::nullopt, "only",
    "Only run listed test, can be specified multiple times.",
    CmdLineArgFlags::VALUE_REQUIRED | CmdLineArgFlags::MULTIPLE};

using DynoTestParser =
    Parser<CoreDialectParser, MetaDialectParser, OpDialectParser,
           HWDialectParser, AIGDialectParser, TestDialectParser>;

class DynoTestPrinter
    : public ContextPrinterWrapper<CoreDialectPrinter, MetaDialectPrinter,
                                   OpDialectPrinter, HWDialectPrinter,
                                   AIGDialectPrinter, TestDialectPrinter> {
public:
  DynoTestPrinter(Context &ctx, std::ostream &os)
      : ContextPrinterWrapper(ctx, os) {
    this->printers.get<HWDialectPrinter>().regNames =
        &ctx.getCtx<HWDialectContext>().regNameInfo;
  }
};

int main(int argc, char **argv) {
  SymbolStore symbols;
  FatContext ctx;
  ctx.add<HWDialectContext>();
  ctx.add<CoreDialectContext>(&symbols);
  ctx.add<OpDialectContext>();
  ctx.add<AIGDialectContext>();
  ctx.add<TestDialectContext>();
  ctx.add<MetaDialectContext>();

  CmdLineArgHandler cmdLineArgHandler;
  cmdLineArgHandler.registerArg(argFileName);
  cmdLineArgHandler.registerArg(argDumpAfterAll);
  cmdLineArgHandler.registerArg(argPrintAfterAll);
  cmdLineArgHandler.registerArg(argTestOnly);
  cmdLineArgHandler.registerArg(argDebug);
  cmdLineArgHandler.parse(argc, argv);

  DynoTestParser parser{ctx};

  TwoLevelSet<StringRef> only{Range(*argTestOnly)};
  auto block = ctx.getStore<Block>().create(ctx.getCFG());

  for (auto file : *argFileName) {
    std::string fileName{file.begin(), file.end()};
    MMap mmap{fileName};
    if (!mmap)
      report_fatal_error("failed to open file: {}", fileName);
    parser.parse(mmap, fileName, block.end());
  }

  ResolveImportsPass{ctx}.run();
  auto sandbox = ctx.create();
  sandbox.getCtx<CoreDialectContext>().setSymbols(
      *ctx.getCtx<CoreDialectContext>().symbols);
  DynoTestPrinter print{ctx, std::cout};
  TestInterpreter interp{sandbox, print};
  auto pass = interp.execBlock(block, sandbox, only, *argPrintAfterAll);
  return pass ? 0 : -1;
}
