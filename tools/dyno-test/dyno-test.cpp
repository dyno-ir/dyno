#include "aig/AIGContext.h"
#include "aig/PrintParse.h"
#include "dyno/BlockCompare.h"
#include "dyno/Context.h"
#include "dyno/DeepCopy.h"
#include "dyno/DialectInfo.h"
#include "dyno/IDImpl.h"
#include "dyno/Instr.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Lexer.h"
#include "dyno/Obj.h"
#include "dyno/Parser.h"
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

CmdLineArg<StringRef> argFileName{
    std::nullopt, "input file", "Input Dyno-IR file path.",
    CmdLineArgFlags::POSITIONAL | CmdLineArgFlags::MANDATORY};

CmdLineArg<bool> argDumpAfterAll{std::nullopt, "dump-after-all",
                                 "Dump IR into ./dumps after every pass.", 0,
                                 false};
CmdLineArg<bool> argPrintAfterAll{'p', "print-after-all",
                                  "Print IR after every pass.", 0, false};

CmdLineArg<bool> argDebug{
    'd', "debug", "Run in debug mode (only has effect for debug builds).", 0,
    false};

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
  cmdLineArgHandler.registerArg(argDebug);
  cmdLineArgHandler.parse(argc, argv);

  DynoTestParser parser{ctx};

  std::string fileName{argFileName->begin(), argFileName->end()};
  MMap mmap{fileName};
  if (!mmap)
    report_fatal_error("failed to open file: {}", fileName);

  DynoTestPrinter print{ctx, std::cout};
  TestInterpreter interp{ctx, print};
  bool pass = true;

  debugType = *argDebug;

  // DynoLexer::State state = {};
  // while (auto instr = parser.parseSingle(mmap, fileName, state)) {
  //   pass &= interp.exec(instr, *argPrintAfterAll);

  //   // Completely reset the context after a single pass. Otherwise previous'
  //   // freeIDs will always affect current, which can affect operand ordering.
  //   // (Other option would be more fuzzy comparison)
  //   ctx.reset();
  //   interp.reset();
  //   parser.reset();
  // }
  return pass ? 0 : -1;
}
