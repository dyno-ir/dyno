
#include "ParseVerilogPass.h"
#include "dyno/Context.h"
#include "hw/HWPrinter.h"
#include "hw/PassPipeline.h"
#include "hw/passes/DumpVerilog.h"
#include "hw/passes/HWDialectPasses.h"
#include "meta/IDs.h"
#include "meta/MetaContext.h"
#include "meta/MetaParser.h"
#include "meta/PassPipelineInterpreter.h"
#include "support/CmdLineArgs.h"
#include "support/ErrorRecovery.h"
#include "support/SubCommand.h"
#include "support/TwoLevelSet.h"
#include "test/IDs.h"
#include "test/TestInterpreter.h"
#include <optional>

using namespace dyno;

// Synth Args
CmdLineArg<Vec<StringRef>> argInputFiles{
    std::nullopt, "input file", "Input files.",
    CmdLineArgFlags::VALUE_REQUIRED | CmdLineArgFlags::MULTIPLE |
        CmdLineArgFlags::POSITIONAL | CmdLineArgFlags::MANDATORY};

CmdLineArg<std::string_view>
    argLibertyFile('l', "liberty", "Liberty (stdcell definitions) file path.",
                   CmdLineArgFlags::VALUE_REQUIRED | CmdLineArgFlags::MANDATORY,
                   "");

CmdLineArg<std::string_view> argOutFile('o', "",
                                        "Output Verilog netlist file path.",
                                        CmdLineArgFlags::VALUE_REQUIRED,
                                        "dump.v");

CmdLineArg<Vec<StringRef>>
    argSlangArgs('X', "Xslang",
                 "Slang arguments. Use multiple times for multiple args.",
                 CmdLineArgFlags::VALUE_REQUIRED | CmdLineArgFlags::MULTIPLE);

CmdLineArg<std::string_view> argFlowScript{'s', "script",
                                           "Dyno-IR flow script file name.",
                                           CmdLineArgFlags::VALUE_REQUIRED, ""};

// Flow/Test Args
CmdLineArg<std::string_view> argScriptFileName{
    std::nullopt, "script file", "Dyno-IR script file name.",
    CmdLineArgFlags::VALUE_REQUIRED | CmdLineArgFlags::MANDATORY |
        CmdLineArgFlags::POSITIONAL,
    ""};

// Test Args
CmdLineArg<Vec<StringRef>> argTestOnly{
    std::nullopt, "only",
    "Only run listed test, can be specified multiple times.",
    CmdLineArgFlags::VALUE_REQUIRED | CmdLineArgFlags::MULTIPLE};

// Common Args
CmdLineArg<bool> argDebug{
    'd', "debug", "Run in debug mode (only has effect for debug builds).", 0,
    false};

CmdLineArg<bool> argPrintAfterAll{std::nullopt, "print-after-all",
                                  "Print IR after all passes.", false};

void runScript(Context &ctx, StringRef fileName) {
  auto flowBlock = ctx.getCFG().blocks.create(ctx.getCFG());
  std::string flowFileName{fileName.begin(), fileName.end()};
  MMap flowFile{flowFileName};
  MetaParser metaParser{ctx};
  if (!flowFile)
    report_fatal_error("failed to open file: {}", flowFileName);
  metaParser.parse(flowFile, flowFileName, flowBlock.end());

  SmallVec<void *, 1> passCtorArgs{reinterpret_cast<void *>(&ctx)};
  MetaPassPipelineInterpreter interp{ctx, passCtorArgs};

  FatDynObjRef<> arg = nullref;
  SmallVec<void *, 1> passRunArgs{reinterpret_cast<void *>(&arg)};
  interp.interpretPassPipeline(flowBlock, passRunArgs);

  std::ofstream of{"dump.dyno"};
  HWPrinter printer{of};
  printer.printCtx(ctx);

  DumpVerilogPass dumpVerilog{ctx};
  dumpVerilog.config.fileName = *argOutFile;
  dumpVerilog.run();
}

void synth(Context &ctx) {
  {
    ParseVerilogPass parse{ctx};
    auto args = *argSlangArgs;
    args.push_back_range(Range{*argInputFiles});
    auto res = parse.parse(args);
    if (!res)
      report_fatal_error("{}", res.error());
  }

  if (argFlowScript->empty()) {
    PassPipeline pipeline{ctx};
    pipeline.printAfterAll = *argPrintAfterAll;
    pipeline.setLibertyPath(*argLibertyFile);
    pipeline.printAfterAll = false;
    pipeline.checkAfterAll = true;
    pipeline.dumpAfterAll = true;

    pipeline.runOptPipeline();
    pipeline.runLoweringPipeline();
    pipeline.dumpVerilog(*argOutFile);

    std::ofstream of{"dump.dyno"};
    pipeline.dumpDyno(of);

  } else {
    runScript(ctx, *argFlowScript);
  }
}

void script(Context &ctx) { runScript(ctx, *argScriptFileName); }

using TestParser = Parser<CoreDialectParser, MetaDialectParser, OpDialectParser,
                          HWDialectParser, AIGDialectParser, TestDialectParser>;
class TestPrinter
    : public ContextPrinterWrapper<CoreDialectPrinter, MetaDialectPrinter,
                                   OpDialectPrinter, HWDialectPrinter,
                                   AIGDialectPrinter, TestDialectPrinter> {
public:
  TestPrinter(Context &ctx, std::ostream &os) : ContextPrinterWrapper(ctx, os) {
    this->printers.get<HWDialectPrinter>().regNames =
        &ctx.getCtx<HWDialectContext>().regNameInfo;
  }
};

void test(Context &ctx) {
  TestParser parser{ctx};

  std::string fileName{argScriptFileName->begin(), argScriptFileName->end()};
  MMap mmap{fileName};
  if (!mmap)
    report_fatal_error("failed to open file: {}", fileName);

  TestPrinter print{ctx, std::cout};
  TestInterpreter interp{ctx, print};
  bool pass = true;

  TwoLevelSet<StringRef> only{Range(*argTestOnly)};
  auto val =
      parser.lexer.emplace(ctx.getDialectInfos(), mmap, std::move(fileName));
  while (auto instr = parser.parseSingle()) {
    if (instr.getDialect() != DIALECT_TEST)
      continue;
    auto nm = instr.def(0)->as<StringObjRef>()->data;
    if (only.empty() || only.contains(nm)) {
      pass &= interp.exec(instr, *argPrintAfterAll);
    }

    // Completely reset the context after a single pass. Otherwise previous'
    // freeIDs will always affect current, which can affect operand ordering.
    // (Other option would be more fuzzy comparison)
    ctx.reset();
    interp.reset();
    parser.reset();
  }
  if (!pass)
    exit(-1);
}

int main(int argc, char **argv) {
  enum { SC_SYNTH, SC_SCRIPT, SC_TEST };
  unsigned sc;
  {
    SubCommand synth("synth", SC_SYNTH);
    SubCommand script("script", SC_SCRIPT);
    SubCommand test("test", SC_TEST);

    synth.registerArg(argInputFiles);
    synth.registerArg(argLibertyFile);
    synth.registerArg(argOutFile);
    synth.registerArg(argSlangArgs);
    synth.registerArg(argFlowScript);
    synth.registerArg(argDebug);
    synth.registerArg(argPrintAfterAll);

    script.registerArg(argScriptFileName);
    script.registerArg(argDebug);
    script.registerArg(argPrintAfterAll);

    test.registerArg(argScriptFileName);
    test.registerArg(argTestOnly);
    test.registerArg(argDebug);
    test.registerArg(argPrintAfterAll);

    SubCommandHandler handler(&synth);
    handler.registerSubCmd(synth);
    handler.registerSubCmd(script);
    handler.registerSubCmd(test);
    sc = handler.parse(argc, argv);
  }

  Context ctx;
  HWDialectContext hwContext;
  CoreDialectContext coreContext;
  OpDialectContext opContext;
  AIGDialectContext aigContext;
  MetaDialectContext metaContext;
  ctx.getPassRegistry().registerPass<ParseVerilogPass>();
  ctx.registerDialect(coreContext);
  ctx.registerDialect(hwContext);
  ctx.registerDialect(opContext);
  ctx.registerDialect(aigContext);

  debugType = *argDebug;

  switch (sc) {
  case SC_SYNTH:
    ctx.registerDialect(metaContext);
    synth(ctx);
    break;
  case SC_SCRIPT:
    ctx.registerDialect(metaContext);
    script(ctx);
    break;
  case SC_TEST: {
    TestDialectContext testContext;
    ctx.registerDialect(testContext);
    ctx.registerDialect(metaContext);
    test(ctx);
    break;
  }
  }
}
