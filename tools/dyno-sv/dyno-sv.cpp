
#include "ParseVerilogPass.h"
#include "aig/AIGContext.h"
#include "dyno/CFG.h"
#include "dyno/Context.h"
#include "dyno/DeepCopy.h"
#include "dyno/FatContext.h"
#include "dyno/Parser.h"
#include "dyno/Symbol.h"
#include "dyno/passes/CoreDialectPasses.h"
#include "dyno/passes/ResolveImports.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/PassPipeline.h"
#include "hw/passes/DumpVerilog.h"
#include "hw/passes/HWDialectPasses.h"
#include "meta/IDs.h"
#include "meta/MetaContext.h"
#include "meta/MetaParser.h"
#include "meta/PassPipelineInterpreter.h"
#include "op/OpContext.h"
#include "support/CmdLineArgs.h"
#include "support/Debug.h"
#include "support/ErrorRecovery.h"
#include "support/SubCommand.h"
#include "support/TwoLevelSet.h"
#include "test/IDs.h"
#include "test/TestInterpreter.h"
#include "test/passes/AssertExists.h"
#include <iterator>
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

CmdLineArg<Vec<StringRef>> argFlowScript{
    's', "script", "Dyno-IR flow script file name.",
    CmdLineArgFlags::VALUE_REQUIRED | CmdLineArgFlags::MULTIPLE};

// Flow/Test Args
CmdLineArg<Vec<StringRef>> argScriptFileName{
    std::nullopt, "script file", "Dyno-IR script file name(s).",
    CmdLineArgFlags::VALUE_REQUIRED | CmdLineArgFlags::MANDATORY |
        CmdLineArgFlags::POSITIONAL | CmdLineArgFlags::MULTIPLE};

// Test Args
CmdLineArg<Vec<StringRef>> argTestOnly{
    std::nullopt, "only",
    "Only run listed test, can be specified multiple times.",
    CmdLineArgFlags::VALUE_REQUIRED | CmdLineArgFlags::MULTIPLE};

// Common Args
CmdLineArg<bool> argDebug{
    'd', "debug", "Run in debug mode (only has effect for debug builds).", 0,
    false};

CmdLineArg<Vec<StringRef>> argDebugPasses{
    std::nullopt,
    "debug-passes",
    "Run passes in debug mode (only has effect for debug builds), can be "
    "specified multiple times.",
    CmdLineArgFlags::VALUE_REQUIRED | CmdLineArgFlags::MULTIPLE,
    {}};

CmdLineArg<bool> argPrintAfterAll{std::nullopt, "print-after-all",
                                  "Print IR after all passes.", false};

using ScriptParser = Parser<CoreDialectParser, MetaDialectParser,
                            OpDialectParser, HWDialectParser, AIGDialectParser>;
void runScript(Context &ctx, ArrayRef<StringRef> fileNames) {
  ScriptParser parser{ctx};
  auto flowBlock = ctx.getCFG().blocks.create(ctx.getCFG());
  for (auto file : fileNames) {
    std::string fileName{file.begin(), file.end()};
    MMap mmap{fileName};
    if (!mmap)
      report_fatal_error("failed to open file: {}", fileName);
    parser.parse(mmap, fileName, flowBlock.end());
  }

  ResolveImportsPass{ctx}.run();

  SmallVec<void *, 1> passCtorArgs{reinterpret_cast<void *>(&ctx)};
  MetaPassPipelineInterpreter interp{ctx, passCtorArgs};

  FatDynObjRef<> arg = nullref;
  SmallVec<void *, 1> passRunArgs{reinterpret_cast<void *>(&arg)};
  interp.interpretPassPipeline(flowBlock, passRunArgs);
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
    DumpVerilogPass dumpVlog{ctx};
    dumpVlog.config.fileName = *argOutFile;
    dumpVlog.run();
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

void test(FatContext &ctx) {
  TestParser parser{ctx};

  TwoLevelSet<StringRef> only{Range(*argTestOnly)};
  auto block = ctx.getStore<Block>().create(ctx.getCFG());

  for (auto file : *argScriptFileName) {
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
  // todo: pass registry sharing. ideally make context not own this so we can
  // just share the ref.
  sandbox.getPassRegistry().registerPass<ParseVerilogPass>();
  sandbox.getPassRegistry().registerPass<AssertExistsPass<TestPrinter>>();
  sandbox.getCtx<MetaDialectContext>().reRegister(sandbox);

  TestPrinter print{sandbox, std::cout};
  TestInterpreter interp{ctx, print};
  auto pass = interp.execBlock(block, sandbox, only, *argPrintAfterAll);
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
    synth.registerArg(argDebugPasses);
    synth.registerArg(argPrintAfterAll);

    script.registerArg(argScriptFileName);
    script.registerArg(argDebug);
    script.registerArg(argDebugPasses);
    script.registerArg(argPrintAfterAll);

    test.registerArg(argScriptFileName);
    test.registerArg(argTestOnly);
    test.registerArg(argDebug);
    test.registerArg(argDebugPasses);
    test.registerArg(argPrintAfterAll);

    SubCommandHandler handler(&synth);
    handler.registerSubCmd(synth);
    handler.registerSubCmd(script);
    handler.registerSubCmd(test);
    sc = handler.parse(argc, argv);
  }

  SymbolStore symbols;

  FatContext ctx;
  ctx.add<HWDialectContext>();
  ctx.add<CoreDialectContext>();
  ctx.add<OpDialectContext>();
  ctx.add<AIGDialectContext>();
  ctx.getPassRegistry().registerPass<ParseVerilogPass>();

  ctx.getCtx<CoreDialectContext>().setSymbols(symbols);

#ifdef DYNO_ENABLE_DEBUG
  if (*argDebug)
    dbg_enable_all();
  ctx.getPassRegistry().setDebugEnForPasses(*argDebugPasses, true);
#endif

  switch (sc) {
  case SC_SYNTH:
    ctx.add<MetaDialectContext>();
    synth(ctx);
    break;
  case SC_SCRIPT:
    ctx.add<MetaDialectContext>();
    script(ctx);
    break;
  case SC_TEST: {
    ctx.add<TestDialectContext>();
    ctx.getPassRegistry().registerPass<AssertExistsPass<TestPrinter>>();
    ctx.add<MetaDialectContext>();
    test(ctx);
    break;
  }
  }
}
