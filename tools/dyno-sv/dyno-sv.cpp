
#include "ParseVerilogPass.h"
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
#include <optional>

using namespace dyno;

CmdLineArg<Vec<StringRef>> argInputFiles{0, "", "Input files.",
                                         CmdLineArgFlags::VALUE_REQUIRED |
                                             CmdLineArgFlags::MULTIPLE |
                                             CmdLineArgFlags::POSITIONAL};
CmdLineArg<std::string_view> argFlowFileName{
    'f', "flow",
    "Dyno-IR flow script file name. If not specified falls "
    "back to default flow,",
    CmdLineArgFlags::VALUE_REQUIRED, ""};

CmdLineArg<std::string_view>
    argLibertyFile('l', "liberty", "Liberty (stdcell definitions) file path.",
                   CmdLineArgFlags::VALUE_REQUIRED | CmdLineArgFlags::MANDATORY,
                   "");

CmdLineArg<std::string_view> argOutFile('o', "",
                                        "Output Verilog netlist file path.",
                                        CmdLineArgFlags::VALUE_REQUIRED,
                                        "dump.v");

CmdLineArg<Vec<StringRef>>
    argSlangArgs('s', "slang",
                 "Slang arguments. Use multiple times for multiple args.",
                 CmdLineArgFlags::VALUE_REQUIRED | CmdLineArgFlags::MULTIPLE);

int main(int argc, char **argv) {
  {
    CmdLineArgHandler cmdLineArgHandler;
    cmdLineArgHandler.registerArg(argInputFiles);
    cmdLineArgHandler.registerArg(argFlowFileName);
    cmdLineArgHandler.registerArg(argLibertyFile);
    cmdLineArgHandler.registerArg(argOutFile);
    cmdLineArgHandler.registerArg(argSlangArgs);
    cmdLineArgHandler.parse(argc, argv);
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
  ctx.registerDialect(metaContext);

  {
    ParseVerilogPass parse{ctx};
    auto args = *argSlangArgs;
    args.push_back_range(Range{*argInputFiles});
    auto res = parse.parse(args);
    if (!res)
      report_fatal_error("{}", res.error());
  }
  std::cout << "\n\n\n";

  if (argFlowFileName->empty()) {
    PassPipeline pipeline{ctx};
    pipeline.setLibertyPath(*argLibertyFile);
    pipeline.printAfterAll = false;
    pipeline.checkAfterAll = true;
    pipeline.dumpAfterAll = true;
    debugType = 1;

    pipeline.runOptPipeline();
    pipeline.runLoweringPipeline();
    pipeline.dumpVerilog(*argOutFile);

    std::ofstream of{"dump.dyno"};
    pipeline.dumpDyno(of);

  } else {
    auto flowBlock = ctx.getCFG().blocks.create(ctx.getCFG());
    std::string flowFileName{*argFlowFileName};
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
}
