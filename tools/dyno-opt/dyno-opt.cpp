#include "aig/AIGContext.h"
#include "dyno/Context.h"
#include "dyno/DialectInfo.h"
#include "dyno/Lexer.h"
#include "dyno/Obj.h"
#include "dyno/Parser.h"
#include "hw/HWContext.h"
#include "hw/HWParser.h"
#include "hw/HWPrinter.h"
#include "hw/PassPipeline.h"
#include "hw/passes/HWDialectPasses.h"
#include "meta/IDs.h"
#include "meta/MetaContext.h"
#include "meta/MetaParser.h"
#include "meta/MetaPassManager.h"
#include "meta/PassPipelineInterpreter.h"
#include "op/MapObj.h"
#include "op/OpContext.h"
#include "support/ArrayRef.h"
#include "support/CmdLineArgs.h"
#include "support/Debug.h"
#include "support/DenseMap.h"
#include "support/ErrorRecovery.h"
#include "support/MMap.h"
#include "support/VectorLUT.h"
#include <array>
#include <string>
using namespace dyno;

CmdLineArg<std::string_view> argFileName{
    std::nullopt, "input file", "Input Dyno-IR file path.",
    CmdLineArgFlags::POSITIONAL | CmdLineArgFlags::MANDATORY};

CmdLineArg<std::string_view> argFlowFileName{
    'f', "flow", "Input Dyno-IR flow file path (passes to execute).",
    CmdLineArgFlags::VALUE_REQUIRED | CmdLineArgFlags::MANDATORY};

CmdLineArg<std::string_view> argOutFile('o', "", "Output Dyno-IR file path.",
                                        CmdLineArgFlags::VALUE_REQUIRED,
                                        "out.dyno");
CmdLineArg<std::string_view>
    argLibertyFile('l', "liberty", "Liberty (stdcell definitions) file path.",
                   CmdLineArgFlags::VALUE_REQUIRED | CmdLineArgFlags::MANDATORY,
                   "");
CmdLineArg<bool> argOptPipeline{std::nullopt, "opt", "Run opt pipeline.", 0,
                                false};
CmdLineArg<bool> argLowerPipeline{std::nullopt, "lower",
                                  "Run lowering pipeline.", 0, false};
CmdLineArg<uint32_t> argRunStart{std::nullopt, "start-from",
                                 "Integer, only run passes with ID >= this.", 0,
                                 0};

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

int main(int argc, char **argv) {
  Context ctx;
  HWDialectContext hwContext;
  CoreDialectContext coreContext;
  MetaDialectContext metaContext;
  OpDialectContext opContext;
  AIGDialectContext aigContext;
  ctx.registerDialect(coreContext);
  ctx.registerDialect(hwContext);
  ctx.registerDialect(opContext);
  ctx.registerDialect(aigContext);
  // meta must be registered last
  ctx.registerDialect(metaContext);

  CmdLineArgHandler cmdLineArgHandler;
  cmdLineArgHandler.registerArg(argFileName);
  cmdLineArgHandler.registerArg(argFlowFileName);
  cmdLineArgHandler.registerArg(argOutFile);
  cmdLineArgHandler.registerArg(argLibertyFile);
  cmdLineArgHandler.registerArg(argOptPipeline);
  cmdLineArgHandler.registerArg(argLowerPipeline);
  cmdLineArgHandler.registerArg(argRunStart);
  cmdLineArgHandler.registerArg(argDumpAfterAll);
  cmdLineArgHandler.registerArg(argPrintAfterAll);
  cmdLineArgHandler.registerArg(argCheckAfterAll);
  cmdLineArgHandler.parse(argc, argv);

  HWParser parser{ctx};

  std::string fileName{*argFileName};
  MMap mmap{fileName};
  if (!mmap)
    report_fatal_error("failed to open file: {}", fileName);
  parser.parse(mmap, fileName);
  dumpCtx(ctx);

  MetaParser metaParser{ctx};
  auto flowBlock = ctx.getCFG().blocks.create(ctx.getCFG());
  auto flowFileName = std::string(*argFlowFileName);
  MMap flowFile{flowFileName};
  if (!flowFile)
    report_fatal_error("failed to open file: {}", flowFileName);
  metaParser.parse(flowFile, flowFileName, flowBlock.end());

  SmallVec<void *, 1> passCtorArgs{reinterpret_cast<void *>(&ctx)};
  FatDynObjRef<> arg = nullref;
  SmallVec<void *, 1> passRunArgs{reinterpret_cast<void *>(&arg)};

  MetaPassPipelineInterpreter pipeline{ctx, passCtorArgs};

  pipeline.interpretPassPipeline(flowBlock, passRunArgs);

  std::ofstream str{argOutFile->data()};
  HWPrinter{str}.printCtx(ctx);
}
