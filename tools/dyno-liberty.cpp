

#include "aig/AIGContext.h"
#include "hw/HWContext.h"
#include "hw/HWParser.h"
#include "hw/HWPrinter.h"
#include "hw/SensList.h"
#include "hw/passes/AggressiveDeadCodeElimination.h"
#include "hw/passes/CommonSubexpressionElimination.h"
#include "hw/passes/FuzzyCSE.h"
#include "hw/passes/HWDialectPasses.h"
#include "hw/passes/InstCombine.h"
#include "hw/passes/OrderInstrs.h"
#include "hw/passes/ParseLiberty.h"
#include "hw/passes/TriggerDedupe.h"
#include "meta/MetaContext.h"
#include "meta/MetaParser.h"
#include "meta/PassPipelineInterpreter.h"
#include "op/OpContext.h"
#include "support/CmdLineArgs.h"
#include "support/MMap.h"
#include <string_view>
using namespace dyno;

CmdLineArg<std::string_view> argFileName{
    std::nullopt, "input file", "Input Liberty file path.",
    CmdLineArgFlags::POSITIONAL | CmdLineArgFlags::MANDATORY};

CmdLineArg<std::string_view> argFlowFileName{
    'f', "flow",
    "Input Dyno-IR flow script path (passes to execute). If not specified "
    "falls back to default "
    "Liberty pipeline.",
    CmdLineArgFlags::VALUE_REQUIRED};

CmdLineArg<std::string_view> argOutFile('o', "", "Output Dyno-IR file path.",
                                        CmdLineArgFlags::VALUE_REQUIRED,
                                        "out.dyno");

CmdLineArg<bool> argDebug{
    'd', "debug", "Run in debug mode (only has effect for debug builds).", 0,
    false};

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
  cmdLineArgHandler.registerArg(argDebug);
  cmdLineArgHandler.parse(argc, argv);

#ifdef DYNO_ENABLE_DEBUG
  if (*argDebug)
    dbg_enable_all();
#endif

  ParseLibertyPass parseLiberty{ctx};
  parseLiberty.config.path = *argFileName;
  parseLiberty.run();

  if (!argFlowFileName->empty()) {
    auto flowBlock = ctx.getCFG().blocks.create(ctx.getCFG());
    auto flowFileName = std::string(*argFlowFileName);
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
  } else {
    AggressiveDeadCodeEliminationPass aggressiveDCE{ctx};
    FuzzyCSEPass fuzzyCSE{ctx};
    OrderInstrsPass orderInstrs{ctx};
    InstCombinePass instCombine{ctx};
    CommonSubexpressionEliminationPass cse{ctx};
    instCombine.config.removeAssumes = true;
    instCombine.config.findFlipFlopEnables = true;
    instCombine.config.findFlipFlopSyncResets = true;
    instCombine.config.inferMuxs = true;

    cse.run();
    aggressiveDCE.run();
    fuzzyCSE.run();
    orderInstrs.run();
    instCombine.run();
    aggressiveDCE.run();
  }

  std::ofstream str{std::string(*argOutFile)};
  if (!str)
    report_fatal_error("failed to open file: {}", *argOutFile);
  HWPrinter print{str};
  print.printCtx(ctx, true);

  return 0;
}
