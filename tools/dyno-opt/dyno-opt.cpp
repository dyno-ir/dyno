#include "aig/AIGContext.h"
#include "dyno/Context.h"
#include "dyno/Obj.h"
#include "dyno/Parser.h"
#include "hw/HWContext.h"
#include "hw/HWParser.h"
#include "hw/HWPrinter.h"
#include "hw/PassPipeline.h"
#include "hw/passes/LowerOps.h"
#include "meta/IDs.h"
#include "meta/MetaContext.h"
#include "meta/MetaParser.h"
#include "meta/MetaPassManager.h"
#include "op/MapObj.h"
#include "op/OpContext.h"
#include "support/CmdLineArgs.h"
#include "support/Debug.h"
#include "support/DenseMap.h"
#include "support/ErrorRecovery.h"
#include "support/MMap.h"
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

class MetaPassPipelineInterpreter {
  DynoLexer &lexer;
  ArrayRef<void *> passCtorArgs;
  DenseMap<uint16_t, TypeErasedPassObj> passObjs;

public:
  void interpretPassPipeline(BlockRef block) {
    for (auto instr : block) {
      if (instr.getDialect() != DIALECT_META)
        report_fatal_error("expected meta dialect instruction");
      auto opc = instr.getDialectOpcode();
      auto &pass =
          passObjs
              .findOrInsert(
                  opc.getOpcodeID().num,
                  [&]() { return metaPassManager.getPass(opc, passCtorArgs); })
              .second.val();

      if (instr.getNumOperands() != 0) {
        if (instr.getNumOperands() != 1)
          report_fatal_error("expected at most one operand (config)");
        auto cfg = instr.operand(0)->dyn_as<MapRef>();
        if (!cfg)
          report_fatal_error("expected map object");
        pass.config(cfg->data, lexer);
      }
      pass.run(ArrayRef<void *>::emptyRef());
    }
  }

  MetaPassPipelineInterpreter(DynoLexer &lexer, ArrayRef<void *> passCtorArgs)
      : lexer(lexer), passCtorArgs(passCtorArgs) {}
};

int main(int argc, char **argv) {
  Context context;
  HWDialectContext hwContext;
  CoreDialectContext coreContext;
  MetaDialectContext metaContext;
  OpDialectContext opContext;
  AIGDialectContext aigContext;
  context.registerDialect(coreContext);
  context.registerDialect(hwContext);
  context.registerDialect(opContext);
  context.registerDialect(aigContext);
  // meta must be registered last
  context.registerDialect(metaContext);

  // use context
  auto w = context.getStore<Wire>().create(32);
  ObjRef<Wire> thin = w;
  FatDynObjRef<> fatDyn = context.resolve(thin);
  assert(w == fatDyn.as<WireRef>());

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

  HWContext ctx;
  HWParser parser{ctx};

  std::string fileName{*argFileName};
  MMap mmap{fileName};
  if (!mmap)
    report_fatal_error("failed to open file: {}", fileName);
  parser.parse(mmap, fileName);
  dumpCtx(ctx);

  MetaContext metaCtx;
  MetaParser metaParser{metaCtx};
  auto flowBlock = metaCtx.getCFG().blocks.create(metaCtx.getCFG());
  auto flowFileName = std::string(*argFlowFileName);
  MMap flowFile{flowFileName};
  if (!flowFile)
    report_fatal_error("failed to open file: {}", flowFileName);
  metaParser.parse(flowFile, flowFileName, flowBlock.end());

  SmallVec<void *, 1> passCtorArgs{reinterpret_cast<void *>(&ctx)};
  MetaPassPipelineInterpreter pipeline{*parser.lexer, passCtorArgs};

  pipeline.interpretPassPipeline(flowBlock);

  std::ofstream str{argOutFile->data()};
  HWPrinter{str}.printCtx(ctx);
}
