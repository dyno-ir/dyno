#include "hw/HWContext.h"
#include "hw/HWParser.h"
#include "hw/PassPipeline.h"
#include "support/CmdLineArgs.h"
#include "support/ErrorRecovery.h"
#include "support/MMap.h"
#include <string>
using namespace dyno;

CmdLineArg<std::string_view> argFileName{
    std::nullopt, "input file", "Input Dyno-IR file path.",
    CmdLineArgFlags::POSITIONAL | CmdLineArgFlags::MANDATORY};
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

  CmdLineArgHandler cmdLineArgHandler;
  cmdLineArgHandler.registerArg(argFileName);
  cmdLineArgHandler.registerArg(argOutFile);
  cmdLineArgHandler.registerArg(argLibertyFile);
  cmdLineArgHandler.registerArg(argOptPipeline);
  cmdLineArgHandler.registerArg(argLowerPipeline);
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
  parser.parse(mmap, std::move(fileName));

  PassPipeline pipeline{ctx};
  pipeline.dumpAfterAll = *argDumpAfterAll;
  pipeline.printAfterAll = *argPrintAfterAll;
  pipeline.checkAfterAll = *argCheckAfterAll;
  pipeline.setLibertyPath(std::string(*argLibertyFile));

  if (*argLowerPipeline)
    pipeline.runLoweringPipeline();
  if (*argOptPipeline)
    pipeline.runOptPipeline();

  pipeline.dumpDyno(std::string(*argOutFile));
}