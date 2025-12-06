#include "hw/HWContext.h"
#include "hw/HWParser.h"
#include "hw/HWPrinter.h"
#include "support/CmdLineArgs.h"
#include "support/ErrorRecovery.h"
#include "support/MMap.h"
#include <string>
using namespace dyno;

CmdLineArg<std::string_view> argFileName{
    std::nullopt, "input file", "Input Dyno-IR file path.",
    CmdLineArgFlags::POSITIONAL | CmdLineArgFlags::MANDATORY};
CmdLineArg<bool> argDebugMode{'d', "debug", "Enable debug mode.", 0, false};

int main(int argc, char **argv) {

  CmdLineArgHandler cmdLineArgHandler;
  cmdLineArgHandler.registerArg(argFileName);
  cmdLineArgHandler.registerArg(argDebugMode);
  cmdLineArgHandler.parse(argc, argv);

  HWContext ctx;
  HWParser parser{ctx};
  std::string fileName{*argFileName};
  MMap mmap{fileName};
  if (!mmap)
    report_fatal_error("failed to open file: {}", fileName);
  parser.parse(mmap, std::move(fileName));

  // todo
  dumpCtx(ctx);
}