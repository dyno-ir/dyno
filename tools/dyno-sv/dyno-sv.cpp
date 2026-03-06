#include "Frontend.h"
#include "hw/HWPrinter.h"
#include "hw/PassPipeline.h"
#include "hw/passes/DumpVerilog.h"
#include "hw/passes/HWDialectPasses.h"
#include "meta/IDs.h"
#include "meta/MetaContext.h"
#include "meta/MetaParser.h"
#include "meta/PassPipelineInterpreter.h"
#include "slang/driver/Driver.h"
#include <optional>

using namespace dyno;

int main(int argc, char **argv) {
  slang::driver::Driver driver;
  driver.addStandardArgs();

  std::optional<std::string> libertyFile = std::nullopt;
  driver.cmdLine.add("--liberty", libertyFile,
                     "Liberty file (stdcell definitions)");

  std::optional<bool> parseOnly = std::nullopt;
  driver.cmdLine.add("--parseOnly", parseOnly, "Parse only.");

  std::optional<std::string> flow = std::nullopt;
  driver.cmdLine.add("--flow", flow,
                     "Dyno-IR flow script file name. If not specified falls "
                     "back to default flow.");

  std::optional<std::string> outfile = "dump.v";
  driver.cmdLine.add("-o", outfile, "Output verilog netlist.");

  if (!driver.parseCommandLine(argc, argv))
    return 1;

  if (!driver.processOptions())
    return 1;

  if (!libertyFile)
    libertyFile = "sky130_fd_sc_hd__tt_025C_1v80.lib";

  std::unique_ptr<slang::ast::Compilation> compilation;

  bool compilation_ok;
  compilation_ok = driver.parseAllSources();
  compilation = driver.createCompilation();
  driver.reportCompilation(*compilation, false);
  auto diag = compilation->getSemanticDiagnostics();

  if (!compilation_ok) {
    fprintf(stderr, "slang (dyno-sv): errors found during compilation\n");
    return 1;
  }

  if (!driver.reportDiagnostics(true))
    return 1;

  Context ctx;
  HWDialectContext hwContext;
  CoreDialectContext coreContext;
  OpDialectContext opContext;
  AIGDialectContext aigContext;
  MetaDialectContext metaContext;
  ctx.registerDialect(coreContext);
  ctx.registerDialect(hwContext);
  ctx.registerDialect(opContext);
  ctx.registerDialect(aigContext);
  ctx.registerDialect(metaContext);

  VisitorAST visitor{ctx, driver.sourceManager};
  compilation->getRoot().visit(visitor);
  visitor.handle_modules();

  std::cout << "\n\n\n";

  if (!flow) {
    PassPipeline pipeline{ctx};
    pipeline.setLibertyPath(*libertyFile);
    pipeline.printAfterAll = false;
    pipeline.checkAfterAll = true;
    pipeline.dumpAfterAll = true;
    debugType = 1;

    if (!parseOnly || !*parseOnly) {
      pipeline.runOptPipeline();
      pipeline.runLoweringPipeline();
      pipeline.dumpVerilog(*outfile);
    }
    std::ofstream of{"dump.dyno"};
    pipeline.dumpDyno(of);

  } else {
    auto flowBlock = ctx.getCFG().blocks.create(ctx.getCFG());
    auto &flowFileName = *flow;
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
    dumpVerilog.config.fileName = *outfile;
    dumpVerilog.run();
  }
}
