#include "Frontend.h"
#include "hw/PassPipeline.h"
#include "slang/driver/Driver.h"

using namespace dyno;

int main(int argc, char **argv) {
  slang::driver::Driver driver;
  driver.addStandardArgs();

  std::optional<std::string> libertyFile = std::nullopt;
  driver.cmdLine.add("--liberty", libertyFile,
                     "Liberty file (stdcell definitions)");

  std::optional<bool> parseOnly = std::nullopt;
  driver.cmdLine.add("--parseOnly", parseOnly, "Parse only.");

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
    printf("dyno-sv: errors found during compilation\n");
    return 1;
  }

  if (!driver.reportDiagnostics(true))
    return 1;

  Context ctx;
  HWDialectContext hwContext;
  CoreDialectContext coreContext;
  OpDialectContext opContext;
  AIGDialectContext aigContext;
  ctx.registerDialect(coreContext);
  ctx.registerDialect(hwContext);
  ctx.registerDialect(opContext);
  ctx.registerDialect(aigContext);

  ctx.getStore<Instr>().destroyHooks.emplace_back(
      [&](InstrRef instr) { assert(!ctx.getCFG().contains(instr)); });

  VisitorAST visitor{ctx, driver.sourceManager};
  compilation->getRoot().visit(visitor);
  visitor.handle_modules();

  std::cout << "\n\n\n";

  PassPipeline pipeline{ctx};
  pipeline.setLibertyPath(*libertyFile);
  pipeline.printAfterAll = false;
  pipeline.checkAfterAll = true;
  pipeline.dumpAfterAll = true;
  debugType = 1;

  if (!parseOnly || !*parseOnly) {
    pipeline.runOptPipeline();
    pipeline.runLoweringPipeline();
    std::ofstream ofV{"dump.v"};
    pipeline.dumpVerilog(ofV);
  }

  std::ofstream of{"dump.dyno"};
  pipeline.dumpDyno(of);
}
