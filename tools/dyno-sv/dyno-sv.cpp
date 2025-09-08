#include "Frontend.h"
#include "hw/PassPipeline.h"
#include "slang/driver/Driver.h"

using namespace dyno;

int main(int argc, char **argv) {
  slang::driver::Driver driver;
  driver.addStandardArgs();
  if (!driver.parseCommandLine(argc, argv))
    return 1;

  if (!driver.processOptions())
    return 1;

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

  HWContext ctx;

  VisitorAST visitor{ctx, driver.sourceManager};
  compilation->getRoot().visit(visitor);
  visitor.handle_modules();

  std::cout << "\n\n\n";

  PassPipeline pipeline{ctx};
  pipeline.printAfterAll = true;
  pipeline.checkAfterAll = true;
  pipeline.dumpAfterAll = false;
  debugType = 1;
  pipeline.runOptPipeline();
  pipeline.runLoweringPipeline();

  std::ofstream of{"dump.dyno"};
  pipeline.dumpDyno(of);

  std::ofstream ofV{"dump.v"};
  pipeline.dumpVerilog(ofV);
}
