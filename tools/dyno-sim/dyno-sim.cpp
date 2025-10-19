
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/passes/ParseDyno.h"

using namespace dyno;

int main(int argc, char **argv) {
  HWContext ctx;

  if (argc != 2) {
    fprintf(stderr, "usage: %s <dyno file>\n", argv[0]);
  }

  ParseDynoPass parse{ctx};
  parse.config.fileName = std::string(argv[1]);

  parse.run();

  HWPrinter print{std::cout};
  print.printCtx(ctx);
}