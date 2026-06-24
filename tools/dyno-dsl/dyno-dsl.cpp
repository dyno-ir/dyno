#include "dyno/InstrPrinter.h"
#include "meta/PrintParse.h"
#include "op/PrintParse.h"
#include "support/CmdLineArgs.h"
#include "support/ErrorRecovery.h"
#include "support/MMap.h"
#include <dsl/DSLContext.h>
#include <dsl/DSLParser.h>
#include <dsl/PrintParse.h>
#include <meta/MetaContext.h>
#include <op/OpContext.h>

using namespace dyno;

CmdLineArg<std::string_view> argFileName{
    std::nullopt, "input file", "Input Dyno-DSL file path.",
    CmdLineArgFlags::POSITIONAL | CmdLineArgFlags::MANDATORY};

class DSLPrinter
    : public ContextPrinterWrapper<CoreDialectPrinter, OpDialectPrinter,
                                   DSLDialectPrinter, MetaDialectPrinter> {
public:
  DSLPrinter(Context &ctx, std::ostream &os) : ContextPrinterWrapper(ctx, os) {}
};

int main(int argc, char **argv) {
  Context ctx;
  CoreDialectContext coreContext;
  OpDialectContext opContext;
  DSLDialectContext dslContext;
  MetaDialectContext metaContext;
  ctx.registerDialect(coreContext);
  ctx.registerDialect(opContext);
  ctx.registerDialect(dslContext);
  ctx.registerDialect(metaContext);

  CmdLineArgHandler cmdLineArgHandler;
  cmdLineArgHandler.registerArg(argFileName);
  cmdLineArgHandler.parse(argc, argv);

  std::string fileName{*argFileName};
  MMap mmap{fileName};
  if (!mmap)
    report_fatal_error("failed to open file: {}", fileName);

  DSLLexer lex(mmap, std::move(fileName));
  DSLParser parser(ctx, lex);
  auto res = parser.parse();
  if (!res) {
    lex.printError(res.error());
    report_fatal_error("parser error: ", fileName);
  }

  DSLPrinter printer(ctx, std::cout);
  printer.printBlock(res->as<BlockRef>());
}
