#include "dsl/DSLPrinter.h"
#include "support/CmdLineArgs.h"
#include "support/ErrorRecovery.h"
#include "support/MMap.h"
#include <algorithm>
#include <dsl/DSLContext.h>
#include <dsl/DSLParser.h>
#include <dsl/passes/Elaborate.h>
#include <meta/MetaContext.h>
#include <op/OpContext.h>
#include <sstream>

using namespace dyno;

CmdLineArg<std::string_view> argFileName{
    std::nullopt, "input file", "Input Dyno-DSL file path.",
    CmdLineArgFlags::POSITIONAL | CmdLineArgFlags::MANDATORY};

CmdLineArg<bool> argParseOnly{std::nullopt, "parse",
                              "Print parser IR without type elaboration.",
                              CmdLineArgFlags::NO_VALUE};

static void printElabError(Context &ctx, DSLLexer &lex,
                           const ElabError &error) {
  std::string message;
  switch (error.kind) {
  case ElabError::Kind::UnknownName:
    message = "unknown name";
    break;
  case ElabError::Kind::DuplicateName:
    message = "duplicate definition";
    break;
  case ElabError::Kind::InvalidBinding:
    message = "invalid binding or parameter";
    break;
  case ElabError::Kind::TypeMismatch:
    message = "type mismatch";
    break;
  case ElabError::Kind::InfiniteType:
    message = "infinite structural type";
    break;
  case ElabError::Kind::NotCallable:
    message = "value is not callable";
    break;
  case ElabError::Kind::UnsupportedOperator:
    message = "unsupported operator operand types";
    break;
  case ElabError::Kind::UnresolvedType:
    message = "unresolved type (polymorphic schemes are not implemented yet)";
    break;
  case ElabError::Kind::UnresolvedWork:
    message =
        "unresolved requirements (generic constraints are not implemented yet)";
    break;
  case ElabError::Kind::GeneralizedVariable:
    message = "cannot unify a generalized variable; instantiate it first";
    break;
  case ElabError::Kind::InvalidType:
    message = "invalid DSL type";
    break;
  case ElabError::Kind::UnsupportedInstruction:
    message = "construct is not supported by type elaboration yet";
    break;
  }
  if (error.member)
    message += ": " + error.member->data;
  if (error.lhs) {
    std::ostringstream types;
    DSLPrinter printer(ctx, types);
    printer.printRefOrUse(error.lhs);
    if (error.rhs) {
      types << " vs ";
      printer.printRefOrUse(error.rhs);
    }
    message += ": " + types.str();
  }
  if (error.origin) {
    auto locations =
        ctx.getCtx<CoreDialectContext>().instrSourceLocInfo.getSourceLocs(
            error.origin);
    for (auto loc : locations) {
      size_t start = 0;
      unsigned line = 1;
      while (start < lex.src.size() && line < loc.beginLine)
        if (lex.src[start++] == '\n')
          ++line;
      start = std::min(lex.src.size(), start + loc.beginCol - 1);
      size_t end =
          loc.beginLine == loc.endLine
              ? std::min(lex.src.size(), start + loc.endCol - loc.beginCol)
              : start;
      lex.printError(
          ParseError{Format{"{}", message}, start, end, loc.beginLine});
      return;
    }
  }
  std::cerr << lex.path << ": error: " << message << '\n';
}

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
  cmdLineArgHandler.registerArg(argParseOnly);
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
    return 1;
  }

  if (!*argParseOnly) {
    ElaboratePass elaborate(ctx);
    auto result = elaborate.run(res->as<BlockRef>());
    if (!result) {
      printElabError(ctx, lex, result.error());
      return 1;
    }
  }

  DSLPrinter printer(ctx, std::cout);
  printer.printBlock(res->as<BlockRef>());
}
