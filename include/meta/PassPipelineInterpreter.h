#pragma once

#include "dyno/Context.h"
#include "dyno/InstrPrinter.h"
#include "meta/PrintParse.h"
#include "op/MapObj.h"
#include "op/PrintParse.h"
#include "support/ErrorRecovery.h"

namespace dyno {

class PassStorage {
  Context &ctx;
  VectorLUT<TypeErasedPassObj> passes;

public:
  TypeErasedPassObj &create(DialectOpcode passOpc, ArrayRef<void *> args) {
    assert(passOpc.getDialectID() == DIALECT_META);
    return passes.insert(passOpc.getOpcodeID(),
                         ctx.getPassRegistry().constructPass(passOpc, args));
  }
  TypeErasedPassObj &findOrCreate(DialectOpcode passOpc,
                                  ArrayRef<void *> args) {
    assert(passOpc.getDialectID() == DIALECT_META);
    if (auto &pass = passes.find(passOpc.getOpcodeID()))
      return *pass;
    return create(passOpc, args);
  }

  TypeErasedPassObj &insertExisting(DialectOpcode passOpc,
                                    TypeErasedPassObj &&existing) {
    return passes.insert(passOpc.getDialectID(), std::move(existing));
  }

  PassStorage(Context &ctx) : ctx(ctx) {}
};

class MetaPassPipelineInterpreter {
  Context &ctx;
  ArrayRef<void *> passCtorArgs;
  PassStorage passes;

public:
  void interpretPassPipeline(BlockRef block, ArrayRef<void *> passRunArgs) {
    for (auto instr : block) {
      if (instr.getDialect() != DIALECT_META)
        report_fatal_error("expected meta dialect instruction");
      auto opc = instr.getDialectOpcode();
      auto &pass = passes.findOrCreate(opc, passCtorArgs);

      if (instr.getNumOperands() != 0) {
        if (instr.getNumOperands() != 1)
          report_fatal_error("expected at most one operand (config)");
        auto cfg = instr.operand(0)->dyn_as<MapRef>();
        if (!cfg)
          report_fatal_error("expected map object");
        DynoLexer lexer{ctx.getDialectInfos(), ArrayRef<char>::emptyRef(),
                        "<internal>"};
        pass.config(cfg->data, lexer);
      }

      if (!pass.run(passRunArgs))
        report_fatal_error("failed to run pass: ",
                           PrinterWrapper<CoreDialectPrinter, OpDialectPrinter,
                                          MetaDialectPrinter>{OStreamWrapper{}}
                               .toString(instr));
    }
  }

  MetaPassPipelineInterpreter(Context &ctx, ArrayRef<void *> passCtorArgs)
      : ctx(ctx), passCtorArgs(passCtorArgs), passes(ctx) {}
};

}; // namespace dyno
