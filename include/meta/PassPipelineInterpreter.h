#pragma once

#include "dyno/Context.h"
#include "dyno/InstrPrinter.h"
#include "meta/PrintParse.h"
#include "op/Function.h"
#include "op/IDs.h"
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

  bool interpretPassPipelineImpl(Context &ctx, BlockRef block,
                                 ArrayRef<void *> passRunArgs) {
    for (auto instr : block) {
      if (instr.isOpc(OP_FUNCTION_DEF, CORE_EXPORT))
        continue;
      if (instr.isOpc(OP_CALL)) {
        auto asCall = instr.as<CallInstrRef>();
        if (!interpretPassPipelineImpl(*asCall.func()->defContext,
                                       asCall.func().iref().getBlock(),
                                       passRunArgs))
          return false;
        continue;
      }
      if (instr.getDialect() != DIALECT_META)
        report_fatal_error("expected meta dialect instruction");

      auto errorCB = [&]() {
        auto locs =
            ctx.getCtx<CoreDialectContext>().instrSourceLocInfo.getSourceLocs(
                instr);
        if (!locs.empty())
          std::print(std::cerr, "{}: ", locs.front());
        std::print(
            std::cerr, "note: in pass {}\n",
            ContextPrinterWrapper<CoreDialectPrinter, OpDialectPrinter,
                                  MetaDialectPrinter>{ctx, OStreamWrapper{}}
                .toString(instr));
      };
      push_fatal_error_callback(errorCB);

      auto opc = instr.getDialectOpcode();
      auto &pass = passes.findOrCreate(opc, passCtorArgs);

      FatObjRef<MapObj> cfg = nullref;
      if (instr.getNumOperands() != 0) {
        if (instr.getNumOperands() != 1)
          report_fatal_error("expected at most one operand (config)");
        cfg = instr.operand(0)->dyn_as<MapRef>();
        if (!cfg)
          report_fatal_error("expected map object");
      }
      DynoLexer lexer{ctx.getDialectInfos(), ArrayRef<char>::emptyRef(),
                      "<internal>"};
      std::map<std::string, std::string> empty{};
      pass.config(cfg ? cfg->data : empty, lexer);

      bool res = pass.run(passRunArgs);
      pop_fatal_error_callback();
      if (!res) {
        std::print(
            dbgs(), "failed to run pass: ",
            ContextPrinterWrapper<CoreDialectPrinter, OpDialectPrinter,
                                  MetaDialectPrinter>{ctx, OStreamWrapper{}}
                .toString(instr));
        return false;
      }
    }
    return true;
  }

public:
  bool interpretPassPipeline(BlockRef block, ArrayRef<void *> passRunArgs) {
    return interpretPassPipelineImpl(ctx, block, passRunArgs);
  }

  MetaPassPipelineInterpreter(Context &ctx, ArrayRef<void *> passCtorArgs)
      : ctx(ctx), passCtorArgs(passCtorArgs), passes(ctx) {}
};

}; // namespace dyno
