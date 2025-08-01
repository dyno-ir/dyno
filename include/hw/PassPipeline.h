#pragma once

#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/passes/ABC.h"
#include "hw/passes/AIGConstruct.h"
#include "hw/passes/AggressiveDeadCodeElimination.h"
#include "hw/passes/CommonSubexpressionElimination.h"
#include "hw/passes/DumpVerilog.h"
#include "hw/passes/FlipFlopInference.h"
#include "hw/passes/FlipFlopMapping.h"
#include "hw/passes/FunctionInline.h"
#include "hw/passes/InstCombine.h"
#include "hw/passes/LinearizeControlFlow.h"
#include "hw/passes/LoopSimplify.h"
#include "hw/passes/LowerOps.h"
#include "hw/passes/ModuleInline.h"
#include "hw/passes/MuxTreeOptimization.h"
#include "hw/passes/ParseLiberty.h"
#include "hw/passes/ProcessLinearize.h"
#include "hw/passes/RemoveBuffers.h"
#include "hw/passes/SSAConstruct.h"
#include "hw/passes/SeqToComb.h"
#include "hw/passes/TriggerDedupe.h"

namespace dyno {

class PassPipeline {
  HWContext &ctx;

  FunctionInlinePass funcInline{ctx};
  TriggerDedupePass triggerDedupe{ctx};
  SeqToCombPass seqToComb{ctx};
  SSAConstructPass ssaConstr{ctx};
  ProcessLinearizePass processLinearize{ctx};
  InstCombinePass instCombine{ctx};
  ModuleInlinePass moduleInline{ctx};
  LoopSimplifyPass loopSimplify{ctx};
  LinearizeControlFlowPass linearizeControlFlow{ctx};
  AggressiveDeadCodeEliminationPass agressiveDCE{ctx};
  LowerOpsPass lowerOps{ctx};
  AIGConstructPass aigConstr{ctx};
  ABCPass abc{ctx};
  ParseLibertyPass parseLiberty{ctx};
  FlipFlopInferencePass flipFlopInference{ctx};
  MuxTreeOptimizationPass muxTreeOpt{ctx};
  CommonSubexpressionEliminationPass cse{ctx};
  FlipFlopMappingPass ffMap{ctx};
  RemoveBuffersPass removeBufs{ctx};

public:
  bool printAfterAll = false;

  template <typename T> void runPass(T &pass) {
    pass.run();
    if (printAfterAll) {
      std::print(std::cerr, "\n\nIR after {}:\n", __PRETTY_FUNCTION__);
      HWPrinter{std::cerr}.printCtx(ctx);
    }
  }

  void runOptPipeline() {
    std::print(std::cerr, "\n\nInitial IR:\n");
    HWPrinter{std::cerr}.printCtx(ctx);

    runPass(funcInline);
    runPass(moduleInline);
    runPass(triggerDedupe);
    runPass(seqToComb);
    runPass(ssaConstr);
    runPass(processLinearize);
    runPass(ssaConstr);
    ssaConstr.config.mode = SSAConstructPass::Config::DEFERRED;
    runPass(ssaConstr);

    runPass(instCombine);
    runPass(loopSimplify);
    runPass(agressiveDCE);
    runPass(cse);
  }

  void runLibertyPipeline() {
    SmallVec<ModuleRef, 4> original;
    for (auto mod : ctx.activeModules()) {
      original.emplace_back(mod);
      mod->ignore = true;
    }

    auto old = printAfterAll;
    printAfterAll = true;
    runPass(parseLiberty);
    runPass(agressiveDCE);
    runPass(cse);
    runPass(instCombine);
    runPass(agressiveDCE);
    printAfterAll = old;

    // todo properly
    for (auto mod : ctx.activeModules())
      mod->ignore = 1;
    for (auto mod : original)
      mod->ignore = 0;
  }

  void runLoweringPipeline() {

    processLinearize.config.retainInnerDeps = 0;
    processLinearize.config.retainIODeps = 0;
    runPass(processLinearize);
    runPass(linearizeControlFlow);
    ssaConstr.config.mode = SSAConstructPass::Config::IMMEDIATE;
    runPass(ssaConstr);
    runPass(instCombine);
    runPass(agressiveDCE);

    runPass(flipFlopInference);
    runPass(cse);
    runPass(muxTreeOpt);
    runPass(agressiveDCE);

    // lower compares and sub and re-run instcombine
    lowerOps.config = LowerOpsPass::Config{
        .lowerMultiInputAdd = false,
        .lowerAddCompress = false,
        .lowerSimpleAdd = false,
        .lowerSub = true,
        .lowerMultiInputBitwise = false,
        .lowerEqualityICMP = false,
        .lowerOrderingICMP = true,
        .lowerShift = false,
    };
    runPass(lowerOps);
    runPass(instCombine);

    // lower everything that can still go through instcombine
    lowerOps.config = LowerOpsPass::Config{
        .lowerMultiInputAdd = true,
        .lowerAddCompress = false,
        .lowerSimpleAdd = false,
        .lowerSub = true,
        .lowerMultiInputBitwise = false,
        .lowerEqualityICMP = true,
        .lowerOrderingICMP = true,
        .lowerShift = true,
    };
    runPass(lowerOps);
    runPass(instCombine);

    runLibertyPipeline();
    runPass(ffMap);
    runPass(ssaConstr);
    runPass(instCombine);
    runPass(agressiveDCE);

    // lower the rest without re-running instcombine.
    lowerOps.config = LowerOpsPass::Config{
        .lowerMultiInputAdd = true,
        .lowerAddCompress = true,
        .lowerSimpleAdd = true,
        .lowerSub = true,
        .lowerMultiInputBitwise = true,
        .lowerEqualityICMP = true,
        .lowerOrderingICMP = true,
        .lowerShift = true,
    };
    runPass(lowerOps);
    runPass(agressiveDCE);

    runPass(aigConstr);
    runPass(agressiveDCE);
    runPass(abc);
    runPass(instCombine);
    runPass(agressiveDCE);
    runPass(removeBufs);
  }

  void dumpVerilog(std::ostream &os) {
    DumpVerilogPass dumpVerilog{ctx, os};
    dumpVerilog.run();
  }

public:
  explicit PassPipeline(HWContext &ctx) : ctx(ctx) {}
};

}; // namespace dyno
