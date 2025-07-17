#pragma once

#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/passes/ABC.h"
#include "hw/passes/AIGConstruct.h"
#include "hw/passes/AggressiveDeadCodeElimination.h"
#include "hw/passes/FunctionInline.h"
#include "hw/passes/InstCombine.h"
#include "hw/passes/LinearizeControlFlow.h"
#include "hw/passes/LoopSimplify.h"
#include "hw/passes/LowerOps.h"
#include "hw/passes/ModuleInline.h"
#include "hw/passes/ProcessLinearize.h"
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

    runPass(linearizeControlFlow);
    runPass(instCombine);
    runPass(agressiveDCE);
  }

  void runLoweringPipeline() {
    // lower compares and sub and re-run instcombine
    lowerOps.config = LowerOpsPass::Config{
        .lowerMultiInputAdd = false,
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
        .lowerSimpleAdd = false,
        .lowerSub = true,
        .lowerMultiInputBitwise = false,
        .lowerEqualityICMP = true,
        .lowerOrderingICMP = true,
        .lowerShift = true,
    };
    runPass(lowerOps);
    runPass(instCombine);

    // lower the rest without re-running instcombine.
    lowerOps.config = LowerOpsPass::Config{
        .lowerMultiInputAdd = true,
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
    runPass(abc);
  }

public:
  explicit PassPipeline(HWContext &ctx) : ctx(ctx) {}
};

}; // namespace dyno
