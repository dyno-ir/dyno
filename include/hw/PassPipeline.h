#pragma once

#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/IDs.h"
#include "hw/passes/ABC.h"
#include "hw/passes/AIGConstruct.h"
#include "hw/passes/AggressiveDeadCodeElimination.h"
#include "hw/passes/CheckPass.h"
#include "hw/passes/CommonSubexpressionElimination.h"
#include "hw/passes/ConstantMapping.h"
#include "hw/passes/DumpVerilog.h"
#include "hw/passes/EarlySharePass.h"
#include "hw/passes/FindLongestPath.h"
#include "hw/passes/FlipFlopInference.h"
#include "hw/passes/FlipFlopMapping.h"
#include "hw/passes/FunctionInline.h"
#include "hw/passes/FuzzyCSE.h"
#include "hw/passes/InstCombine.h"
#include "hw/passes/LinearizeControlFlow.h"
#include "hw/passes/LoopSimplify.h"
#include "hw/passes/LowerOps.h"
#include "hw/passes/ModuleInline.h"
#include "hw/passes/MuxTreeOptimization.h"
#include "hw/passes/OrderInstrs.h"
#include "hw/passes/ParseLiberty.h"
#include "hw/passes/ProcessLinearize.h"
#include "hw/passes/RegisterPartition.h"
#include "hw/passes/RemoveBuffers.h"
#include "hw/passes/SSAConstruct.h"
#include "hw/passes/SeqToComb.h"
#include "hw/passes/SimpleMemoryMapping.h"
#include "hw/passes/TriggerDedupe.h"
#include "support/Debug.h"

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
  LinearizeControlFlowPass linearizeControlFlow{ctx, instCombine};
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
  OrderInstrsPass orderInstrs{ctx};
  ConstantMapping constMap{ctx};
  FindLongestPathPass longestPath{ctx};
  CheckPass checkPass{ctx};
  RegisterPartitionPass regPartition{ctx};
  FuzzyCSEPass fuzzyCse{ctx};
  EarlySharePass earlyShare{ctx};
  SimpleMemoryMappingPass simpleMemMap{ctx};

public:
  bool printAfterAll = true;
  bool checkAfterAll = true;
  bool dumpAfterAll = false;

  template <typename T> void runPass(T &pass, bool skipCheck = false) {
    pass.run();
    if (printAfterAll) {
      std::print(std::cerr, "\n\nIR after {}:\n", __PRETTY_FUNCTION__);
      HWPrinter{std::cerr}.printCtx(ctx);
    }
    if (dumpAfterAll)
      dumpDyno();
    if (checkAfterAll && !skipCheck)
      checkPass.run();
  }

  void runOptPipeline() {
    // std::print(std::cerr, "\n\nInitial IR:\n");
    // HWPrinter{std::cerr}.printCtx(ctx);
    {
      std::ofstream ostr("initial.dyno");
      dumpDyno(ostr);
    }

    runPass(funcInline);
    runPass(instCombine);
    runPass(moduleInline);
    runPass(triggerDedupe);
    runPass(seqToComb);
    ssaConstr.config.mode = SSAConstructPass::Config::IMMEDIATE;
    runPass(ssaConstr);
    runPass(processLinearize);
    runPass(ssaConstr);
    ssaConstr.config.mode = SSAConstructPass::Config::DEFERRED;
    runPass(ssaConstr);

    runPass(instCombine);
    runPass(loopSimplify);
    runPass(agressiveDCE);
    runPass(cse, true);
    orderInstrs.config.assertNoCircularDeps = true;
    orderInstrs.config.moveStoresBeforeLoads = false;
    runPass(orderInstrs);
    runPass(instCombine);
    runPass(agressiveDCE);

    runPass(instCombine);
  }

  void runLibertyPipeline() {
    SmallVec<ModuleRef, 4> original;
    for (auto mod : ctx.activeModules()) {
      original.emplace_back(mod);
      mod->ignore = true;
    }

    auto old = std::pair(printAfterAll, debugType);
    printAfterAll = false;
    debugType = 0;
    runPass(parseLiberty);
    runPass(agressiveDCE);
    runPass(cse, true);
    runPass(orderInstrs);
    runPass(instCombine);
    runPass(agressiveDCE);
    std::tie(printAfterAll, debugType) = old;

    // todo properly
    for (auto mod : ctx.activeModules())
      mod->ignore = 1;
    for (auto mod : original)
      mod->ignore = 0;
  }

  void runLoweringPipeline() {
    // fuse processes aggressively and unroll loops
    linearizeControlFlow.config.flattenLoops = 1;
    linearizeControlFlow.config.flattenMultiway = 0;
    checkPass.config.noLoops = true;
    runPass(linearizeControlFlow);
    // processLinearize.config.retainInnerDeps = 0;
    // processLinearize.config.retainIODeps = 0;
    // runPass(processLinearize);
    // ssaConstr.config.mode = SSAConstructPass::Config::IMMEDIATE;
    // runPass(ssaConstr);
    runPass(instCombine);
    runPass(cse, true);

    orderInstrs.config.assertNoCircularDeps = true;
    orderInstrs.config.moveStoresBeforeLoads = false;
    runPass(orderInstrs);
    runPass(instCombine);
    runPass(agressiveDCE);

    // lower subtract and ordering compares for CSE/share
    lowerOps.config = LowerOpsPass::Config{
        .lowerMultiInputAdd = false,
        .lowerAddCompress = false,
        .lowerSimpleAdd = false,
        .lowerSub = true,
        .lowerConstantMul = true,
        .lowerMultiInputBitwise = false,
        .lowerEqualityICMP = false,
        .lowerOrderingICMP = true,
        .lowerShift = false,
        .lowerInsert = false,
        .lowerExtract = false,
    };

    runPass(lowerOps);
    runPass(instCombine);
    runPass(cse, true);
    runPass(fuzzyCse, true);
    runPass(orderInstrs);
    runPass(instCombine);

    earlyShare.config.opToShare = OP_ADD;
    runPass(earlyShare);
    earlyShare.config.opToShare = HW_SPLICE;
    runPass(earlyShare);

    runPass(instCombine);
    ssaConstr.config.mode = SSAConstructPass::Config::IMMEDIATE;
    runPass(ssaConstr);
    runPass(instCombine);
    runPass(agressiveDCE);

    processLinearize.config.retainInnerDeps = 0;
    processLinearize.config.retainIODeps = 0;
    runPass(processLinearize);
    linearizeControlFlow.config.flattenLoops = 1;
    linearizeControlFlow.config.flattenMultiway = 1;
    runPass(linearizeControlFlow);
    ssaConstr.config.mode = SSAConstructPass::Config::IMMEDIATE;
    runPass(ssaConstr);
    runPass(instCombine);
    runPass(agressiveDCE);

    runPass(instCombine);
    runPass(cse, true);
    runPass(orderInstrs);
    runPass(instCombine);

    // runPass(simpleMemMap);

    // dumpDyno("a.dyno");
    runPass(muxTreeOpt);
    runPass(instCombine);
    // dumpDyno("b.dyno");

    runPass(cse, true);
    runPass(fuzzyCse, true);
    orderInstrs.config.assertNoCircularDeps = true;
    orderInstrs.config.moveStoresBeforeLoads = true;
    runPass(orderInstrs);
    runPass(instCombine);
    runPass(ssaConstr);
    runPass(instCombine);

    // lower everything that can still go through regular instcombine
    lowerOps.config = LowerOpsPass::Config{
        .lowerMultiInputAdd = true,
        .lowerAddCompress = false,
        .lowerSimpleAdd = false,
        .lowerSub = true,
        .lowerConstantMul = true,
        .lowerMultiInputBitwise = false,
        .lowerEqualityICMP = true,
        .lowerOrderingICMP = true,
        .lowerShift = true,
        .lowerInsert = true,
        .lowerExtract = true,
    };
    runPass(lowerOps);

    // lift MUXs for reg partition
    instCombine.config.liftMUX = true;
    runPass(instCombine);
    runPass(regPartition);
    runPass(instCombine);
    runPass(processLinearize);
    runPass(instCombine);
    runPass(ssaConstr);
    runPass(instCombine);
    runPass(agressiveDCE);

    runPass(flipFlopInference);
    runPass(cse, true);
    runPass(orderInstrs);
    runPass(instCombine);
    // re-run mux tree opt to remove loopback MUXs after ff inference
    runPass(muxTreeOpt);
    runPass(instCombine);

    runLibertyPipeline();

    runPass(ffMap, true);
    runPass(instCombine);
    runPass(processLinearize);
    runPass(orderInstrs);
    runPass(instCombine);
    runPass(cse, true);
    runPass(orderInstrs);
    runPass(ssaConstr);
    runPass(instCombine);
    runPass(agressiveDCE);

    // lower the rest
    lowerOps.config = LowerOpsPass::Config{
        .lowerMultiInputAdd = true,
        .lowerAddCompress = true,
        .lowerSimpleAdd = true,
        .lowerSub = true,
        .lowerConstantMul = true,
        .lowerMultiInputBitwise = true,
        .lowerEqualityICMP = true,
        .lowerOrderingICMP = true,
        .lowerShift = true,
        .lowerInsert = true,
        .lowerExtract = true,
    };
    runPass(lowerOps);
    instCombine.config.fuseCommutative = false;
    runPass(instCombine);
    runPass(agressiveDCE);

    runPass(aigConstr);
    runPass(agressiveDCE);
    runPass(abc);
    runPass(agressiveDCE);
    runPass(removeBufs);

    runPass(cse);
    runPass(instCombine);
    runPass(agressiveDCE);

    runPass(constMap);
    runPass(cse);
    runPass(instCombine);
    runPass(agressiveDCE);

    orderInstrs.config.assertNoCircularDeps = false;
    runPass(orderInstrs);
    runPass(longestPath);
  }

  void dumpVerilog(std::ostream &os) {
    DumpVerilogPass dumpVerilog{ctx, os};
    dumpVerilog.run();
  }

  void dumpDyno(std::ostream &os) {
    HWPrinter print{os};
    print.printCtx(ctx);
  }

  void dumpDyno(std::string path) {
    std::ofstream str{path};
    dumpDyno(str);
  }
  __attribute__((used)) void dumpDyno() { dumpDyno("dump_unnamed.dyno"); }

public:
  explicit PassPipeline(HWContext &ctx) : ctx(ctx) {}
};

}; // namespace dyno
