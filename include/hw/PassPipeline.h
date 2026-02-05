#pragma once

#include "dyno/Context.h"
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
#include "hw/passes/LoadCoalesce.h"
#include "hw/passes/LoopSimplify.h"
#include "hw/passes/LowerOps.h"
#include "hw/passes/ModuleInline.h"
#include "hw/passes/MuxTreeOptimization.h"
#include "hw/passes/OrderInstrs.h"
#include "hw/passes/ParseLiberty.h"
#include "hw/passes/ProcessLinearize.h"
#include "hw/passes/RegisterPartition.h"
#include "hw/passes/RemoveBuffers.h"
#include "hw/passes/RemoveInitProcs.h"
#include "hw/passes/SSAConstruct.h"
#include "hw/passes/SeqToComb.h"
#include "hw/passes/SimpleMemoryMapping.h"
#include "hw/passes/TriggerDedupe.h"
#include "support/Debug.h"

namespace dyno {

class PassPipeline {
  Context &ctx;

  FunctionInlinePass funcInline{ctx};
  TriggerDedupePass triggerDedupe{ctx};
  SeqToCombPass seqToComb{ctx};
  SSAConstructPass ssaConstr{ctx};
  ProcessLinearizePass processLinearize{ctx};
  InstCombinePass instCombine{ctx};
  ModuleInlinePass moduleInline{ctx};
  LoopSimplifyPass loopSimplify{ctx};
  LinearizeControlFlowPass linearizeControlFlow{ctx};
  AggressiveDeadCodeEliminationPass aggressiveDCE{ctx};
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
  ConstantMappingPass constMap{ctx};
  FindLongestPathPass longestPath{ctx};
  CheckPass checkPass{ctx};
  RegisterPartitionPass regPartition{ctx};
  FuzzyCSEPass fuzzyCse{ctx};
  EarlySharePass earlyShare{ctx};
  SimpleMemoryMappingPass simpleMemMap{ctx};
  LoadCoalescePass loadCoalesce{ctx};
  RemoveInitProcsPass removeInit{ctx};

public:
  bool printAfterAll = true;
  bool checkAfterAll = true;
  bool dumpAfterAll = false;
  int idx = 0;
  int32_t startIdx = 0;

  template <typename T> void runPass(T &pass, bool skipCheck = false) {
    if (idx >= startIdx) {
      pass.run();
      std::string_view name;
      if constexpr (requires { T::passName; })
        name = T::passName;
      else
        name = __PRETTY_FUNCTION__;
      if (printAfterAll) {

        std::print(std::cerr, "\n\nIR after {}:\n", name);
        HWPrinter{std::cerr}.printCtx(ctx);
      }
      if (dumpAfterAll)
        dumpDyno(std::string("dumps/_") + std::to_string(idx) +
                 std::string(name));
      // dumpDyno();
      if (checkAfterAll && !skipCheck)
        checkPass.run();
    }
    idx++;
  }

  void setLibertyPath(const std::string &path) {
    parseLiberty.config.path = path;
    abc.config.path = path;
  }

  void runOptPipeline() {
    // std::print(std::cerr, "\n\nInitial IR:\n");
    // HWPrinter{std::cerr}.printCtx(ctx);
    runPass(removeInit);
    runPass(funcInline);
    runPass(instCombine);
    runPass(moduleInline);
    runPass(triggerDedupe);

    cse.config.differentBlocks = false;
    runPass(cse);
    cse.config.differentBlocks = true;
    runPass(instCombine);

    runPass(seqToComb);
    ssaConstr.config.mode = SSAConstructPass::Config::IMMEDIATE;
    runPass(ssaConstr);
    processLinearize.config.retainInnerDeps = false;
    processLinearize.config.retainIODeps = false;
    runPass(processLinearize);
    runPass(ssaConstr);
    ssaConstr.config.mode = SSAConstructPass::Config::DEFERRED;
    runPass(ssaConstr);
    runPass(instCombine);

    runPass(loadCoalesce);
    runPass(instCombine);

    runPass(loopSimplify);
    runPass(aggressiveDCE);
    runPass(cse, true);
    orderInstrs.config.assertNoCircularDeps = true;
    orderInstrs.config.moveStoresBeforeLoads = false;
    runPass(orderInstrs);
    runPass(instCombine);
    runPass(aggressiveDCE);

    runPass(instCombine);
  }

  void runLibertyPipeline() {
    for (auto mod : ctx.getCtx<HWDialectContext>().activeModules()) {
      mod->ignore = true;
    }

    auto old = std::pair(printAfterAll, debugType);
    printAfterAll = false;
    debugType = 0;
    runPass(parseLiberty);
    runPass(aggressiveDCE);
    runPass(cse, true);
    runPass(orderInstrs);
    runPass(instCombine);
    runPass(aggressiveDCE);
    std::tie(printAfterAll, debugType) = old;

    // todo properly
    for (auto mod : ctx.getCtx<HWDialectContext>().modules)
      mod->ignore = !mod->ignore;
  }

  void runLoweringPipeline() {
    // fuse processes aggressively and unroll loops
    linearizeControlFlow.config.flattenLoops = 1;
    linearizeControlFlow.config.flattenMultiway = 0;
    checkPass.config.noLoops = true;
    // dumpDyno("a.dyno");
    runPass(linearizeControlFlow);
    // runPass(ssaConstr);
    // runPass(loadCoalesce);
    // runPass(instCombine);
    // runPass(cse, true);
    // orderInstrs.config.assertNoCircularDeps = true;
    // orderInstrs.config.moveStoresBeforeLoads = false;
    // runPass(orderInstrs);

    processLinearize.config.retainInnerDeps = 0;
    processLinearize.config.retainIODeps = 0;
    runPass(processLinearize);

    ssaConstr.config.mode = SSAConstructPass::Config::IMMEDIATE;
    runPass(ssaConstr);
    runPass(loadCoalesce);
    runPass(instCombine);
    runPass(cse, true);

    orderInstrs.config.assertNoCircularDeps = true;
    orderInstrs.config.moveStoresBeforeLoads = false;
    runPass(orderInstrs);
    runPass(instCombine);
    runPass(aggressiveDCE);

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
    earlyShare.config.opToShare = HW_INSERT;
    runPass(earlyShare);

    runPass(instCombine);
    ssaConstr.config.mode = SSAConstructPass::Config::IMMEDIATE;
    runPass(ssaConstr);
    runPass(instCombine);
    runPass(aggressiveDCE);

    processLinearize.config.retainInnerDeps = 0;
    processLinearize.config.retainIODeps = 0;
    runPass(processLinearize);
    orderInstrs.config.moveStoresBeforeLoads = true;
    runPass(orderInstrs);
    runPass(ssaConstr);
    runPass(loadCoalesce);
    runPass(instCombine);
    runPass(cse, true);
    runPass(orderInstrs);
    runPass(aggressiveDCE);

    linearizeControlFlow.config.flattenLoops = 1;
    linearizeControlFlow.config.flattenMultiway = 1;
    runPass(linearizeControlFlow);
    ssaConstr.config.mode = SSAConstructPass::Config::IMMEDIATE;
    runPass(ssaConstr);
    runPass(loadCoalesce);
    runPass(cse);
    runPass(instCombine);
    runPass(aggressiveDCE);

    runPass(instCombine);
    runPass(cse, true);
    runPass(fuzzyCse, true);
    orderInstrs.config.moveStoresBeforeLoads = true;
    runPass(orderInstrs);
    runPass(instCombine);
    runPass(ssaConstr);
    runPass(instCombine);
    runPass(cse);
    runPass(instCombine);
    runPass(aggressiveDCE);

    // muxTreeOpt.config.exploreConditions = true;
    // runPass(muxTreeOpt);
    runPass(cse);
    runPass(instCombine);

    instCombine.config.liftMUX = true;
    runPass(instCombine);
    runPass(cse);
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
    // dumpDyno("pre_lower.dyno");
    runPass(lowerOps);
    runPass(cse);

    // lift MUXs for reg partition
    instCombine.config.liftMUX = true;
    runPass(instCombine);
    runPass(cse);
    runPass(instCombine);
    runPass(regPartition);
    runPass(instCombine);

    runPass(processLinearize);
    runPass(instCombine);
    runPass(ssaConstr);
    runPass(instCombine);
    runPass(aggressiveDCE);

    // sink MUXs again for ff inference
    instCombine.config.liftMUX = false;
    runPass(instCombine);
    runPass(flipFlopInference);
    runPass(triggerDedupe);
    runPass(cse);
    runPass(instCombine);
    runPass(cse);
    runPass(instCombine);

    // re-run mux tree opt to remove loopback MUXs after ff inference
    // dumpDyno("pre_mux_opt.dyno");
    muxTreeOpt.config.dontCareMUXsOnly = true;
    muxTreeOpt.config.exploreConditions = false;
    runPass(muxTreeOpt);
    runPass(instCombine);
    // dumpDyno("postmux_opt.dyno");

    runLibertyPipeline();
    // todo: don't re-order after fmap or canonicalize cylic deps in
    // orderInstrs. otherwise we get a break at random point in the cycle.

    runPass(ffMap, true);
    runPass(instCombine);
    runPass(processLinearize);
    runPass(orderInstrs);
    runPass(instCombine);
    runPass(cse, true);
    runPass(orderInstrs);
    runPass(ssaConstr);
    runPass(instCombine);
    runPass(aggressiveDCE);

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
    runPass(aggressiveDCE);

    // dumpDyno("a.dyno");
    runPass(aigConstr);
    // dumpDyno("b.dyno");
    runPass(aggressiveDCE);
    runPass(abc);
    runPass(aggressiveDCE);
    runPass(removeBufs);

    runPass(cse);
    runPass(instCombine);
    runPass(aggressiveDCE);

    runPass(constMap);
    runPass(cse);
    runPass(instCombine);
    runPass(aggressiveDCE);

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
  explicit PassPipeline(Context &ctx) : ctx(ctx) {}
};

}; // namespace dyno
