#pragma once

#include "dyno/IDImpl.h"
#include "hw/passes/ABC.h"
#include "hw/passes/AIGConstruct.h"
#include "hw/passes/AggressiveDeadCodeElimination.h"
#include "hw/passes/CheckPass.h"
#include "hw/passes/CommonSubexpressionElimination.h"
#include "hw/passes/ConstantMapping.h"
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
#include "meta/MetaPassManager.h"

namespace dyno {
template <>
inline void
registerDialectPasses<DialectID{DIALECT_HW}>(MetaPassManager &metaPassManager) {
  metaPassManager.registerPass<FunctionInlinePass>();
  metaPassManager.registerPass<TriggerDedupePass>();
  metaPassManager.registerPass<SeqToCombPass>();
  metaPassManager.registerPass<SSAConstructPass>();
  metaPassManager.registerPass<ProcessLinearizePass>();
  metaPassManager.registerPass<InstCombinePass>();
  metaPassManager.registerPass<ModuleInlinePass>();
  metaPassManager.registerPass<LoopSimplifyPass>();
  metaPassManager.registerPass<LinearizeControlFlowPass>();
  metaPassManager.registerPass<AggressiveDeadCodeEliminationPass>();
  metaPassManager.registerPass<LowerOpsPass>();
  metaPassManager.registerPass<AIGConstructPass>();
  metaPassManager.registerPass<ABCPass>();
  metaPassManager.registerPass<ParseLibertyPass>();
  metaPassManager.registerPass<FlipFlopInferencePass>();
  metaPassManager.registerPass<MuxTreeOptimizationPass>();
  metaPassManager.registerPass<CommonSubexpressionEliminationPass>();
  metaPassManager.registerPass<FlipFlopMappingPass>();
  metaPassManager.registerPass<RemoveBuffersPass>();
  metaPassManager.registerPass<OrderInstrsPass>();
  metaPassManager.registerPass<ConstantMappingPass>();
  metaPassManager.registerPass<FindLongestPathPass>();
  metaPassManager.registerPass<CheckPass>();
  metaPassManager.registerPass<RegisterPartitionPass>();
  metaPassManager.registerPass<FuzzyCSEPass>();
  metaPassManager.registerPass<EarlySharePass>();
  metaPassManager.registerPass<SimpleMemoryMappingPass>();
  metaPassManager.registerPass<LoadCoalescePass>();
  metaPassManager.registerPass<RemoveInitProcsPass>();
}
}; // namespace dyno
