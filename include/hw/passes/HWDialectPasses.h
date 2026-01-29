#pragma once

#include "dyno/IDImpl.h"
#include "hw/passes/ABC.h"
#include "hw/passes/AIGConstruct.h"
#include "hw/passes/AggressiveDeadCodeElimination.h"
#include "hw/passes/CheckPass.h"
#include "hw/passes/CommonSubexpressionElimination.h"
#include "hw/passes/ConstantMapping.h"
#include "hw/passes/DumpPass.h"
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
#include "hw/passes/MuxTreeFlatten.h"
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
registerDialectPasses<DialectID{DIALECT_HW}>(PassRegistry &passRegistry) {
  passRegistry.registerPass<FunctionInlinePass>();
  passRegistry.registerPass<TriggerDedupePass>();
  passRegistry.registerPass<SeqToCombPass>();
  passRegistry.registerPass<SSAConstructPass>();
  passRegistry.registerPass<ProcessLinearizePass>();
  passRegistry.registerPass<InstCombinePass>();
  passRegistry.registerPass<ModuleInlinePass>();
  passRegistry.registerPass<LoopSimplifyPass>();
  passRegistry.registerPass<LinearizeControlFlowPass>();
  passRegistry.registerPass<AggressiveDeadCodeEliminationPass>();
  passRegistry.registerPass<LowerOpsPass>();
  passRegistry.registerPass<AIGConstructPass>();
  passRegistry.registerPass<ABCPass>();
  passRegistry.registerPass<ParseLibertyPass>();
  passRegistry.registerPass<FlipFlopInferencePass>();
  passRegistry.registerPass<MuxTreeOptimizationPass>();
  passRegistry.registerPass<CommonSubexpressionEliminationPass>();
  passRegistry.registerPass<FlipFlopMappingPass>();
  passRegistry.registerPass<RemoveBuffersPass>();
  passRegistry.registerPass<OrderInstrsPass>();
  passRegistry.registerPass<ConstantMappingPass>();
  passRegistry.registerPass<FindLongestPathPass>();
  passRegistry.registerPass<CheckPass>();
  passRegistry.registerPass<RegisterPartitionPass>();
  passRegistry.registerPass<FuzzyCSEPass>();
  passRegistry.registerPass<EarlySharePass>();
  passRegistry.registerPass<SimpleMemoryMappingPass>();
  passRegistry.registerPass<LoadCoalescePass>();
  passRegistry.registerPass<RemoveInitProcsPass>();
  passRegistry.registerPass<MuxTreeFlattenPass>();
  passRegistry.registerPass<DumpPass>();
}
}; // namespace dyno
