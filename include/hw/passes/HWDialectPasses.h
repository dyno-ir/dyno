#pragma once

#include "dyno/IDImpl.h"
#include "meta/MetaPassManager.h"

namespace dyno {

void registerDialectPasses_impls0(PassRegistry &passRegistry);
void registerDialectPasses_impls1(PassRegistry &passRegistry);
void registerDialectPasses_impls2(PassRegistry &passRegistry);
void registerDialectPasses_impls3(PassRegistry &passRegistry);

template <>
inline void registerDialectPasses<DIALECT_HW>(PassRegistry &passRegistry) {
  registerDialectPasses_impls0(passRegistry);
  registerDialectPasses_impls1(passRegistry);
  registerDialectPasses_impls2(passRegistry);
  registerDialectPasses_impls3(passRegistry);
}
}; // namespace dyno

// Central place to assign passes to TUs, rather than forcing a TU
// for every pass, or having this list distributed in multiple files.
// We should probably autogenerate this at some point though.

#ifdef IMPL
#if IMPL == 0

#include "hw/passes/ABC.h"
#include "hw/passes/AIGConstruct.h"
#include "hw/passes/AggressiveDeadCodeElimination.h"
#include "hw/passes/CheckPass.h"
#include "hw/passes/CommonSubexpressionElimination.h"
#include "hw/passes/ConstantMapping.h"
#include "hw/passes/DumpPass.h"
#include "hw/passes/DumpSimHeader.h"
#include "hw/passes/DumpVerilog.h"
#include "hw/passes/EarlySharePass.h"
#include "hw/passes/EvalInitProcs.h"
#include "hw/passes/FindLongestPath.h"

namespace dyno {
void registerDialectPasses_impls0(PassRegistry &passRegistry) {
  passRegistry.registerPass<ABCPass>(DIALECT_HW);
  passRegistry.registerPass<AIGConstructPass>(DIALECT_HW);
  passRegistry.registerPass<AggressiveDeadCodeEliminationPass>(DIALECT_HW);
  passRegistry.registerPass<CheckPass>(DIALECT_HW);
  passRegistry.registerPass<CommonSubexpressionEliminationPass>(DIALECT_HW);
  passRegistry.registerPass<ConstantMappingPass>(DIALECT_HW);
  passRegistry.registerPass<DumpPass>(DIALECT_HW);
  passRegistry.registerPass<DumpSimHeaderPass>(DIALECT_HW);
  passRegistry.registerPass<DumpVerilogPass>(DIALECT_HW);
  passRegistry.registerPass<EarlySharePass>(DIALECT_HW);
  passRegistry.registerPass<EvalInitProcsPass>(DIALECT_HW);
  passRegistry.registerPass<FindLongestPathPass>(DIALECT_HW);
}
} // namespace dyno

#elif IMPL == 1

#include "hw/passes/FlipFlopInference.h"
#include "hw/passes/FlipFlopMapping.h"
#include "hw/passes/FunctionInline.h"
#include "hw/passes/FuzzyCSE.h"
#include "hw/passes/InactiveCopy.h"
#include "hw/passes/InstCombine.h"
#include "hw/passes/LiftFlipFlops.h"
#include "hw/passes/LiftMux.h"
#include "hw/passes/LiftStdCells.h"
#include "hw/passes/LinearizeControlFlow.h"
#include "hw/passes/LoadCoalesce.h"
#include "hw/passes/LoopSimplify.h"

namespace dyno {
void registerDialectPasses_impls1(PassRegistry &passRegistry) {
  passRegistry.registerPass<FlipFlopInferencePass>(DIALECT_HW);
  passRegistry.registerPass<FlipFlopMappingPass>(DIALECT_HW);
  passRegistry.registerPass<FunctionInlinePass>(DIALECT_HW);
  passRegistry.registerPass<FuzzyCSEPass>(DIALECT_HW);
  passRegistry.registerPass<InactiveCopyPass>(DIALECT_HW);
  passRegistry.registerPass<InstCombinePass>(DIALECT_HW);
  passRegistry.registerPass<LiftFlipFlopsPass>(DIALECT_HW);
  passRegistry.registerPass<LiftMuxPass>(DIALECT_HW);
  passRegistry.registerPass<LiftStdCellsPass>(DIALECT_HW);
  passRegistry.registerPass<LinearizeControlFlowPass>(DIALECT_HW);
  passRegistry.registerPass<LoadCoalescePass>(DIALECT_HW);
  passRegistry.registerPass<LoopSimplifyPass>(DIALECT_HW);
}
} // namespace dyno

#elif IMPL == 2

#include "hw/passes/LowerMemAccess.h"
#include "hw/passes/LowerOps.h"
#include "hw/passes/MemoryMapping.h"
#include "hw/passes/ModuleInline.h"
#include "hw/passes/MuxTreeFlatten.h"
#include "hw/passes/NetlistToProc.h"
#include "hw/passes/OrderInstrs.h"
#include "hw/passes/ParseDyno.h"
#include "hw/passes/ParseLiberty.h"
#include "hw/passes/PopulateInlineCaches.h"
#include "hw/passes/ProcToNetlist.h"
#include "hw/passes/ProcessLinearize.h"

namespace dyno {
void registerDialectPasses_impls2(PassRegistry &passRegistry) {
  passRegistry.registerPass<LowerMemAccessPass>(DIALECT_HW);
  passRegistry.registerPass<LowerOpsPass>(DIALECT_HW);
  passRegistry.registerPass<MemoryMappingPass>(DIALECT_HW);
  passRegistry.registerPass<ModuleInlinePass>(DIALECT_HW);
  passRegistry.registerPass<MuxTreeFlattenPass>(DIALECT_HW);
  passRegistry.registerPass<NetlistToProcPass>(DIALECT_HW);
  passRegistry.registerPass<OrderInstrsPass>(DIALECT_HW);
  passRegistry.registerPass<ParseDynoPass>(DIALECT_HW);
  passRegistry.registerPass<ParseLibertyPass>(DIALECT_HW);
  passRegistry.registerPass<PopulateInlineCachesPass>(DIALECT_HW);
  passRegistry.registerPass<ProcToNetlistPass>(DIALECT_HW);
  passRegistry.registerPass<ProcessLinearizePass>(DIALECT_HW);
}
} // namespace dyno

#elif IMPL == 3

#include "hw/passes/RandomEquivalenceCheck.h"
#include "hw/passes/RegisterPartition.h"
#include "hw/passes/RemoveBuffers.h"
#include "hw/passes/RemoveInitProcs.h"
#include "hw/passes/RemoveSideEffectInstrs.h"
#include "hw/passes/ResolveModules.h"
#include "hw/passes/SSAConstruct.h"
#include "hw/passes/SelectModules.h"
#include "hw/passes/SeqToComb.h"
#include "hw/passes/SimpleMemoryInference.h"
#include "hw/passes/TemplateLower.h"
#include "hw/passes/TriggerDedupe.h"

namespace dyno {
void registerDialectPasses_impls3(PassRegistry &passRegistry) {
  passRegistry.registerPass<RandomEquivalenceCheckPass>(DIALECT_HW);
  passRegistry.registerPass<RegisterPartitionPass>(DIALECT_HW);
  passRegistry.registerPass<RemoveBuffersPass>(DIALECT_HW);
  passRegistry.registerPass<RemoveInitProcsPass>(DIALECT_HW);
  passRegistry.registerPass<RemoveSideEffectInstrsPass>(DIALECT_HW);
  passRegistry.registerPass<ResolveModulesPass>(DIALECT_HW);
  passRegistry.registerPass<SSAConstructPass>(DIALECT_HW);
  passRegistry.registerPass<SelectModulesPass>(DIALECT_HW);
  passRegistry.registerPass<SeqToCombPass>(DIALECT_HW);
  passRegistry.registerPass<SimpleMemoryInferencePass>(DIALECT_HW);
  passRegistry.registerPass<TemplateLowerPass>(DIALECT_HW);
  passRegistry.registerPass<TriggerDedupePass>(DIALECT_HW);
}
} // namespace dyno

#endif
#endif
