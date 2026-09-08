#pragma once

#include "dyno/IDImpl.h"
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
#include "hw/passes/FlipFlopInference.h"
#include "hw/passes/FlipFlopMapping.h"
#include "hw/passes/FunctionInline.h"
#include "hw/passes/FuzzyCSE.h"
#include "hw/passes/InactiveCopy.h"
#include "hw/passes/InstCombine.h"
#include "hw/passes/LiftFlipFlops.h"
#include "hw/passes/LiftMUXs.h"
#include "hw/passes/LiftStdCells.h"
#include "hw/passes/LinearizeControlFlow.h"
#include "hw/passes/LoadCoalesce.h"
#include "hw/passes/LoopSimplify.h"
#include "hw/passes/LowerMemAccess.h"
#include "hw/passes/LowerOps.h"
#include "hw/passes/MemoryMapping.h"
#include "hw/passes/ModuleInline.h"
#include "hw/passes/MuxTreeFlatten.h"
#include "hw/passes/NetlistToProc.h"
#include "hw/passes/OrderInstrs.h"
#include "hw/passes/ParseDyno.h"
#include "hw/passes/ParseLiberty.h"
#include "hw/passes/ProcToNetlist.h"
#include "hw/passes/ProcessLinearize.h"
#include "hw/passes/RandomEquivalenceCheck.h"
#include "hw/passes/RebuildCache.h"
#include "hw/passes/RegisterPartition.h"
#include "hw/passes/RemoveBuffers.h"
#include "hw/passes/RemoveInitProcs.h"
#include "hw/passes/ResolveModules.h"
#include "hw/passes/SSAConstruct.h"
#include "hw/passes/SelectModules.h"
#include "hw/passes/SeqToComb.h"
#include "hw/passes/SimpleMemoryInference.h"
#include "hw/passes/TriggerDedupe.h"
#include "meta/MetaPassManager.h"

namespace dyno {
template <>
inline void registerDialectPasses<DIALECT_HW>(PassRegistry &passRegistry) {
  // Inline functions, copy their logic to all function call sites.
  passRegistry.registerPass<FunctionInlinePass>(DIALECT_HW);
  // Consolidate identical triggers.
  passRegistry.registerPass<TriggerDedupePass>(DIALECT_HW);
  // Converts SEQ_PROCESS_DEFs into COMB_PROCESS_DEFs by moving trigger
  // sensitivity from process itself into instructions, e.g.: STORE_DEFER ->
  // STORE_DEFER with explicit trigger STORE -> STORE_DEFER + STORE (maps to a
  // nonblocking assignment in always_ff, converted to split seq and comb
  // variables) ASSERT -> ASSERT_DEFER PRINT -> PRINT_DEFER
  passRegistry.registerPass<SeqToCombPass>(DIALECT_HW);
  // Convert to static single assignment form. We use structured control flow,
  // so YIELD value instead of PHI nodes. This makes it so that processes only
  // read/write each accessed register bitslice exactly once.
  passRegistry.registerPass<SSAConstructPass>(DIALECT_HW);
  // Schedule and fuse multiple processes. Depending on flags either maintains
  // certain dependencies or just fuses everything.
  passRegistry.registerPass<ProcessLinearizePass>(DIALECT_HW);
  // Combine/simplify instructions, main workhorse simplification pass. Many
  // small simplification patterns written in DSL or C++. Can be configured to
  // achieve various canonicalizations.
  passRegistry.registerPass<InstCombinePass>(DIALECT_HW);
  // Inline modules s.t. only top modules remain, with all logic directly in
  // them.
  passRegistry.registerPass<ModuleInlinePass>(DIALECT_HW);
  // Convert (do) while loops to for loops, simplify loop yield values.
  passRegistry.registerPass<LoopSimplifyPass>(DIALECT_HW);
  // Unroll loops & convert SSA-constructed if statements and switches to MUXs.
  passRegistry.registerPass<LinearizeControlFlowPass>(DIALECT_HW);
  // Liveness-based dead code elimination: Starting at root "used" values
  // (module outputs) mark all dependencies recursively. Then delete all
  // unmarked instructions.
  passRegistry.registerPass<AggressiveDeadCodeEliminationPass>(DIALECT_HW);
  // Lower complicated operations like add or multiply to simple gate-level
  // operations like and/or/xor.
  passRegistry.registerPass<LowerOpsPass>(DIALECT_HW);
  // Lower all logic to an And-Inverter Graph in preparation for logic
  // synthesis.
  passRegistry.registerPass<AIGConstructPass>(DIALECT_HW);
  // Run ABC on all AIGs for logic synthesis. Converts all comb logic to LUTs or
  // standard cell instances.
  passRegistry.registerPass<ABCPass>(DIALECT_HW);
  // Parse Liberty Files. Each cell is converted to a Dyno-IR module that
  // performs the specified operation.
  passRegistry.registerPass<ParseLibertyPass>(DIALECT_HW);
  // Convert STORE_DEFER to FLIP_FLOP. Mostly handles clock and asynchronous
  // reset, enable and synchronous reset handled in InstCombine.
  passRegistry.registerPass<FlipFlopInferencePass>(DIALECT_HW);
  // Old attempt at explicitly optimizing MUX trees. Blows up on nontrivial
  // logic, do not use.
  passRegistry.registerPass<MuxTreeOptimizationPass>(DIALECT_HW);
  // Hash all instructions and merge equivalent ones.
  passRegistry.registerPass<CommonSubexpressionEliminationPass>(DIALECT_HW);
  // Convert generic FLIP_FLOPs to specific target flip flops from liberty file.
  // Attempts to use flip flop with matching polarities, enable, reset/preset,
  // else falls backs to fixups (e.g. inserting loopback MUX for enable).
  passRegistry.registerPass<FlipFlopMappingPass>(DIALECT_HW);
  // Remove buffer standard cells or LUT2's generated by ABC.
  passRegistry.registerPass<RemoveBuffersPass>(DIALECT_HW);
  // Drop all dataflow-only instructions from CFG and them re-insert them in
  // topological dataflow order.
  passRegistry.registerPass<OrderInstrsPass>(DIALECT_HW);
  // Convert dyno constants to concat of constant 0/constant 1 signals.
  passRegistry.registerPass<ConstantMappingPass>(DIALECT_HW);
  // Find longest path through standard cells, resetting at flip flops.
  passRegistry.registerPass<FindLongestPathPass>(DIALECT_HW);
  // Perform IR integrity checks, e.g. all ADD operands are of the same bit
  // size.
  passRegistry.registerPass<CheckPass>(DIALECT_HW);
  // Splits registers with multiple distinct stores.
  passRegistry.registerPass<RegisterPartitionPass>(DIALECT_HW);
  // Fuzzy common subexpression elimination, finds common subexpression that
  // simple hashing can't find, like ADD a, b, c ADD a, b
  passRegistry.registerPass<FuzzyCSEPass>(DIALECT_HW);
  // Share instructions based on trivial exclusivity, i.e. being in different
  // blocks of a switch or if/else statement.
  passRegistry.registerPass<EarlySharePass>(DIALECT_HW);
  // Convert INSERT/EXTRACT= chains to MEM_LOAD/MEM_STORE explicit accesses in
  // preparation for mapping to target memories.
  passRegistry.registerPass<SimpleMemoryInferencePass>(DIALECT_HW);
  // Merge loads of adjacent bit slices of a registers.
  passRegistry.registerPass<LoadCoalescePass>(DIALECT_HW);
  // Delete init processes (evaluate before with EvalInitProcs)
  passRegistry.registerPass<RemoveInitProcsPass>(DIALECT_HW);
  // Convert MUX trees to flat n-ary ONEHOT_MUXs
  passRegistry.registerPass<MuxTreeFlattenPass>(DIALECT_HW);
  // Dump Dyno-IR to file (default /dev/stdout).
  passRegistry.registerPass<DumpPass>(DIALECT_HW);
  // Convert MEM_LOAD/MEM_STORE accessed registers to target-specific memories.
  passRegistry.registerPass<MemoryMappingPass>(DIALECT_HW);
  // Parse Dyno IR files
  passRegistry.registerPass<ParseDynoPass>(DIALECT_HW);
  // Dump gate level Dyno IR as Verilog
  passRegistry.registerPass<DumpVerilogPass>(DIALECT_HW);
  // For use after parsing, regenerate inline caches of all modules that have
  // them.
  passRegistry.registerPass<PopulateInlineCachesPass>(DIALECT_HW);
  // Checks equivalence of 2 modules under random stimulus. Expects exactly 2
  // modules.
  passRegistry.registerPass<RandomEquivalenceCheckPass>(DIALECT_HW);
  // For use with RandomEquivalenceCheckPass, creates inactive (not touched by
  // passes) copies of current modules.
  passRegistry.registerPass<InactiveCopyPass>(DIALECT_HW);
  // Turns wire-based in-process STDCELL_INSTANCE instrs to regular
  // register-based INSTANCE instrs.
  passRegistry.registerPass<LiftStdCellsPass>(DIALECT_HW);
  // Select/unselect modules for passes to operate on with regex.
  passRegistry.registerPass<SelectModulesPass>(DIALECT_HW);
  // Lift hw.FLIP_FLOP(_SRST) instrs back to STORE_DEFER with rst/en MUXs
  passRegistry.registerPass<LiftFlipFlopsPass>(DIALECT_HW);
  // Lowers hw.MEM_LOAD/hw.MEM_STORE instrs on registers to dataflow
  // INSERT/SPLICE.
  passRegistry.registerPass<LowerMemAccessPass>(DIALECT_HW);
  // tbd
  passRegistry.registerPass<LiftMuxPass>(DIALECT_HW);
  // Convert netlists (wire forward references allowed) to processes (forward
  // references only via register).
  passRegistry.registerPass<NetlistToProcPass>(DIALECT_HW);
  // Convert processes (forward references only via register) to netlists (wire
  // forward references allowed).
  passRegistry.registerPass<ProcToNetlistPass>(DIALECT_HW);
  // Link modules by name to resolve definition-less module uses.
  passRegistry.registerPass<ResolveModulesPass>(DIALECT_HW);
  // Dumps HW module hierarchy and registers as C++ header for Verilator-style
  // simulations.
  passRegistry.registerPass<DumpSimHeaderPass>(DIALECT_HW);
  // Interpret initial processes to find initial register values.
  passRegistry.registerPass<EvalInitProcsPass>(DIALECT_HW);
}
}; // namespace dyno
