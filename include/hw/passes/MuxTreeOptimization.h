#pragma once

#include "dyno/Constant.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "dyno/ObjMap.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "op/IDs.h"
#include "support/Bits.h"
#include "support/Debug.h"
#include "support/DenseMap.h"
#include "support/DenseMapInfo.h"
#include "support/DenseMultimap.h"
#include "support/DynBitSet.h"
#include "support/ErrorRecovery.h"
#include "support/SlabAllocator.h"
#include <bit>
#include <climits>

#include "hw/analysis/MuxTree.h"

namespace dyno {
class MuxTreeOptimizationPass {

  SlabAllocator<MuxTree> muxTreeAlloc;
  ObjMapVec<Instr, bool> visitedMap;

  HWContext &ctx;
  HWInstrBuilder build;

  struct Selector {
    HWValue signal;
    SmallVec<InstrRef, 4> depending;
  };

  struct MuxTree2 {
    SmallVec<Selector, 4> selector;
  };

  void printMuxTree(MuxTree *tree) {
    DEBUG("MuxTreeOptimization", {
      dbgs() << "mux tree at: ";
      if (tree->root)
        dumpInstr(tree->root);
      else
        dbgs() << "\n";
      for (auto entry : tree->entries) {

        entry.expr.dump(false);
        dbgs() << ": ";

        dumpObj(ctx.resolveObj(entry.output));
        dbgs() << "\n";
      }
    });
  }

  // heuristic to select expression to MUX on next. You can e.g. just return
  // tree->entry[0].expr to always select between element 0 and the rest, thus
  // building a linear MUX chain. What this actually does is greedily peel of
  // elements that resolve the assignment for an entire var. If there's none of
  // those left, it just selects the single boolean signal as close to 50/50 as
  // possible.
  SmallBoolExprCNF getBestMUXExpr(MuxTree *tree) {
    uint maxNumSingleton = 0;
    uint maxNumSingletonIdx;
    for (auto [idx, entry] : Range{tree->entries}.enumerate()) {
      auto inv = entry.expr.negated2(tree->conditions.size());
      inv.simplify(tree->conditions.size());

      uint numSingleton = 0;
      for (auto clause : inv.clauses())
        if (clause.size() == 1)
          numSingleton++;

      if (numSingleton > maxNumSingleton) {
        maxNumSingletonIdx = idx;
        maxNumSingleton = numSingleton;
      }
    }
    if (maxNumSingleton != 0) {
      return tree->entries[maxNumSingletonIdx].expr;
    }

    dbgs() << "no singletons found, instead selecting max entropy single bit\n";

    uint bestLitIdx = 0;
    uint bestTotal = 0;
    double bestRatio = 0.0;

    for (uint litIdx = 0; litIdx < tree->conditions.size(); litIdx++) {
      SmallBoolExprCNF testExpr;
      testExpr.literals.emplace_back(
          BoolExprLiteral{uint16_t(litIdx), 0, true});
      SmallBoolExprCNF testExprInv = testExpr.negated2(tree->conditions.size());

      uint numTrue = 0;
      uint numFalse = 0;

      for (auto &entry : tree->entries) {
        SmallBoolExprCNF cond = entry.expr;
        cond.addAsGlobalAND(testExpr);
        // dbgs() << "checking true case: ";
        // cond.dump();
        auto trueRes = cond.simplify(tree->conditions.size());
        if (trueRes.has_value() && trueRes.value() == false) {
          numFalse++;
          continue;
        }
        cond = entry.expr;
        cond.addAsGlobalAND(testExprInv);
        // dbgs() << "checking false case: ";
        // cond.dump();
        auto falseRes = cond.simplify(tree->conditions.size());
        if (falseRes.has_value() && falseRes.value() == false) {
          numTrue++;
          continue;
        }
      }

      uint total = numTrue + numFalse;
      uint totalUnadj = total;
      if (total != tree->entries.size()) {
        uint diff = tree->entries.size() - total;
        numTrue += diff;
        numFalse += diff;
        total += 2 * diff;
      }

      double ratio = (double)numTrue / total;
      ratio = std::min(ratio, 1.0 - ratio);
      if (bestTotal < totalUnadj || (bestTotal <= totalUnadj && ratio > bestRatio)) {
        bestLitIdx = litIdx;
        bestTotal = totalUnadj;
        bestRatio = ratio;
      }
    }

    SmallBoolExprCNF selExpr;
    selExpr.literals.emplace_back(uint16_t(bestLitIdx), 0, true);
    return selExpr;
  }

  std::pair<WireRef, bool> getLiteralVal(MuxTree *tree, BoolExprLiteral cond) {
    WireRef out = ctx.getWires().resolve(tree->conditions[cond.id].wire);
    if (out.getNumBits() != 1)
      out = build.buildSplice(out, BitRange{tree->conditions[cond.id].idx, 1u})
                .as<WireRef>();

    return std::make_pair(out, !!cond.inverse);
  }

  HWValue getExprVal(MuxTree *tree, SmallBoolExprCNF &expr) {
    SmallVec<HWValue, 4> andOperands;
    for (auto clause : expr.clauses()) {
      SmallVec<HWValue, 4> orOperands;
      for (auto lit : clause) {
        auto [val, inv] = getLiteralVal(tree, lit);
        if (inv)
          val = build.buildNot(val).as<WireRef>();
        orOperands.emplace_back(val);
      }
      if (orOperands.size() == 1)
        andOperands.emplace_back(orOperands.front());
      else {
        auto wire = build.buildInstr(OP_OR, true, ArrayRef{orOperands}).defW();
        wire->numBits = 1;
        andOperands.emplace_back(wire);
      }
    }
    HWValue sel;
    if (andOperands.size() == 1)
      sel = andOperands[0];
    else {
      auto wire = build.buildInstr(OP_AND, true, ArrayRef{andOperands}).defW();
      sel = wire;
    }

    return sel;
  }

  template <std::invocable<MutArrayRef<BoolExprLiteral>> Func>
  void forAllClauses(MuxTree::Entry &entry, Func &&func) {
    for (auto clause : entry.expr.clauses()) {
      func(MutArrayRef{clause.begin(), clause.end()});
    }
  }

  HWValue lowerMuxTreeSimple(MuxTree *tree) {
    // left = false, right = true
    if (tree->root)
      build.setInsertPoint(tree->root);

    if (tree->entries.size() == 1) {
      return ctx.resolveObj(tree->entries.front().output);
    }

    printMuxTree(tree);
    if (tree->entries.size() == 2) {
      return build.buildMux(getExprVal(tree, tree->entries[0].expr),
                            ctx.resolveObj(tree->entries[0].output),
                            ctx.resolveObj(tree->entries[1].output));
    }

    auto selExpr = getBestMUXExpr(tree);
    DEBUG("MuxTreeOptimization", {
      dbgs() << "splitting tree on: ";
      selExpr.dump();
    });
    dbgs() << "\n\n";

    MuxTree *left = muxTreeAlloc.allocate();
    MuxTree *right = muxTreeAlloc.allocate();

    left->conditions = tree->conditions;
    left->root = nullref;

    right->conditions = tree->conditions;
    right->root = nullref;

    // only valid if the corresponding tree is empty.
    DynObjRef leftSingleOutput = nullref;
    DynObjRef rightSingleOutput = nullref;

    // go thru entries. consider the entry expression in the cases that the
    // expression we MUX on is true (right subtree) or false (left subtree). If
    // the entry expression becomes unsatisfiable in either subtree we can
    // safely discard it.
    for (auto entry : tree->entries) {
      // decide whether to put entry in left, right or both.
      assert(!entry.expr.isUnsat());
      auto [ifTrueRem, trueBrSat, ifFalseRem, falseBrSat] =
          entry.expr.evalWithBoundVars2(entry.expr, selExpr,
                                        tree->conditions.size());

      if (!trueBrSat && !falseBrSat)
        continue;

      if (trueBrSat)
        rightSingleOutput = entry.output;
      if (falseBrSat)
        leftSingleOutput = entry.output;

      if (falseBrSat) {
        left->entries.emplace_back(
            MuxTree::Entry{std::move(ifFalseRem), entry.output});
      }
      if (trueBrSat) {
        right->entries.emplace_back(
            MuxTree::Entry{std::move(ifTrueRem), entry.output});
      }
    }

    DEBUG("MuxTreeOptimization", {
      dbgs() << "left subtree\n";
      printMuxTree(left);

      dbgs() << "right subtree\n";
      printMuxTree(right);
    })

    HWValue leftVal;
    HWValue rightVal;

    if (left->entries.size() == 0)
      leftVal = ctx.resolveObj(leftSingleOutput);
    else
      leftVal = lowerMuxTreeSimple(left);

    if (right->entries.size() == 0)
      rightVal = ctx.resolveObj(rightSingleOutput);
    else
      rightVal = lowerMuxTreeSimple(right);

    auto sel = getExprVal(tree, selExpr);
    return build.buildMux(sel, rightVal, leftVal);
  }

  void runOnProcess(ProcessIRef proc) {
    MuxtreeAnalysis analysis;
    // assume proc is already flat for now, ignore SCF constructs.
    for (auto instr :
         Range{proc.block().begin(), proc.block().end()}.reverse()) {
      if (visitedMap[instr])
        continue;

      switch (*instr.getDialectOpcode()) {
      case *HW_MUX:
        visitedMap[instr] = 1;
        auto muxtree = analysis.analyzeMuxTree(
            instr, [&](InstrRef ref) { visitedMap[ref] = 1; });
        analysis.simplifyConditions(&muxtree);
        analysis.dedupeMuxTreeOutputs(&muxtree);
        printMuxTree(&muxtree);
        auto wire = lowerMuxTreeSimple(&muxtree);
        instr.def(0)->as<WireRef>().replaceAllUsesWith(wire);
        return;
        break;
      }
    }
  }

  void runOnModule(ModuleIRef mod) {
    for (auto proc : mod.procs())
      runOnProcess(proc);
  }

public:
  void run() {
    visitedMap.resize(ctx.getInstrs().numIDs());

    ctx.getInstrs().createHooks.emplace_back(
        [&](InstrRef ref) { visitedMap.get_ensure(ref) = 1; });

    for (auto mod : ctx.getModules()) {
      runOnModule(mod.iref());
    }
  }

  explicit MuxTreeOptimizationPass(HWContext &ctx) : ctx(ctx), build(ctx) {}
};
}; // namespace dyno
