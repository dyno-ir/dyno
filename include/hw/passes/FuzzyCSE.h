#pragma once

#include "dyno/HierBlockIterator.h"
#include "hw/AutoDebugInfo.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/analysis/BitAliasAnalysis.h"
#include "hw/analysis/ControlFlow.h"
#include "support/Debug.h"
#include "support/DynBitSet.h"

namespace dyno {

class FuzzyCSEPass {
  HWContext &ctx;
  BitAliasAnalysis bitAlias;
  HWInstrBuilder build;
  ControlFlowAnalysis controlFlowAnalysis;
  AutoCopyDebugInfoStack autoDbgInfo;

  struct AbstractValue {
    uint16_t id;

    friend bool operator==(AbstractValue lhs, AbstractValue rhs) {
      return lhs.id == rhs.id;
    }
    friend auto operator<=>(AbstractValue lhs, AbstractValue rhs) {
      return lhs.id <=> rhs.id;
    }
  };

  struct AbstractInstr {
    SmallVec<std::pair<uint16_t, AbstractValue>, 4> operands;
    InstrRef ref;
    uint idx;
  };

public:
  struct Config {
    bool enableShare = false;
    uint32_t minSharedBitsForMerge = 4;
  };
  Config config;

private:
  // InstrRef selectSharePair(InstrRef root, IsRange auto shareCandidates) {
  //   // filter out candidates that aren't mutually exclusive
  //   SmallVec<InstrRef, 4> candidates;
  //   for (auto candidate : shareCandidates) {
  //     if (!controlFlowAnalysis.isTriviallyMutuallyExclusive(root, candidate))
  //       continue;
  //     candidates.emplace_back(candidate);
  //   }

  //   // heuristically select best (todo)
  //   if (candidates.size() != 1)
  //     return nullref;

  //   return candidates.front();
  // }

  // void shareInstrs(InstrRef root, InstrRef other) {

  // }

  static void intersect(SmallVecImpl<std::pair<uint16_t, uint16_t>> &out,
                        ArrayRef<std::pair<uint16_t, AbstractValue>> lhs,
                        ArrayRef<std::pair<uint16_t, AbstractValue>> rhs) {
    size_t i = 0;
    size_t j = 0;
    while (i != lhs.size() && j != rhs.size()) {
      if (lhs[i].second == rhs[j].second) {
        out.emplace_back(lhs[i].first, rhs[j].first);
        i++;
        j++;
      } else if (lhs[i].second < rhs[j].second) {
        i++;
      } else
        j++;
    }
  }

  uint32_t intersectConstants(InstrRef lhs, InstrRef rhs) {
    auto lhsLast = lhs.other(lhs.getNumOthers() - 1)->dyn_as<ConstantRef>();
    auto rhsLast = rhs.other(rhs.getNumOthers() - 1)->dyn_as<ConstantRef>();
    if (!lhsLast || !rhsLast)
      return 0;

    BigInt diff;
    BigInt::xorOp4S(diff, lhsLast, rhsLast);
    BigInt::resizeOp4S(diff, diff,
                       std::min(lhsLast.getNumBits(), rhsLast.getNumBits()));
    auto matchingBits = BigInt::trailingZeros4SExact(diff);
    if (matchingBits == 0)
      return 0;

    return matchingBits;
  }

  auto rebuildInstr(AbstractInstr &instr,
                    ArrayRef<RegisterValue> matchingPrefixes,
                    const DynSymbSet<SmallVec<uint64_t, 1>, 1> &covered,
                    IsRange auto matchedIdxs, WireRef sharedSum) {
    // Rebuild the two instructions, with (1) adding the shared sum as an
    // additional operand and (2) replacing all matched operand fragments with
    // zero.
    auto tok = autoDbgInfo.addWithToken(instr.ref);
    SmallVec<HWValue, 4> lhsNewOps;
    for (auto [opIdx, op] : Range{instr.ref.others()}.enumerate()) {
      if (covered[opIdx])
        continue;
      lhsNewOps.emplace_back(op->as<HWValue>());
    }

    for (auto [idx, opIdx] : matchedIdxs.enumerate()) {
      assert(covered[opIdx]);
      auto op = instr.ref.other(opIdx);
      HWValue value = op->template as<HWValue>();

      auto matchedLen = matchingPrefixes[idx].getLen();
      value = build.buildSplice(value, *value.getNumBits() - matchedLen,
                                matchedLen);
      auto zero = ctx.constBuild().zero(matchedLen).get();
      value = build.buildConcat(value, zero);

      lhsNewOps.emplace_back(value);
    }
    auto outBits = *instr.ref.def(0)->as<WireRef>().getNumBits();
    lhsNewOps.emplace_back(build.buildResize(sharedSum, outBits, false));
    auto ibLHS = build.buildInstrRaw(OP_ADD, 1 + lhsNewOps.size());
    ibLHS.addRef(instr.ref.def(0)->as<WireRef>()).other().addRefs(lhsNewOps);

    // Delete all uses of this instruction (todo: better data structure or don't
    // reimplement def use)
    for (auto op : instr.operands) {
      auto &uses = operandUses[op.second.id];
      auto useIt =
          Range{uses}.find_if([&](Use use) { return use.instr == instr.idx; });
      assert(useIt != uses.end());
      uses.erase_unordered(useIt);
    }

    instr.ref.def(0).replace(FatDynObjRef{nullref});
    build.destroyInstr(instr.ref);
    instr.ref = nullref;
    bitAlias.clearCache();
  }

  bool tryCombineInstrs(AbstractInstr &lhsAbstr, AbstractInstr &rhsAbstr) {

    // we call this based on heuristic match of one operand.
    SmallVec<std::pair<uint16_t, uint16_t>, 4> intersectOps;
    intersect(intersectOps, ArrayRef{lhsAbstr.operands},
              ArrayRef{rhsAbstr.operands});
    auto matchingConstBits = intersectConstants(lhsAbstr.ref, rhsAbstr.ref);

    assert(intersectOps.size() != 0 && "heuristic not working?");
    if ((intersectOps.size() + !!matchingConstBits) < 2)
      return false;

    DEBUG("FuzzyCSE", {
      dbgs() << "heuristically matched instrs:\n";
      HWPrinter print{dbgs()};
      print.printInstr(lhsAbstr.ref, ctx);
      print.printInstr(rhsAbstr.ref, ctx);
      dbgs() << "matching operand first frags:\n";
      for (auto [lhs, rhs] : intersectOps) {
        dbgs() << lhs << ", " << rhs << "\n";
      }
    })

    DynSymbSet<SmallVec<uint64_t, 1>, 1> lhsCovered, rhsCovered;
    lhsCovered.resize(lhsAbstr.ref.getNumOthers());
    rhsCovered.resize(rhsAbstr.ref.getNumOthers());

    SmallVec<RegisterValue, 4> matchingPrefixes;
    uint32_t maxPrefixSize = 0;
    for (auto it = intersectOps.begin(); it != intersectOps.end();) {
      auto [lhsOpIdx, rhsOpIdx] = *it;
      auto lhsOp = lhsAbstr.ref.other(lhsOpIdx);
      auto rhsOp = rhsAbstr.ref.other(rhsOpIdx);

      auto lhsV = lhsOp->as<HWValue>();
      auto rhsV = rhsOp->as<HWValue>();

      auto [lhsRepr, lhsChange] = bitAlias.getReprAliases(lhsV);
      auto [rhsRepr, rhsChange] = bitAlias.getReprAliases(rhsV);

      auto &prefix = matchingPrefixes.emplace_back();
      prefix.untouched = false;
      auto len = std::min(lhsRepr.frags.size(), rhsRepr.frags.size());
      for (size_t i = 0; i < len; i++) {
        auto &lhsFrag = lhsRepr.frags[i];
        auto &rhsFrag = rhsRepr.frags[i];
        assert(lhsFrag.dstAddr == rhsFrag.dstAddr);
        if (lhsFrag.ref != rhsFrag.ref || lhsFrag.srcAddr != rhsFrag.srcAddr)
          break;

        if (lhsFrag.len != rhsFrag.len) {
          auto shorter = lhsFrag.len < rhsFrag.len ? lhsFrag : rhsFrag;
          prefix.frags.emplace_back(shorter);
          // fixme: make version of register value w/o untouched
          prefix.frags.back().untouched = false;
          break;
        }

        prefix.frags.emplace_back(lhsFrag);
        prefix.frags.back().untouched = false;
      }
      maxPrefixSize = std::max(maxPrefixSize, prefix.getLen());
      if (prefix.frags.empty()) {
        return false;
        matchingPrefixes.pop_back();
        it = intersectOps.erase(it);
        continue;
      }
      lhsCovered[lhsOpIdx] = 1;
      rhsCovered[rhsOpIdx] = 1;
      ++it;
    }

    // don't merge tiny ops
    if (maxPrefixSize < config.minSharedBitsForMerge)
      return false;

    assert(matchingPrefixes.size() == intersectOps.size());

    if (matchingConstBits != 0) {
      // constants are always last in operands
      auto lhsLastOpIdx = lhsAbstr.ref.getNumOthers() - 1;
      auto rhsLastOpIdx = rhsAbstr.ref.getNumOthers() - 1;
      lhsCovered[lhsLastOpIdx] = 1;
      rhsCovered[rhsLastOpIdx] = 1;
      intersectOps.emplace_back(lhsLastOpIdx, rhsLastOpIdx);
      auto constVal = lhsAbstr.ref.other(lhsLastOpIdx)->as<ConstantRef>();
      ConstantRef truncVal =
          ctx.constBuild().val(constVal).resize(matchingConstBits);
      matchingPrefixes.emplace_back(
          RegisterValue{truncVal, truncVal.getNumBits(), 0, 0, nullopt});
      maxPrefixSize = std::max(maxPrefixSize, matchingConstBits);
    }

    if (matchingPrefixes.size() == 0)
      return false;

    autoDbgInfo.pushDebugInfo(lhsAbstr.ref);
    autoDbgInfo.pushDebugInfo(rhsAbstr.ref);

    auto parentBlock = controlFlowAnalysis.findSharedParentBlock(
        HWInstrRef{lhsAbstr.ref}.parentBlock(ctx),
        HWInstrRef{rhsAbstr.ref}.parentBlock(ctx));
    build.setInsertPoint(parentBlock.begin());
    auto ib = build.buildInstrRaw(lhsAbstr.ref.getDialectOpcode(),
                                  1 + matchingPrefixes.size());
    auto defW = ctx.getWires().create(maxPrefixSize + 1);
    ib.addRef(defW).other();

    build.setInsertPoint(ib.instr());

    auto &newInstrAbstr = candidates.emplace_back();
    newInstrAbstr.ref = ib.instr();
    newInstrAbstr.idx = candidates.size() - 1;
    for (auto [idx, prefix] : Range{matchingPrefixes}.enumerate()) {
      auto val = prefix.get(build, false);
      ib.addRef(build.buildZExt(maxPrefixSize + 1, val));
      auto ref = prefix.frags.front().ref;
      if (ref.is<ObjRef<Wire>>()) {
        auto it = operands.find(ref);
        newInstrAbstr.operands.emplace_back(idx, it.val());
        auto &uses = operandUses[it.val()];
        uses.emplace_back(newInstrAbstr.idx);
      }
    }
    worklist.emplace_back(newInstrAbstr.idx);

    autoDbgInfo.popDebugInfo();
    autoDbgInfo.popDebugInfo();

    build.setInsertPoint(lhsAbstr.ref);
    auto lhsIdxs = Range{intersectOps}.transform(
        [](size_t, auto pair) { return pair.first; });
    rebuildInstr(lhsAbstr, matchingPrefixes, lhsCovered, lhsIdxs, defW);

    build.setInsertPoint(rhsAbstr.ref);
    auto rhsIdxs = Range{intersectOps}.transform(
        [](size_t, auto pair) { return pair.second; });
    rebuildInstr(rhsAbstr, matchingPrefixes, rhsCovered, rhsIdxs, defW);

    return true;
  }

  DenseMap<DynObjRef, uint32_t> operands;
  struct Use {
    uint16_t instr;
  };
  std::vector<SmallVec<Use, 4>> operandUses;
  SmallVec<AbstractInstr, 16> candidates;
  SmallVec<uint32_t, 32> worklist;

  void runOnProcess(ProcessIRef proc) {
    operands.clear();
    operandUses.clear();
    operands.clear();
    worklist.clear();
    candidates.clear();

    for (auto instr : HierBlockRange{proc.block()}) {
      if (instr.isOpc(OP_ADD)) {
        auto &abstr = candidates.emplace_back();
        abstr.ref = instr;
        abstr.idx = candidates.size() - 1;

        for (auto [opIdx, op] : Range{instr.others()}.enumerate()) {
          if (!op->is<WireRef>())
            continue;
          auto wire = op->as<WireRef>();
          auto [aliases, change] = bitAlias.getReprAliases(wire);
          auto &front = aliases.frags.front();
          // we use the first fragment's ref as the operand for heuristic
          // matching. higher fragments (if any) will be checked during detailed
          // matching.
          auto [found, id] = operands.findOrInsert(front.ref, operands.size());

          abstr.operands.emplace_back(uint16_t(opIdx),
                                      AbstractValue{uint16_t(id.val())});
        }
        Range{abstr.operands}.sort(
            [](auto lhs, auto rhs) { return lhs.second < rhs.second; });
      }
    }

    operandUses.resize(operands.size());
    for (auto [candidateIdx, abstr] : Range{candidates}.enumerate()) {
      for (auto [opIndex, op] : Range{abstr.operands}.enumerate()) {
        operandUses[op.second.id].emplace_back(candidateIdx);
      }
    }

    worklist.resize(candidates.size());
    for (uint i = 0; i < worklist.size(); i++)
      worklist[i] = i;

    while (!worklist.empty()) {
      auto candidateIdx = worklist.pop_back_val();
      auto &candidate = candidates[candidateIdx];

      if (!candidate.ref)
        continue;

      for (auto op : candidate.operands) {
        auto &arr = operandUses[op.second.id];
        for (auto &otherUse : arr) {
          if (otherUse.instr == candidate.idx) {
            continue;
          }
          auto success =
              tryCombineInstrs(candidate, candidates[otherUse.instr]);
          if (success)
            goto next;
        }
      }
    next:;
    }
  }

  void runOnModule(ModuleIRef mod) {
    for (auto proc : mod.procs())
      runOnProcess(proc);
  }

public:
  FuzzyCSEPass(HWContext &ctx)
      : ctx(ctx), bitAlias(ctx), build(ctx), controlFlowAnalysis(ctx),
        autoDbgInfo(ctx) {}
  void run() {
    bitAlias.clearCache();
    for (auto mod : ctx.activeModules()) {
      runOnModule(mod.iref());
    }
  }
};

}; // namespace dyno
