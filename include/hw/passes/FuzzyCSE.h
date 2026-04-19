#pragma once

#include "dyno/Context.h"
#include "dyno/HierBlockIterator.h"
#include "dyno/Opcode.h"
#include "dyno/Pass.h"
#include "hw/AutoDebugInfo.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/analysis/BitAliasAnalysis.h"
#include "hw/analysis/ControlFlow.h"
#include "support/Bits.h"
#include "support/Debug.h"
#include "support/DynBitSet.h"
#include "support/Tuple.h"
#include "support/Utility.h"
#include <cassert>

namespace dyno {

class FuzzyCSEPass : public Pass<FuzzyCSEPass> {
  Context &ctx;
  BitAliasAnalysis bitAlias;
  HWInstrBuilder build;
  ControlFlowAnalysis controlFlowAnalysis;
  TempBindVal<AutoCopyDebugInfoStack> autoDbgInfo;

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
    unsigned idx;
  };

public:
#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM)                                      \
  FIELD(uint32_t, minSharedBitsForMerge, 4)                                    \
  FIELD(uint32_t, maxSharedBitsForMerge, 0xFFFFFFFF)                           \
  FIELD(uint32_t, minSharedConstantBits, 6)                                    \
  FIELD(DialectOpcode, opToShare, OP_ADD)
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
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

  static size_t numIntersect(ArrayRef<std::pair<uint16_t, AbstractValue>> lhs,
                             ArrayRef<std::pair<uint16_t, AbstractValue>> rhs) {
    size_t i = 0;
    size_t j = 0;
    size_t rv = 0;
    while (i != lhs.size() && j != rhs.size()) {
      if (lhs[i].second == rhs[j].second) {
        i++;
        j++;
        rv++;
      } else if (lhs[i].second < rhs[j].second) {
        i++;
      } else
        j++;
    }
    return rv;
  }
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

    // if the shared range makes for constant/identity, don't share
    BigInt::rangeSelectOp4S(diff, lhsLast, 0, matchingBits);
    if (diff.valueEqualsS(0))
      return 0;
    if (config.opToShare.is(OP_AND, OP_OR) && diff.valueEqualsS(-1))
      return 0;

    return matchingBits;
  }

  auto rebuildInstr(AbstractInstr &instr,
                    ArrayRef<RegisterValue> matchingPrefixes,
                    const DynSymbSet<SmallVec<uint64_t, 1>, 1> &covered,
                    IsRange auto matchedIdxs, HWValue sharedSum) {
    // Rebuild the two instructions, with (1) adding the shared sum as an
    // additional operand and (2) replacing all matched operand fragments with
    // neutral element.
    auto tok = autoDbgInfo->addWithToken(instr.ref);
    SmallVec<HWValue, 8> lhsNewOps;
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

      if (*value.getNumBits() == matchedLen)
        continue;

      value = build.buildSplice(value, *value.getNumBits() - matchedLen,
                                matchedLen);
      ConstantRef neutral;
      auto cbuild = ConstantBuilder{ctx.getStore<Constant>()};
      if (config.opToShare == OP_AND)
        neutral = cbuild.ones(matchedLen);
      else
        neutral = cbuild.zero(matchedLen);

      value = build.buildConcat(value, neutral);
      lhsNewOps.emplace_back(value);
    }
    auto outBits = *instr.ref.def(0)->as<WireRef>().getNumBits();
    lhsNewOps.emplace_back(build.buildResize(sharedSum, outBits, false));
    auto newVal = build.buildCommutative(config.opToShare, lhsNewOps);
    instr.ref.def(0)->as<WireRef>().replaceAllUsesWith(newVal);

    // Delete all uses of this instruction (todo: better data structure or don't
    // reimplement def use)
    for (auto op : instr.operands) {
      auto &uses = operandUses[op.second.id];
      auto useIt =
          Range{uses}.find_if([&](Use use) { return use.instr == instr.idx; });
      assert(useIt != uses.end());
      uses.erase_unordered(useIt);
    }

    build.destroyInstr(instr.ref);
    instr.ref = nullref;
    bitAlias.clearCache();
    return newVal;
  }

  auto addToAbstractDefUse(HWValue newVal) {
    // Insert new instruction into abstract def/use
    if (auto w = newVal.as<WireRef>()) {
      auto instr = w.getDefI();

      assert(Range{candidates}.find_if(
                 [&](auto c) { return c.ref == instr; }) == candidates.end());

      auto &abstr = candidates.emplace_back();
      abstr.idx = candidates.size() - 1;
      abstr.ref = instr;
      for (auto [opIdx, op] : instr.others().enumerate()) {
        auto w = op->dyn_as<WireRef>();
        if (!w)
          continue;
        convertToAbstractOperand(w, opIdx, abstr);
      }
      Range{abstr.operands}.sort(
          [](auto lhs, auto rhs) { return lhs.second < rhs.second; });
      worklist.emplace_back(abstr.idx);
    }
  }

  bool tryCombineInstrs(AbstractInstr &lhsAbstr, AbstractInstr &rhsAbstr) {
    assert(&lhsAbstr != &rhsAbstr);
    assert(lhsAbstr.ref != rhsAbstr.ref);
    auto opc = lhsAbstr.ref.getDialectOpcode();

    // we call this based on heuristic match of one operand.
    SmallVec<std::pair<uint16_t, uint16_t>, 4> intersectOps;
    intersect(intersectOps, ArrayRef{lhsAbstr.operands},
              ArrayRef{rhsAbstr.operands});
    auto matchingConstBits = intersectConstants(lhsAbstr.ref, rhsAbstr.ref);

    assert(intersectOps.size() != 0 && "heuristic not working?");
    if ((intersectOps.size() +
         (matchingConstBits >= config.minSharedConstantBits)) < 2)
      return false;

    DynSymbSet<SmallVec<uint64_t, 1>, 1> lhsCovered, rhsCovered;
    lhsCovered.resize(lhsAbstr.ref.getNumOthers());
    rhsCovered.resize(rhsAbstr.ref.getNumOthers());

    // find matching prefixes
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
        matchingPrefixes.pop_back();
        it = intersectOps.erase(it);
        continue;
        // or bail?
        // return false;
      }
      lhsCovered[lhsOpIdx] = 1;
      rhsCovered[rhsOpIdx] = 1;
      ++it;
    }

    // don't merge tiny ops
    if (maxPrefixSize < config.minSharedBitsForMerge)
      return false;
    if (maxPrefixSize > config.maxSharedBitsForMerge)
      return false;

    assert(matchingPrefixes.size() == intersectOps.size());

    if (matchingConstBits >= config.minSharedConstantBits) {
      // constants are always last in operands
      auto lhsLastOpIdx = lhsAbstr.ref.getNumOthers() - 1;
      auto rhsLastOpIdx = rhsAbstr.ref.getNumOthers() - 1;
      lhsCovered[lhsLastOpIdx] = 1;
      rhsCovered[rhsLastOpIdx] = 1;
      auto constVal = lhsAbstr.ref.other(lhsLastOpIdx)->as<ConstantRef>();
      ConstantRef truncVal =
          ConstantBuilder{ctx.getStore<Constant>()}.val(constVal).resize(
              matchingConstBits);
      assert(!truncVal.valueEquals(0));
      assert(!config.opToShare.is(OP_AND, OP_OR) || !truncVal.valueEqualsS(-1));
      intersectOps.emplace_back(lhsLastOpIdx, rhsLastOpIdx);
      matchingPrefixes.emplace_back(
          RegisterValue{truncVal, truncVal.getNumBits(), 0, 0, nullopt});
      maxPrefixSize = std::max(maxPrefixSize, matchingConstBits);
    }

    uint32_t sharedInstSize = maxPrefixSize;
    if (config.opToShare == OP_ADD)
      sharedInstSize += 1;

    // if we end up with less than one shared op prefix, bail
    if (matchingPrefixes.size() <= 1)
      return false;

    DYNO_DBG({
      dbgs() << "matched instrs:\n";
      HWPrinter print{dbgs()};
      print.printInstr(lhsAbstr.ref, ctx);
      print.printInstr(rhsAbstr.ref, ctx);
    })

    autoDbgInfo->pushDebugInfo(lhsAbstr.ref);
    autoDbgInfo->pushDebugInfo(rhsAbstr.ref);

    auto parentBlock = controlFlowAnalysis.findSharedParentBlock(
        HWInstrRef{lhsAbstr.ref}.parentBlock(ctx),
        HWInstrRef{rhsAbstr.ref}.parentBlock(ctx));
    build.setInsertPoint(parentBlock.begin());

    SmallVec<HWValue, 8> sharedOperands;
    AbstractInstr newInstrAbstr;
    newInstrAbstr.idx = candidates.size();
    for (auto [idx, prefix] : Range{matchingPrefixes}.enumerate()) {
      auto val = prefix.get(build, false);
      sharedOperands.emplace_back(build.buildZExt(sharedInstSize, val));
    }
    // assert(Range{sharedOperands}.any([](HWValue v) {
    //   return v.is<WireRef>();
    // }) && "only constants?");
    auto defVal = build.buildCommutative(opc, sharedOperands);
    auto defW = defVal.dyn_as<WireRef>();

    // bail if the builder folded the instruction, can probably
    // be done a bit better
    if (!defW || !defW.getDefI().isOpc(opc) ||
        // todo: more efficient
        Range{candidates}.find_if([&](auto &cand) {
          return cand.ref == defW.getDefI();
        }) != candidates.end()) {
      DYNO_DBG({ dbgs() << "builder folded, bailing\n"; })
      // we may have created a couple of zexts, leave these for DCE to clean up
      return false;
    }
    newInstrAbstr.ref = defW.getDefI();

    for (auto [idx, prefix] : Range{matchingPrefixes}.enumerate()) {
      auto ref = prefix.frags.front().ref;
      if (ref.is<ObjRef<Wire>>()) {
        auto [_, it] = operands.findOrInsert(ref, operands.size());
        newInstrAbstr.operands.emplace_back(idx, it.val());
        auto &uses = operandUses[it.val()];
        uses.emplace_back(newInstrAbstr.idx);
      }
    }

    // do we need to re-sort?
    Range{newInstrAbstr.operands}.sort(
        [](auto lhs, auto rhs) { return lhs.second < rhs.second; });
    worklist.emplace_back(newInstrAbstr.idx);

    autoDbgInfo->popDebugInfo();
    autoDbgInfo->popDebugInfo();

    build.setInsertPoint(lhsAbstr.ref);
    auto lhsIdxs = Range{intersectOps}.transform(
        [](size_t, auto pair) { return pair.first; });
    auto newLHS =
        rebuildInstr(lhsAbstr, matchingPrefixes, lhsCovered, lhsIdxs, defW);

    build.setInsertPoint(rhsAbstr.ref);
    auto rhsIdxs = Range{intersectOps}.transform(
        [](size_t, auto pair) { return pair.second; });
    auto newRHS =
        rebuildInstr(rhsAbstr, matchingPrefixes, rhsCovered, rhsIdxs, defW);

    DYNO_DBG({
      dbgs() << "replaced with:\n";
      HWPrinter print{dbgs()};
      dbgs() << "shared: ";
      print.printInstr(newInstrAbstr.ref, ctx);
      dbgs() << "new lhs: ";
      if (auto w = newLHS.as<WireRef>())
        print.printInstr(w.getDefI(), ctx);
      else
        dbgs() << "<none>\n";
      dbgs() << "new rhs: ";
      if (auto w = newRHS.as<WireRef>())
        print.printInstr(w.getDefI(), ctx);
      else
        dbgs() << "<none>\n";
    })

    candidates.emplace_back(newInstrAbstr);
    if (newLHS != defW)
      addToAbstractDefUse(newLHS);
    if (newRHS != defW)
      addToAbstractDefUse(newRHS);
    return true;
  }

  DenseMap<DynObjRef, uint32_t> operands;
  struct Use {
    uint16_t instr;
  };
  Vec<SmallVec<Use, 4>> operandUses;
  SmallVec<AbstractInstr, 16> candidates;
  SmallVec<uint32_t, 32> worklist;

  void convertToAbstractOperand(WireRef wire, uint16_t opIdx,
                                AbstractInstr &abstr) {

    auto [aliases, change] = bitAlias.getReprAliases(wire);
    auto &front = aliases.frags.front();
    // we use the first fragment's ref as the operand for heuristic
    // matching. higher fragments (if any) will be checked during detailed
    // matching.

    // todo: do not include trailing zeros in here, we don't want to match
    // everything that has a trailing zero fragment.
    auto [found, id] = operands.findOrInsert(front.ref, operands.size());

    abstr.operands.emplace_back(uint16_t(opIdx),
                                AbstractValue{uint16_t(id.val())});
    if (id.val() >= operandUses.size())
      operandUses.resize(ceil_to_pow2(id.val() + 1));
    operandUses[id.val()].emplace_back(abstr.idx);
  }

  void runOnProcess(ProcessIRef proc) {
    operands.clear();
    operandUses.clear();
    operands.clear();
    worklist.clear();
    candidates.clear();

    for (auto instr : HierBlockRange{proc.block()}) {
      if (instr.isOpc(config.opToShare)) {
        assert(Range{candidates}.find_if([&](AbstractInstr &abstr) {
          return abstr.ref == instr;
        }) == candidates.end());
        auto &abstr = candidates.emplace_back();
        abstr.ref = instr;
        abstr.idx = candidates.size() - 1;

        for (auto [opIdx, op] : Range{instr.others()}.enumerate()) {
          if (!op->is<WireRef>())
            continue;
          auto wire = op->as<WireRef>();
          convertToAbstractOperand(wire, opIdx, abstr);
        }
        Range{abstr.operands}.sort(
            [](auto lhs, auto rhs) { return lhs.second < rhs.second; });
      }
    }

    // operandUses.resize(operands.size());
    // for (auto [candidateIdx, abstr] : Range{candidates}.enumerate()) {
    //   for (auto [opIndex, op] : Range{abstr.operands}.enumerate()) {
    //     operandUses[op.second.id].emplace_back(candidateIdx);
    //   }
    // }

    worklist.resize(candidates.size());
    for (unsigned i = 0; i < worklist.size(); i++)
      worklist[i] = i;

    while (!worklist.empty()) {
      auto candidateIdx = worklist.pop_back_val();
      auto &candidate = candidates[candidateIdx];

      if (!candidate.ref)
        continue;

      for (auto op : candidate.operands) {
        auto &arr = operandUses[op.second.id];

        SmallVec<std::pair<uint32_t, uint32_t>, 4> subCandidates;

        for (auto [otherUseIdx, otherUse] : Range{arr}.enumerate()) {
          if (otherUse.instr == candidate.idx) {
            continue;
          }
          auto &other = candidates[otherUse.instr];
          assert(candidate.ref);
          assert(other.ref);

          auto score = numIntersect(candidate.operands, other.operands) +
                       (intersectConstants(candidate.ref, other.ref) >
                        config.minSharedConstantBits);
          if (score > 1)
            subCandidates.emplace_back(otherUseIdx, score);
        }

        Range{subCandidates}.stable_sort(
            [](auto &a, auto &b) { return a.second > b.second; });
        for (auto [otherUseIdx, score] : Range{subCandidates}) {
          auto &otherUse = arr[otherUseIdx];
          auto &other = candidates[otherUse.instr];

          auto success = tryCombineInstrs(candidate, other);
          if (success) {
            goto next;
          }
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
  FuzzyCSEPass(Context &ctx)
      : ctx(ctx), bitAlias(ctx), build(ctx), controlFlowAnalysis(ctx) {}
  static auto make(Context &ctx) { return FuzzyCSEPass{ctx}; }

  void runWrapper(auto &&runFunc) {
    auto tok = autoDbgInfo.emplace(ctx);
    bitAlias.clearCache();
    runFunc();
  }
  void run() {
    runWrapper([&] {
      for (auto mod : ctx.getCtx<HWDialectContext>().activeModules()) {
        runOnModule(mod.iref());
      }
    });
  }
  void runModule(ModuleIRef mod) {
    runWrapper([&] { runOnModule(mod); });
  }

  static constexpr auto runFuncs =
      mk_tuple(&FuzzyCSEPass::runModule, &FuzzyCSEPass::run);
};

}; // namespace dyno
