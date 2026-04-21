#pragma once

#include "dyno/Constant.h"
#include "dyno/Context.h"
#include "dyno/CustomInstr.h"
#include "dyno/HierBlockIterator.h"
#include "dyno/IDs.h"
#include "dyno/Instr.h"
#include "dyno/MutInstr.h"
#include "dyno/Obj.h"
#include "dyno/Opcode.h"
#include "dyno/Pass.h"
#include "hw/FlipFlop.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Register.h"
#include "hw/Wire.h"
#include "hw/analysis/BitAliasAnalysis.h"
#include "hw/analysis/DemandedBits.h"
#include "hw/analysis/KnownBits.h"
#include "hw/analysis/LoopbackAnalysis.h"
#include "hw/analysis/RegisterValue.h"
#include "hw/passes/FlipFlopInference.h"
#include "op/IDs.h"
#include "op/StructuredControlFlow.h"
#include "support/ArrayRef.h"
#include "support/Bits.h"
#include "support/Debug.h"
#include "support/DenseMap.h"
#include "support/DynBitSet.h"
#include "support/ErrorRecovery.h"
#include "support/Ranges.h"
#include "support/SmallVec.h"
#include "support/Tuple.h"
#include "support/Utility.h"
#include <algorithm>
#include <iterator>
#include <type_traits>
namespace dyno {

class InstCombinePass : public Pass<InstCombinePass> {
  Context &ctx;
  SmallVec<InstrRef, 128> worklist;
  SmallVec<InstrRef, 8> currentMatched;
  SmallVec<OperandRef, 4> currentReplaced;
  ConstantBuilder cbuild;

  // todo: outside of pass
  KnownBitsAnalysis knownBits;
  DemandedBitsAnalysis demandedBits;
  BitAliasAnalysis bitAlias;
  DeriveBitsAnalysis deriveBits;
  LoopbackAnalysis loopbackAnalysis;

public:
  using TaggedIRef = CustomInstrRef<InstrRef, uint64_t>;

  // this is a bool that contains information on what pattern matched (function)
  struct PatBool {
    const char *function;
    explicit operator bool() { return !!function; }
    PatBool() = default;
    explicit PatBool(const char *function) : function(function) {}

    // allow bool construction only from false
    consteval PatBool(bool b) : function(nullptr) { assert(!b); }
  };

#define PAT_TRUE PatBool{__FUNCTION__}
#define PAT_FALSE PatBool{nullptr}
#define PAT_BOOL(x) ((x) ? PAT_TRUE : PAT_FALSE)

#define PAT_LIST

#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM)                                      \
  FIELD(bool, fuseCommutative, true)                                           \
  FIELD(bool, liftMUX, false)                                                  \
  FIELD(bool, simplifyDeMorgan, true)                                          \
  FIELD(bool, boolExprSimplify, true)                                          \
  FIELD(bool, removeAssumes, false)                                            \
  FIELD(bool, findFlipFlopEnables, false)                                      \
  FIELD(bool, muxToOneHotMux, false)
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
  Config config;

private:
  static bool generated(Context &ctx, Config &config,
                        SmallVecImpl<InstrRef> &matched,
                        SmallVecImpl<OperandRef> &replaced, HWInstrRef);

  PatBool knownBitsConstProp(InstrRef instr) {
    auto wire = instr.def(0)->as<WireRef>();
    auto known = knownBits.getKnownBits(wire);
    if (known.getIs4S())
      return false;
    assert(known.getNumBits() == wire.getNumBits());
    replaceUses(wire, cbuild.val(known).get());
    deleteMatchedInstr(instr);
    return PAT_TRUE;
  }

  void deleteMatchedInstr(InstrRef instr) {
    TaggedIRef{instr}.get() = 1;
    currentMatched.emplace_back(instr);
  }

  void replaceUses(WireRef wire, HWValue newVal) {
    assert(newVal.getNumBits() == wire.getNumBits());
    knownBits.replaceAt(wire, newVal);
    bitAlias.replaceAt(wire, newVal);

    size_t pos = currentReplaced.size();
    wire.replaceAllUsesWith(
        newVal, [&](OperandRef ref) { currentReplaced.emplace_back(ref); });

    for (size_t i = pos; i < currentReplaced.size(); i++) {
      auto instr = currentReplaced[i].instr();
      for (auto def : instr.defs()) {
        if (!def->is<HWValue>())
          continue;
        knownBits.recomputeAt(def->as<HWValue>());
        bitAlias.recomputeAt(def->as<HWValue>());
      }
    }
  }
  void replaceUse(OperandRef ref, HWValue newVal) {
    assert(newVal.getNumBits() == ref->as<WireRef>().getNumBits());
    knownBits.replaceAt(ref->as<WireRef>(), newVal);
    bitAlias.replaceAt(ref->as<WireRef>(), newVal);

    ref.replace(newVal);
    currentReplaced.emplace_back(ref);

    auto instr = currentReplaced.back().instr();
    for (auto def : instr.defs()) {
      if (!def->is<HWValue>())
        continue;
      knownBits.recomputeAt(def->as<HWValue>());
      bitAlias.recomputeAt(def->as<HWValue>());
    }
  }

  PatBool findFlipFlopEnables(FlipFlopIRef instr) {
    if (!config.findFlipFlopEnables)
      return false;
    auto use = instr.q().getSingleUse();
    if (!use || !use->instr().isOpc(HW_STORE))
      return false;
    auto store = use->instr().as<StoreIRef>();
    if (!store.isFullReg())
      return false;
    auto load = store.reg().iref().getSingleLoad();
    if (!load || !load.as<LoadIRef>().isFullReg())
      return false;

    auto part = loopbackAnalysis.get(instr.d(), load.as<LoadIRef>().value());
    if (Range{part.frags}.all([](auto frag) { return !frag; }))
      return false;

    HWInstrBuilder build{ctx, instr};
    OperandVec<HWValue> concat(ctx, 1, part.frags.size());
    concat.emplace_back(instr.q());

    for (auto &frag : part.frags) {
      HWValue d = build.buildSplice(instr.d(), frag.len, frag.dstAddr);
      HWValue en = instr.clkEnRaw();

      if (frag) {
        if (frag.size() == 0) {
          // loopback always active. todo: delete FF here or somewhere else assuming
          // no initval/rst behavior.
          en = ConstantRef::fromBool(false);
        } else {
          auto newEn = build.buildNot(build.buildAnd(Range{frag}.resolve(ctx)));
          d = build.buildAssume(d, newEn);
          en = build.buildAnd(en, newEn);
        }
      }

      auto val = build.buildFlipFlop(instr.clk(), d, en, instr.rsts());
      concat.emplace_back(val);
    }
    concat.others().do_reverse();

    instr.def().replace(FatDynObjRef<>{nullref});
    deleteMatchedInstr(instr);
    build.buildConcat(std::move(concat));

    return PAT_TRUE;
  }

  PatBool simplifyFlipFlopResets(FlipFlopIRef instr) {
    auto compare = [](auto lhs, auto rhs) {
      auto lhsV = std::get<1>(lhs);
      auto rhsV = std::get<1>(rhs);
      if (lhsV.template is<WireRef>() && rhsV.template is<WireRef>())
        return lhsV.getObjID() < rhsV.getObjID();
      else if (lhsV.template is<WireRef>() || rhsV.template is<WireRef>())
        return !lhsV.template is<WireRef>();
      else {
        // run 2 state compare on possibly 4 state numbers, we just want an
        // order
        if (lhsV.template as<ConstantRef>().getIs4S() !=
            rhsV.template as<ConstantRef>().getIs4S())
          return !lhsV.template as<ConstantRef>().getIs4S();
        return BigInt::icmpUnsignedLessOp(lhsV.template as<ConstantRef>(),
                                          rhsV.template as<ConstantRef>());
      }
    };

    HWInstrBuilder build{ctx, instr};
    auto rebuild = [&](MutArrayRef<std::tuple<HWValue, HWValue>> rstVals) {
      auto ib =
          build.buildInstrRaw(instr.getDialectOpcode(), instr.getNumOperands());
      ib.addRef(instr.def()->fat()).other();
      instr.def().replace(FatDynObjRef{nullref});
      ib.addRefs(instr.others()
                     .subrange(0, FlipFlopIRef::numBaseOperands)
                     .as<HWValue>());
      Range{rstVals}.sort(compare);
      for (auto [rstEn, rstVal] : rstVals)
        ib.addRef(rstEn).addRef(rstVal);
      deleteMatchedInstr(instr);
    };

    if (!instr.rsts().is_sorted(compare)) {
      SmallVec<std::tuple<HWValue, HWValue>, 4> rstVals(instr.rsts());
      rebuild(rstVals);
      return PAT_TRUE;
    }

    SmallDenseMap<DynObjRef, SmallVec<uint32_t, 2>, 4> map;
    for (auto [i, pair] : Range{instr.rsts()}.enumerate()) {
      if (auto asConst = std::get<0>(pair).dyn_as<ConstantRef>();
          asConst && asConst.valueEquals(0))
        continue;
      map[std::get<1>(pair)].emplace_back(i);
    }
    if (map.size() == instr.numRsts())
      return false;

    SmallVec<std::tuple<HWValue, HWValue>, 4> rstVals;
    for (auto [val, idxs] : map) {
      auto sel = instr.rstRaw(idxs.front());
      for (auto other : Range{idxs}.drop_front()) {
        sel = build.buildOr(instr.rstRaw(other));
      }
      rstVals.emplace_back(sel, ctx.resolve(val));
    }
    rebuild(rstVals);
    return PAT_TRUE;
  }

  PatBool simplifyFlipFlopEnable(FlipFlopIRef instr) {
    // constant zero enable
    // could still be used as latch with multiple
    // rests (or change state away from init with single)
    if (instr.numRsts() == 0)
      if (auto c = instr.clkEn().dyn_as<ConstantRef>(); c && c.valueEquals(0)) {
        auto w = instr.def()->as<WireRef>();
        // todo: init val
        w.replaceAllUsesWith(cbuild.undef(*w.getNumBits()));
        deleteMatchedInstr(instr);
        return PAT_TRUE;
      }

    auto d = instr.d().dyn_as<WireRef>();
    if (!d)
      return false;

    HWInstrBuilder build{ctx, instr};
    auto rebuildWithEn = [&](HWValue newEn) {
      newEn = build.buildAnd(newEn, instr.clkEnRaw());
      auto d = instr.d();
      // todo: ff en as implicit assume rather than explicit
      d = build.buildAssume(d, newEn);

      auto ib =
          build.buildInstrRaw(instr.getDialectOpcode(), instr.getNumOperands());
      ib.addRef(instr.def()->fat()).other();
      instr.def().replace(FatDynObjRef{nullref});
      ib.addRef(instr.clk());
      ib.addRef(d);
      ib.addRef(newEn);
      ib.addRefs(
          instr.others().subrange(FlipFlopIRef::numBaseOperands).as<HWValue>());
      deleteMatchedInstr(instr);
    };

    RegisterIRef qReg = nullref;
    for (auto use : instr.q().uses()) {
      if (auto asSt = use.instr().dyn_as<StoreIRef>();
          asSt && !asSt.isOpc(HW_STORE_DEFER) &&
          asSt.reg().iref().getSingleStore()) {
        // not valid with full control flow, assuming there's no flip flops
        // while there's stil control flow.
        qReg = asSt.reg().iref();
      }
    }

    if (d.getDefI().isOpc(HW_MUX)) {
      auto mux = d.getDefI();

      auto range = mux.others().drop_front().as<HWValue>();

      // try plain wire loopback, then register loopback
      auto idx = range.find(instr.q()) - range.begin();
      if (qReg && idx == range.size()) {
        idx = range.find_if([&](auto val) {
          return FlipFlopInferencePass::isQLoad(qReg, val);
        }) - range.begin();
      }
      if (idx == range.size())
        return false;
      auto val = mux.other(0)->as<HWValue>();
      if (idx == 0)
        val = build.buildNot(val);
      rebuildWithEn(val);
      return PAT_TRUE;
    }

    if (d.getDefI().isOpc(HW_ONEHOT_MUX)) {
      auto mux = d.getDefI();

      auto range = mux.others().drop_front().step(2).as<HWValue>();

      // try plain wire loopback, then register loopback
      auto idx = range.find(instr.q()) - range.begin();
      if (qReg && idx == range.size()) {
        idx = range.find_if([&](auto val) {
          return FlipFlopInferencePass::isQLoad(qReg, val);
        }) - range.begin();
      }
      if (idx == range.size())
        return false;
      auto val = mux.other(2 * idx)->as<HWValue>();
      rebuildWithEn(build.buildNot(val));
      return PAT_TRUE;
    }

    return false;
  }

  bool boolExprSimplifySub(OperandRef root) {
    SmallVec<OperandRef, 16> stack{root};
    bool change = false;
    while (!stack.empty()) {
      auto ref = stack.pop_back_val();
      auto wire = ref->as<WireRef>();

      bool singleUse = wire.getNumUses() == 1;

      auto known = deriveBits.knownBits.get(wire);
      assert(known.getNumBits() == wire.getNumBits());
      if (!known.getIs4S()) {
        replaceUse(ref, ConstantBuilder{ctx.getStore<Constant>()}
                            .val(std::move(known))
                            .get());
        change = true;
      }

      if (!singleUse)
        continue;

      auto instr = wire.getDefI();
      for (auto op : instr.others()) {
        if (op->is<WireRef>())
          stack.emplace_back(op);
      }
    }
    return change;
  }

  // Optimize and/or operands under assumption all other operands are 1/0
  // respectively.
  PatBool boolExprSimplify(InstrRef instr) {
    if (!config.boolExprSimplify)
      return false;

    HWInstrBuilder build{ctx, instr};
    assert(instr.isOpc(OP_AND, OP_OR) &&
           instr.def(0)->as<WireRef>().getNumBits() == 1);

    if (instr.getNumOperands() > 16)
      return false;

    auto trueV = instr.isOpc(OP_AND) ? true : false;

    bool change = false;

    for (auto op : instr.others()) {
      bool contradiction = false;
      for (auto other : instr.others()) {
        if (other.getNum() == op.getNum())
          continue;
        contradiction |= !deriveBits.propKnownValueUp(
            other->as<WireRef>(), ConstantRef::fromBool(trueV));
        if (contradiction)
          break;
      }

      auto known = deriveBits.knownBits.getKnownBits(op->as<HWValue>());
      // contradiction setting all other operands, or operand is known and short
      // circuits -> whole instr is known
      if (contradiction || known.valueEquals(!trueV)) {
        replaceUses(instr.def(0)->as<WireRef>(), ConstantRef::fromBool(!trueV));
        deleteMatchedInstr(instr);
        deriveBits.clearCache();
        return PAT_TRUE;
      } //  operand is known but not short circuit -> delete
      else if (known.valueEquals(trueV)) {
        auto newV = build.buildCommutative(
            instr.getDialectOpcode(),
            instr.others()
                .filter([&](auto x) { return x.getNum() != op.getNum(); })
                .deref()
                .as<HWValue>());
        replaceUses(instr.def(0)->as<WireRef>(), newV);
        deleteMatchedInstr(instr);
        deriveBits.clearCache();
        return PAT_TRUE;
      } else {
        change |= boolExprSimplifySub(op);
      }

      deriveBits.clearCache();
    }
    deriveBits.clearCache();
    currentMatched.emplace_back(instr);
    return PAT_BOOL(change);
  }

  PatBool simplifyAssume(InstrRef instr) {
    if (config.removeAssumes) {
      replaceUses(instr.def()->as<WireRef>(), instr.other(0)->as<HWValue>());
      deleteMatchedInstr(instr);
      return PAT_TRUE;
    }

    if (!instr.other(0)->is<WireRef>() || !instr.other(1)->is<WireRef>())
      return false;
    auto given = instr.other(1)->as<WireRef>();
    assert(given.getNumBits() == 1);
    auto contradict =
        !deriveBits.propKnownValueUp(given, BigInt::fromU64(1, 1));
    assert(!contradict && "contradicting assume");

    auto rv = boolExprSimplifySub(instr.other(0));
    deriveBits.clearCache();
    if (rv)
      currentMatched.emplace_back(instr);
    return PAT_BOOL(rv);
  }

  PatBool optimizeOneHotMuxUndef(InstrRef instr) {
    uint32_t idx;
    for (auto [sel, val] : Range{instr.others()}.pairwise()) {
      if (auto asConst = val->dyn_as<ConstantRef>()) {
        if (asConst.allBitsUndef()) {
          idx = sel.getNum();
          goto found;
        }
      }
    }
    return false;
  found:;

    HWInstrBuilder build{ctx, instr};
    auto val = build.buildNot(instr.operand(idx)->as<HWValue>());

    auto range =
        instr.others()
            .pairwise()
            .filter([&](auto pair) { return pair.first.getNum() != idx; })
            .transform([&](size_t, auto pair) {
              auto newCond =
                  build.buildAssume(pair.first->template as<HWValue>(), val);
              return std::make_pair(newCond,
                                    pair.second->template as<HWValue>());
            });
    SmallVec<std::pair<HWValue, HWValue>, 32> cases;
    cases.reserve(instr.getNumOthers() / 2 - 1);
    cases.push_back_range(range);
    replaceUses(instr.def()->as<WireRef>(), build.buildOneHotMux(cases));
    deleteMatchedInstr(instr);
    return PAT_TRUE;
  }

  PatBool mergeOneHotMux(InstrRef root) {
    SmallVec<std::pair<InstrRef, HWValue>, 4> stack{{root, nullref}};
    SmallVec<std::pair<HWValue, HWValue>, 32> cases;
    auto oldDefW = root.def(0)->as<WireRef>();

    HWInstrBuilder build{ctx, root};

    bool change = false;
    while (!stack.empty()) {
      auto [instr, prefix] = stack.pop_back_val();

      for (auto [sel, val] : instr.others().pairwise()) {
        if (auto asWire = val->dyn_as<WireRef>()) {
          auto defI = asWire.getSingleDef()->instr();
          if (defI.isOpc(HW_ONEHOT_MUX) && asWire.hasSingleUse()) {
            auto newPrefix = prefix ? build.buildAnd(prefix, sel->as<HWValue>())
                                    : sel->as<HWValue>();
            stack.emplace_back(defI, newPrefix);
            deleteMatchedInstr(defI);
            change = true;
            continue;
          }
        }

        auto selPrefixed = sel->as<HWValue>();
        if (prefix)
          selPrefixed = build.buildAnd(prefix, selPrefixed);
        cases.emplace_back(selPrefixed, val->as<HWValue>());
        continue;
      }
    }
    if (!change)
      return false;

    auto newVal = build.buildOneHotMux(cases);
    replaceUses(oldDefW, newVal);
    deleteMatchedInstr(root);
    return PAT_TRUE;
  }

  PatBool optimizeOneHotMux(InstrRef instr) {
    // hash to find duplicates
    SmallDenseMap<DynObjRef, SmallVec<uint32_t, 2>, 16> map;
    for (auto [sel, val] : Range{instr.others()}.pairwise()) {
      // check if select is known
      auto known = knownBits.getKnownBits(sel->as<HWValue>());
      if (known.valueEquals(1)) {
        // found a one entry, remove the instr
        replaceUses(instr.def(0)->as<WireRef>(), val->as<HWValue>());
        deleteMatchedInstr(instr);
        return PAT_TRUE;
      } else if (known.valueEquals(0)) {
        continue;
      }

      map[val->thin()].emplace_back(sel.getNum());
    }
    if (map.size() == instr.getNumOthers() / 2)
      return false;

    HWInstrBuilder build{ctx, instr};
    SmallVec<std::pair<HWValue, HWValue>, 16> entries;

    for (auto [val, selIdxs] : map) {
      auto rng = Range{selIdxs}.transform([&](size_t, uint32_t idx) {
        return instr.operand(idx)->as<HWValue>();
      });
      auto mergedSel = build.buildOr(rng);
      entries.emplace_back(mergedSel, ctx.resolve(val));
    }

    HWValue newVal = build.buildOneHotMux(entries);
    replaceUses(instr.def(0)->as<WireRef>(), newVal);
    deleteMatchedInstr(instr);
    return PAT_TRUE;
  }

  PatBool reduceBitWidth(InstrRef instr) {
    // todo: this only handles a leading bits known region. should be
    // generalized to arbitrary regions.
    // todo: div and mod?
    uint32_t minLeading = UINT32_MAX;
    uint32_t sumLeading = 0;
    uint32_t absorbedLeading = 0;

    WireRef outWire = instr.def(0)->as<WireRef>();
    uint32_t originalBits = *outWire.getNumBits();

    SmallVec<BigInt, 4> bigInts;

    auto demanded = demandedBits.getDemandedBits(instr.def(0)->as<WireRef>());
    assert(demanded.getNumBits() == originalBits);
    auto numNonDemanded = BigInt::leadingZeros(demanded);
    bool reduceViaOutputs = numNonDemanded != 0;
    bool reduceViaInputs = true;

    auto combineLeading = [&](BigInt &known) {
      switch (*instr.getDialectOpcode()) {
      case *OP_ADD: {
        auto newLeading = BigInt::leadingZeros4SExact(known);
        if (newLeading <= 1)
          return false;
        newLeading--;
        minLeading = std::min(minLeading, newLeading);
        break;
      }

      case *OP_MUL: {
        auto newLeading = BigInt::leadingZeros4SExact(known);
        if (newLeading <= 1)
          return false;
        sumLeading += newLeading;
        auto fullBits = (instr.getNumOthers() * originalBits);
        minLeading = originalBits - (fullBits - sumLeading);
        if (originalBits < (fullBits - sumLeading))
          minLeading = 0;
        break;
      }
      case *OP_AND:
      case *OP_OR:
      case *OP_XOR: {
        uint32_t newAbsorbed = 0;
        if (instr.isOpc(OP_AND))
          newAbsorbed = BigInt::leadingBits4SExact(known, FourState::S0);
        else if (instr.isOpc(OP_OR))
          newAbsorbed = BigInt::leadingBits4SExact(known, FourState::S1);

        absorbedLeading = std::max(absorbedLeading, newAbsorbed);
        auto newLeading = BigInt::leadingNonUnk(known);
        // we can't early abort and/or because later operands might absorb
        if (newLeading == 0 && !instr.isOpc(OP_AND, OP_OR))
          return false;
        minLeading =
            std::max(absorbedLeading, std::min(minLeading, newLeading));
        break;
      }
      }
      return true;
    };

    for (auto op : instr.others()) {
      auto known = knownBits.getKnownBits(op->as<HWValue>());
      assert(known.getNumBits() == originalBits);

      bool cont = combineLeading(known);
      if (!cont) {
        reduceViaInputs = false;
        break;
      }

      bigInts.emplace_back(known);
    }

    if (minLeading == 0)
      reduceViaInputs = false;

    if (!reduceViaInputs && !reduceViaOutputs)
      return false;

    uint32_t activeBits =
        originalBits -
        std::max(numNonDemanded, reduceViaInputs ? minLeading : 0);
    activeBits = std::max(activeBits, 1u);
    if (activeBits == originalBits)
      return false;
    assert(activeBits < originalBits);

    bool sign = false;

    HWInstrBuilder build{ctx};
    build.setInsertPoint(instr);
    auto newVal = build.buildCommutative(
        instr.getDialectOpcode(),
        instr.others().transform([&](size_t, OperandRef ref) {
          return build.buildTrunc(activeBits, ref->as<HWValue>());
        }));

    build.setInsertPoint(instr);

    if (instr.isOpc(OP_AND, OP_OR, OP_XOR) && reduceViaInputs) {
      for (auto &bigInt : bigInts) {
        BigInt::rangeSelectOp4S(bigInt, bigInt, activeBits,
                                originalBits - activeBits);
      }
      void (*func)(BigInt &, const BigInt &, const BigInt &);
      switch (*instr.getDialectOpcode()) {
      case *OP_AND:
        func = BigInt::andOp4S<BigInt, BigInt>;
        break;
      case *OP_OR:
        func = BigInt::orOp4S<BigInt, BigInt>;
        break;
      case *OP_XOR:
        func = BigInt::xorOp4S<BigInt, BigInt>;
        break;
      default:
        dyno_unreachable("unknown opcode");
      }
      BigInt::reduce(bigInts[0], Range{bigInts}.drop_front(), func);
      build.buildInstrRaw(HW_CONCAT, 3)
          .addRef(outWire)
          .other()
          .addRef(cbuild.val(bigInts[0]).get())
          .addRef(newVal);
    } else {
      DialectOpcode opc;
      if (reduceViaInputs)
        opc = sign ? OP_SEXT : OP_ZEXT;
      else
        opc = OP_ANYEXT;
      assert(*outWire->numBits >= *newVal.getNumBits());
      build.buildInstrRaw(opc, 2).addRef(outWire).other().addRef(newVal);
    }

    // instr.def(0).replace(FatDynObjRef<>{nullref});
    deleteMatchedInstr(instr);
    return PAT_TRUE;
  }

  PatBool liftMUX(InstrRef instr) {
    if (!config.liftMUX)
      return false;
    auto selV = instr.other(0)->as<HWValue>();
    auto trueV = instr.other(1)->as<HWValue>();
    auto falseV = instr.other(2)->as<HWValue>();

    auto bits = *trueV.getNumBits();

    auto trueRepr = bitAlias.getReprAliases(trueV);
    auto falseRepr = bitAlias.getReprAliases(falseV);

    // auto diffs =
    //     diffRegisterValues(std::to_array({&trueRepr, &falseRepr}), false,
    //     true);

    SmallVec<HWValue, 8> outFrags;

    HWInstrBuilder build{ctx, instr};

    // do not change if no differences or if everything different
    // if (diffs.size() == 1 && diffs.front().addr() == 0 &&
    //    diffs.front().len() == bits)
    //  return false;

    auto seams =
        regValueFindCommonSeams(std::to_array({&trueRepr, &falseRepr}));
    assert(seams.size() >= 2);
    if (seams.size() == 2)
      return false;

    auto handle = [&](uint32_t addr, uint32_t len) {
      auto fragV = build.buildMux(selV, trueRepr.get(build, addr, len, false),
                                  falseRepr.get(build, addr, len, false));
      outFrags.emplace_back(fragV);
    };

    uint32_t last = 0;
    for (auto seam : Range{seams}.drop_front()) {
      auto len = seam - last;
      assert(len != 0);
      handle(last, len);
      last = seam;
      // auto fragV = build.buildMux(
      //     selV, trueRepr.get(build, diff.addr(), diff.len(), false),
      //     falseRepr.get(build, diff.addr(), diff.len(), false));
      // outFrags.emplace_back(fragV);
      // last = diff.addr() + diff.len();
    }
    if (auto len = bits - last)
      handle(last, len);

    std::reverse(outFrags.begin(), outFrags.end());
    auto out = build.buildConcat(outFrags);
    replaceUses(instr.def(0)->as<WireRef>(), out);
    deleteMatchedInstr(instr);
    return PAT_TRUE;
  }

  PatBool liftOneHotMUX(InstrRef instr) {
    if (!config.liftMUX)
      return false;

    // todo: remove when regular MUX version retired
    SmallVec<RegisterValue *, 16> reprsPtrs(reserve_tag,
                                            instr.getNumOthers() / 2);
    SmallVec<RegisterValue, 16> reprs(reserve_tag, instr.getNumOthers() / 2);
    for (auto [sel, val] : instr.others().deref().as<HWValue>().pairwise()) {
      reprsPtrs.emplace_back(&reprs.emplace_back(bitAlias.getReprAliases(val)));
    }

    auto bits = *instr.other(1)->as<HWValue>().getNumBits();

    SmallVec<HWValue, 8> outFrags;
    HWInstrBuilder build{ctx, instr};

    auto seams = regValueFindCommonSeams(reprsPtrs);
    assert(seams.size() >= 2);
    if (seams.size() == 2)
      return false;

    auto handle = [&](uint32_t addr, uint32_t len) {
      SmallVec<std::pair<HWValue, HWValue>, 16> vals;
      vals.reserve(reprs.size());
      for (auto [i, repr] : Range{reprs}.enumerate()) {
        vals.emplace_back(instr.other(2 * i)->as<HWValue>(),
                          repr.get(build, addr, len, false));
      }
      auto fragV = build.buildOneHotMux(vals);
      outFrags.emplace_back(fragV);
    };

    uint32_t last = 0;
    for (auto seam : Range{seams}.drop_front()) {
      auto len = seam - last;
      assert(len != 0);
      handle(last, len);
      last = seam;
      // auto fragV = build.buildMux(
      //     selV, trueRepr.get(build, diff.addr(), diff.len(), false),
      //     falseRepr.get(build, diff.addr(), diff.len(), false));
      // outFrags.emplace_back(fragV);
      // last = diff.addr() + diff.len();
    }
    if (auto len = bits - last)
      handle(last, len);

    std::reverse(outFrags.begin(), outFrags.end());
    auto out = build.buildConcat(outFrags);
    replaceUses(instr.def(0)->as<WireRef>(), out);
    deleteMatchedInstr(instr);
    return PAT_TRUE;
  }

  PatBool sinkMUX(InstrRef instr) {
    if (config.liftMUX)
      return false;
    assert(instr.isOpc(HW_CONCAT));

    SmallVec<std::pair<uint32_t, uint32_t>, 4> ranges;
    size_t lastI = 0;
    HWValue lastSel;
    bool active = false;

    auto commit = [&](size_t i) {
      if (!active || i == lastI)
        return;
      active = false;
      if (i - lastI >= 2)
        ranges.emplace_back(lastI, i - lastI);
      lastI = i;
    };

    constexpr bool allowEmptyRegions = true;

    for (auto [i, op] : Range{instr.others()}.enumerate()) {
      if (auto wire = op->dyn_as<WireRef>();
          wire && wire.getDefI().isOpc(HW_MUX) &&
          wire.getDefI().def(0)->as<WireRef>().hasSingleUse()) {
        auto mux = wire.getDefI();
        auto sel = mux.other(0)->as<HWValue>();
        if (sel != lastSel)
          commit(i);
        if (!active) {
          lastI = i;
          lastSel = sel;
        }
        active = true;
      } else
        commit(i);
    }
    commit(instr.getNumOthers());

    if (ranges.empty() || (!allowEmptyRegions && ranges.size() != 1))
      return false;

    HWInstrBuilder build{ctx, instr};

    SmallVec<HWValue, 8> vals;
    uint32_t lastEnd = 0;
    for (auto range : ranges) {
      if (range.first != lastEnd) {
        vals.push_back_range(Range{instr.other_begin() + lastEnd,
                                   instr.other_begin() + range.first}
                                 .deref()
                                 .as<HWValue>());
      }

      auto ops = Range{instr.other_begin() + range.first,
                       instr.other_begin() + range.first + range.second};

      SmallVec<HWValue, 4> trueVals;
      SmallVec<HWValue, 4> falseVals;
      for (auto op : ops) {
        auto mux = op->as<WireRef>().getDefI();
        assert(mux.isOpc(HW_MUX));
        trueVals.emplace_back(mux.other(1)->as<HWValue>());
        falseVals.emplace_back(mux.other(2)->as<HWValue>());
      }
      auto sel =
          (*ops.begin())->as<WireRef>().getDefI().other(0)->as<HWValue>();
      vals.emplace_back(build.buildMux(sel, build.buildConcat(trueVals),
                                       build.buildConcat(falseVals)));
      lastEnd = range.first + range.second;
    }
    if (instr.getNumOthers() != lastEnd) {
      vals.push_back_range(Range{instr.other_begin() + lastEnd,
                                 instr.other_begin() + instr.getNumOthers()}
                               .deref()
                               .as<HWValue>());
    }

    auto val = build.buildConcat(vals);
    replaceUses(instr.def(0)->as<WireRef>(), val);
    deleteMatchedInstr(instr);
    return PAT_TRUE;
  }

  PatBool sinkOneHotMUX(InstrRef instr) {
    if (config.liftMUX)
      return false;
    assert(instr.isOpc(HW_CONCAT));

    SmallVec<std::pair<uint32_t, uint32_t>, 4> ranges;
    size_t lastI = 0;
    Range<step_iterator<dyno::InstrRef::iterator>> lastSel;

    bool active = false;

    auto commit = [&](size_t i) {
      if (!active || i == lastI)
        return;
      active = false;
      if (i - lastI >= 2)
        ranges.emplace_back(lastI, i - lastI);
      lastI = i;
    };

    constexpr bool allowEmptyRegions = true;

    for (auto [i, op] : Range{instr.others()}.enumerate()) {
      if (auto wire = op->dyn_as<WireRef>();
          wire && wire.getDefI().isOpc(HW_ONEHOT_MUX) &&
          wire.getDefI().def(0)->as<WireRef>().hasSingleUse()) {
        auto mux = wire.getDefI();
        auto sel = mux.others().step(2);
        if (active &&
            !sel.as<FatDynObjRef<>>().equals(lastSel.as<FatDynObjRef<>>()))
          commit(i);
        if (!active) {
          lastI = i;
          lastSel = sel;
        }
        active = true;
      } else
        commit(i);
    }
    commit(instr.getNumOthers());

    if (ranges.empty() || (!allowEmptyRegions && ranges.size() != 1))
      return false;

    HWInstrBuilder build{ctx, instr};

    OtherVec<HWValue> vals{ctx};
    uint32_t lastEnd = 0;
    for (auto range : ranges) {
      if (range.first != lastEnd) {
        vals.push_back_range(Range{instr.other_begin() + lastEnd,
                                   instr.other_begin() + range.first}
                                 .as<HWValue>());
      }

      auto ops = Range{instr.other_begin() + range.first,
                       instr.other_begin() + range.first + range.second};
      auto headMux = instr.other(range.first)->as<WireRef>().getDefI();

      OtherVec<HWValue> muxOperands{ctx, 1, headMux.getNumOthers()};

      for (unsigned i = 0; i < headMux.getNumOthers() / 2; i++) {
        OtherVec<HWValue> concatOperands{ctx, 1, range.second - range.first};
        for (auto op : ops) {
          auto subMux = op->as<WireRef>().getDefI();
          concatOperands.emplace_back(subMux.other(2 * i + 1)->as<HWValue>());
        }
        // select signal
        muxOperands.emplace_back(headMux.other(2 * i)->as<HWValue>());
        muxOperands.emplace_back(build.buildConcat(std::move(concatOperands)));
      }

      vals.emplace_back(build.buildOneHotMux(std::move(muxOperands)));
      lastEnd = range.first + range.second;
    }
    if (instr.getNumOthers() != lastEnd) {
      vals.push_back_range(Range{instr.other_begin() + lastEnd,
                                 instr.other_begin() + instr.getNumOthers()}
                               .deref()
                               .as<HWValue>());
    }

    auto val = build.buildConcat(std::move(vals));
    replaceUses(instr.def(0)->as<WireRef>(), val);
    deleteMatchedInstr(instr);
    return PAT_TRUE;
  }

  PatBool fuseInserts(InsertIRef insert) {
    // todo: directly fuse all in chain instead of just two
    auto inWire = insert.in()->dyn_as<WireRef>();
    while (inWire && inWire.getNumUses() == 1 &&
           inWire.getDefI().isOpc(HW_INSERT)) {
      auto other = inWire.getDefI().as<InsertIRef>();
      // todo: doesn't actually have to be exactly equal. Constant offset is
      // also fine.
      if (!addressingFragsEqual(insert, other))
        return false;

      auto thisBase = insert.getBase();
      auto thisLen = insert.getLen();

      auto otherBase = other.getBase();
      auto otherLen = other.getLen();

      // no intersect or touching
      if (std::max(thisBase, otherBase) >
          std::min(thisBase + thisLen, otherBase + otherLen)) {
        inWire = other.in()->dyn_as<WireRef>();
        continue;
      }

      auto low = std::min(thisBase, otherBase);
      auto high = std::max(thisBase + thisLen, otherBase + otherLen);
      auto len = high - low;

      HWInstrBuilder build{ctx, insert};

      // use RegisterValue for easy merging
      RegisterValue val{nullref, len, 0, false, nullopt};
      val.overwrite(other.val()->as<HWValue>(), 0, otherBase - low, otherLen);
      val.overwrite(insert.val()->as<HWValue>(), 0, thisBase - low, thisLen);

      auto outWire =
          build.buildInsert(insert.in()->as<HWValue>(), val.get(build, false),
                            low, insert.terms());
      replaceUses(insert.out()->as<WireRef>(), outWire);
      // skip inserting other
      replaceUses(other.out()->as<WireRef>(), other.in()->as<HWValue>());
      deleteMatchedInstr(other);
      deleteMatchedInstr(insert);
      return PAT_TRUE;
    }
    return false;
  }

  PatBool simplifyDeMorgan(InstrRef instr) {
    if (!config.simplifyDeMorgan)
      return false;
    unsigned inverted = 0;
    for (auto op : instr.others()) {
      if (op->is<WireRef>() && op->as<WireRef>().getDefI().isOpc(OP_NOT))
        inverted++;
    }
    // todo: check output inverted free?, operand inversions free?
    if (inverted <= round_up_div(instr.getNumOthers(), 2u))
      return false;

    // if (instr.getNumOthers() & 1) {
    //   if (inverted <= instr.getNumOthers() / 2U)
    //     return false;
    // } else if (!(instr.getNumOthers() & 1)) {
    //   if (instr.isOpc(OP_AND) ? (inverted <= instr.getNumOthers() / 2U)
    //                           : (inverted < instr.getNumOthers() / 2U))
    //     return false;
    // }

    HWInstrBuilder build{ctx, instr};
    SmallVec<HWValue, 8> ops;
    ops.reserve(instr.getNumOthers());
    for (auto op : instr.others())
      ops.emplace_back(build.buildNot(op->as<HWValue>()));

    HWValue val;
    if (instr.isOpc(OP_OR))
      val = build.buildAnd(ops);
    else
      val = build.buildOr(ops);

    replaceUses(instr.def(0)->as<WireRef>(), build.buildNot(val));
    deleteMatchedInstr(instr);
    return PAT_TRUE;
  }

  PatBool simplifyBitAliases(InstrRef instr) {
    // Main function for canoncializing addressing. This gets quite ugly, we
    // have to match instrs to see if the existing canonical pattern exists
    // already (return false if it does) and else build it. Currently doing all
    // the compares manually. Maybe implement this w/ DSL or continuous CSE (
    // build abstract instructions and check if equal to actual) in the future.

    auto defW = instr.def(0)->as<WireRef>();
    auto aliases = bitAlias.getReprAliases(defW);
    assert(aliases.frags.size() != 0);
    HWInstrBuilder build{ctx, instr};

    auto operandEqualsFrag = [&](HWValue val,
                                 RegisterValueFragment &frag) -> HWValue {
      auto instr = !val.is<WireRef>() ? nullref : val.as<WireRef>().getDefI();
      auto fragRef = ctx.resolve(frag.ref);
      if (frag.srcAddr == 0 && frag.ref == val && frag.len == val.getNumBits())
        return nullref;
      if (frag.srcAddr == 0) {
        if (instr && instr.isOpc(OP_TRUNC) &&
            instr.other(0)->as<HWValue>() == frag.ref &&
            instr.def(0)->as<HWValue>().getNumBits() == frag.len)
          return nullref;
        return build.buildTrunc(frag.len, fragRef);
      } else {
        if (instr && instr.isOpc(HW_SPLICE) &&
            instr.as<SpliceIRef>().isConstantOffs() &&
            instr.as<SpliceIRef>().getBase() == frag.srcAddr &&
            instr.as<SpliceIRef>().getLen() == frag.len &&
            instr.as<SpliceIRef>().in()->thin() == frag.ref)
          return nullref;
        return build.buildSplice(fragRef, frag.len, frag.srcAddr);
      }
    };

    if (aliases.frags.size() == 1) {
      auto &frag = aliases.frags.front();
      auto repl = operandEqualsFrag(defW, frag);
      if (!repl)
        return false;
      replaceUses(defW, repl);
      deleteMatchedInstr(instr);
      return PAT_TRUE;
    }

    auto matchInsert = [&]() -> Optional<int> {
      auto &front = aliases.frags[0];
      auto &mid = aliases.frags[1];
      auto &end = aliases.frags[2];
      if (front.ref != end.ref || front.srcAddr != 0 ||
          end.srcAddr != end.dstAddr)
        return nullopt;
      auto pad = ctx.resolve(front.ref).as<HWValue>();
      if (pad.getNumBits() != aliases.getLen())
        return nullopt;
      auto midRef = ctx.resolve(mid.ref).as<HWValue>();
      HWValue midVal;

      if (instr.isOpc(HW_INSERT)) {
        auto insert = instr.as<InsertIRef>();
        if (insert.isConstantOffs() && insert.in()->thin() == front.ref &&
            insert.getLen() == mid.len && insert.getBase() == mid.dstAddr &&
            insert.getMemoryLen() == (front.len + mid.len + end.len)) {
          if (insert.val()->thin() == mid.ref)
            return false;
          midVal = operandEqualsFrag(insert.val()->as<HWValue>(), mid);
          if (!midVal)
            return false;
        } else
          midVal = build.buildSplice(midRef, mid.len, mid.srcAddr);
      } else
        midVal = build.buildSplice(midRef, mid.len, mid.srcAddr);

      auto out = build.buildInsert(pad, midVal, mid.dstAddr);
      replaceUses(defW, out);
      deleteMatchedInstr(instr);
      return true;
    };
    if (aliases.frags.size() == 3)
      if (auto rv = matchInsert())
        return PAT_BOOL(*rv);

    SmallVec<HWValue, 16> operands;
    bool anyMismatch = false;

    InstrRef concat =
        (instr.isOpc(HW_CONCAT) && instr.getNumOthers() == aliases.frags.size())
            ? instr
            : nullref;
    for (unsigned i = 0; i < aliases.frags.size(); i++) {
      auto &frag = aliases.frags[aliases.frags.size() - i - 1];
      auto existing = concat ? concat.other(i)->as<HWValue>() : nullref;
      auto op = operandEqualsFrag(existing, frag);
      if (!op) {
        assert(existing);
        operands.emplace_back(existing);
      } else {
        operands.emplace_back(op);
        anyMismatch = true;
      }
    }

    if (!anyMismatch)
      return false;

    auto ib = build.buildInstrRaw(HW_CONCAT, 1 + aliases.frags.size());
    build.setInsertPoint(ib.instr());
    ib.addRef(instr.def(0)->as<WireRef>()).other();
    ib.addRefs(operands);
    deleteMatchedInstr(instr);
    return PAT_TRUE;
  }

  PatBool coalesceConcatOfLoads(InstrRef concat) {
    SmallVec<HWValue, 8> outOps;

    RegisterRef current = nullref;
    uint32_t currentDst = 0;
    uint32_t currentSrc = 0;

    uint32_t bits = 0;

    HWInstrBuilder build{ctx, concat};
    auto flush = [&]() {
      if (!current)
        return;
      outOps.emplace_back(
          build.buildLoad(current, bits - currentDst, currentSrc));
      current = nullref;
    };

    for (auto op : Range{concat.others()}.reverse()) {
      if (op->is<ConstantRef>()) {
        flush();
        outOps.emplace_back(op->as<ConstantRef>());
        bits += op->as<ConstantRef>().getNumBits();
        continue;
      }
      auto wire = op->as<WireRef>();
      auto instr = wire.getDefI();

      // look through splices
      auto spliceBase = 0;
      auto spliceLen = wire.getNumBits();
      if (auto splice = instr.dyn_as<SpliceIRef>();
          splice && splice.isConstantOffs() && splice.in()->is<WireRef>()) {
        spliceBase = splice.getBase();
        spliceLen = splice.getLen();
        instr = splice.in()->as<WireRef>().getDefI();
      }

      if (!instr.isOpc(HW_LOAD) || !instr.as<LoadIRef>().isConstantOffs()) {
        flush();
        bits += *wire.getNumBits();
        outOps.emplace_back(wire);
        continue;
      }
      auto load = instr.as<LoadIRef>();

      auto base = load.getBase() + spliceBase;

      if (current &&
          (current != load.reg() || base != currentSrc + (bits - currentDst))) {
        flush();
      }

      if (!current) {
        current = load.reg();
        currentSrc = base;
        currentDst = bits;
      } else {
        ;
      }

      bits += *wire.getNumBits();
    }
    flush();

    if (outOps.size() == concat.getNumOthers())
      return false;

    std::reverse(outOps.begin(), outOps.end());
    auto out = build.buildConcat(outOps);
    replaceUses(concat.def(0)->as<WireRef>(), out);
    deleteMatchedInstr(concat);
    return PAT_TRUE;
  }

  PatBool reduceBitWidthICMP(InstrRef instr) {
    uint32_t minLeading = UINT32_MAX;

    WireRef outWire = instr.def(0)->as<WireRef>();
    uint32_t bits = *instr.other(0)->as<HWValue>().getNumBits();

    SmallVec<BigInt, 2> inputKnownBits;

    auto combineLeading = [&](BigInt &known) {
      auto newLeading = BigInt::leadingNonUnk(known);
      if (newLeading <= 1)
        return false;
      minLeading = std::min(minLeading, newLeading);
      return true;
    };

    for (auto op : instr.others()) {
      auto known = knownBits.getKnownBits(op->as<HWValue>());
      if (!combineLeading(known))
        return false;
      inputKnownBits.emplace_back(known);
    }

    if (minLeading == 0)
      return false;

    uint32_t activeBits = bits - minLeading;
    assert(activeBits > 0 && "constprop should have handled this");
    if (activeBits == bits)
      return false;
    assert(activeBits < bits);

    for (auto &bigInt : inputKnownBits) {
      BigInt::rangeSelectOp4S(bigInt, bigInt, activeBits,
                              bigInt.getNumBits() - activeBits);
    }
    bool equal = true;
    for (auto &bigInt : Range{inputKnownBits}.drop_front()) {
      if (bigInt != inputKnownBits.front()) {
        equal = false;
        break;
      }
    }
    if (!equal) {
      if (instr.isOpc(OP_ICMP_EQ, OP_ICMP_NE)) {
        replaceUses(outWire, ConstantRef::fromBool(instr.isOpc(OP_ICMP_NE)));
        return PAT_TRUE;
      }
      BigInt::ICmpPred pred =
          BigInt::ICmpPred(instr.getOpcode().num - OP_ICMP_EQ.opc.num);
      assert(pred >= BigInt::ICmpPred::ICMP_EQ &&
             pred <= BigInt::ICmpPred::ICMP_SGE);
      replaceUses(outWire, ConstantRef::fromBool(BigInt::icmpOp(
                               inputKnownBits[0], inputKnownBits[1], pred)));
      return PAT_TRUE;
    }

    HWInstrBuilder build{ctx};

    build.setInsertPoint(instr);
    auto ibAdd =
        build.buildInstrRaw(instr.getDialectOpcode(), instr.getNumOperands());
    ibAdd.addRef(outWire).other();
    build.setInsertPoint(ibAdd.instr());
    for (auto op : instr.others()) {
      auto truncd = build.buildTrunc(activeBits, op->as<HWValue>());
      ibAdd.addRef(truncd);
    }
    build.setInsertPoint(instr);

    // instr.def(0).replace(FatDynObjRef<>{nullref});
    deleteMatchedInstr(instr);
    return PAT_TRUE;
  }

  PatBool simplifyAndCanonicalizeCommOps(InstrRef root) {
    SmallVec<WireRef, 8> operands;
    SmallVec<ConstantRef, 8> constants;
    SmallVec<InstrRef, 4> stack{root};
    auto opc = root.getDialectOpcode();

    auto oldDefW = root.def(0)->as<WireRef>();

    bool change = false;
    while (!stack.empty()) {
      auto instr = stack.pop_back_val();
      // std::optional<DialectOpcode> extendType = std::nullopt;
      // uint32_t extendBits;
      // auto defWire = instr.def(0)->as<WireRef>();

      // if (oldDefW.getNumBits() != defWire.getNumBits()) {
      //   assert(*oldDefW.getNumBits() > *defWire.getNumBits());
      //   auto extendInstr = defWire.getSingleUse()->instr();
      //   auto extendWire = extendInstr.def(0)->as<WireRef>();
      //   extendBits = *extendWire.getNumBits();
      //   auto thisBits = *defWire.getNumBits();
      //   if (extendBits > thisBits)
      //     extendType = extendInstr.getDialectOpcode();
      // }
      // auto expandIfNecessary = [&](HWValue value) {
      //   if (!extendType)
      //     return value;
      //   return HWInstrBuilder{ctx,
      //   ctx.getCtx<CoreDialectContext>().cfg[root]}.buildExt(
      //       extendBits, value, *extendType);
      // };

      for (auto operand : instr.others()) {
        if (auto asWire = operand->dyn_as<WireRef>()) {
          auto defI = asWire.getSingleDef()->instr();

          // // eagerly fuse other ops even if done on fewer bits. expectation
          // // is that optimizer can nicely handle extends on operands.
          // if (defI.isOpc(OP_ZEXT, OP_SEXT, OP_ANYEXT) &&
          //     asWire.hasSingleUse()) {
          //   asWire = defI.other(0)->as<WireRef>();
          //   defI = asWire.getDefI();
          // }
          if (defI.isOpc(opc) && asWire.hasSingleUse() &&
              config.fuseCommutative) {
            stack.emplace_back(defI);
            deleteMatchedInstr(defI);
            change = true;
            continue;
          }

          operands.emplace_back(operand->as<WireRef>());
          continue;
        }
        if (operand != *instr.end() - 1)
          change = true;
        constants.emplace_back(operand->as<ConstantRef>());
        continue;
      }
    }

    change |= constants.size() > 1;

    if (constants.size() > 1) {
      cbuild.val(constants.front());
      switch (*root.getDialectOpcode()) {
#define FUNC(opc, hb, cb, bib)                                                 \
  case *opc:                                                                   \
    for (auto val : Range{constants}.drop_front())                             \
      cbuild.cb(val);                                                          \
    break;
        FOR_HW_COMM_OPS(FUNC)
#undef FUNC
      }
      constants.front() = cbuild.get();
    }
    if (!constants.empty()) {
      switch (*root.getDialectOpcode()) {
      case *OP_OR:
        if (constants[0].valueEqualsS(-1)) {
          deleteMatchedInstr(root);
          replaceUses(oldDefW, cbuild.ones(*oldDefW->numBits).get());
          return PAT_TRUE;
        }
        [[fallthrough]];
      case *OP_XOR:
      case *OP_ADD:
        if (constants[0].valueEquals(0)) {
          change |= 1;
          constants.clear();
        }
        break;
      case *OP_AND:
        if (constants[0].valueEquals(0)) {
          deleteMatchedInstr(root);
          replaceUses(oldDefW, cbuild.zero(*oldDefW->numBits).get());
          return PAT_TRUE;
        }
        if (constants[0].valueEqualsS(-1)) {
          change |= 1;
          constants.clear();
        }
        break;
      default:;
      }
    }

    if (operands.size() + constants.size() <= 1) {
      if (operands.size() + constants.size() == 0) {
        switch (*root.getDialectOpcode()) {
        case *OP_XOR:
        case *OP_ADD:
        case *OP_OR: {
          deleteMatchedInstr(root);
          replaceUses(oldDefW, cbuild.zero(*oldDefW->numBits).get());
          return PAT_TRUE;
        }
        case *OP_MUL: {
          deleteMatchedInstr(root);
          replaceUses(oldDefW, cbuild.one(*oldDefW->numBits).get());
          return PAT_TRUE;
        }
        case *OP_AND: {
          deleteMatchedInstr(root);
          replaceUses(oldDefW, cbuild.ones(*oldDefW->numBits).get());
          return PAT_TRUE;
        }
        default:
          break;
        }
        dyno_unreachable("no neutral element");
      }
      switch (*root.getDialectOpcode()) {
      case *OP_ADD:
      case *OP_AND:
      case *OP_OR:
      case *OP_XOR:
      case *OP_MUL:
        deleteMatchedInstr(root);
        if (constants.size() == 1)
          replaceUses(oldDefW, constants[0]);
        else
          replaceUses(oldDefW, operands[0]);
        return PAT_TRUE;
      }
      dyno_unreachable("no 1-ary output value");
    }

    if (!std::is_sorted(operands.begin(), operands.end(),
                        HWInstrBuilder::commutativeOpWireOrder))
      change = true;

    if (!change) {
      return false;
    }
    deleteMatchedInstr(root);
    HWInstrBuilder build{ctx};
    build.setInsertPoint(ctx.getCtx<CoreDialectContext>().cfg[root]);

    std::sort(operands.begin(), operands.end(),
              HWInstrBuilder::commutativeOpWireOrder);

    ++build.insert;
    auto ibuild = build.buildInstrRaw(opc, 1 + operands.size() +
                                               (constants.empty() ? 0 : 1));

    // todo: steal slot mechanism
    ibuild.addRef(oldDefW);
    ibuild.other();
    for (auto operand : operands)
      ibuild.addRef(operand);
    if (!constants.empty())
      ibuild.addRef(constants[0]);

    return PAT_TRUE;
  }

  PatBool nonTemporalLoadElim(LoadIRef load) {
    auto proc = load.parentProc(ctx);
    if (!proc.isOpc(HW_NETLIST_PROCESS_DEF))
      return false;
    auto storeI = load.reg().iref().getSingleStore();
    if (!storeI)
      return false;
    auto store = StoreIRef{storeI};
    if (!store.isFullReg())
      return false;
    if (store.parentProc(ctx) != proc)
      return false;
    HWInstrBuilder build{ctx, load};

    auto val = build.buildSplice(store.value(), load.getLen(), load.getBase(),
                                 load.terms());
    if (load.value() != val) {
      replaceUses(load.value(), val);
      deleteMatchedInstr(load);
    } else {
      deleteMatchedInstr(store);
    }

    return PAT_TRUE;
  }

  PatBool storeValConstProp(StoreIRef instr) {
    if (!instr.isFullReg())
      return false;
    HWValueOrReg ref = nullref;
    bool inverse = false;
    if (auto asConst = instr.value().dyn_as<ConstantRef>())
      ref = asConst;
    else if (auto asWire = instr.value().dyn_as<WireRef>()) {
      if (instr.isOpc(HW_STORE_DEFER))
        return false;

      if (asWire.getDefI().isOpc(OP_NOT)) {
        inverse = true;
        HWValue op = asWire.getDefI().other(0)->as<HWValue>();
        if (!op.is<WireRef>())
          return false;
        asWire = op.as<WireRef>();
      }

      if (!asWire.getDefI().isOpc(HW_LOAD))
        return false;

      auto load = asWire.getDefI().as<LoadIRef>();
      if (!load.isFullReg())
        return false;
      if (load.reg().getNumBits())
        ref = load.reg();
    }

    RegisterRef reg = instr.reg();
    SmallVec<OperandRef, 4> uses;
    for (auto use : reg.uses()) {
      if (use.instr().isOpc(HW_STORE, HW_STORE_DEFER)) {
        if (use.instr() != instr) {
          return false;
        }
        continue;
      }
      // can't do const prop on instances
      if (ref.is<ConstantRef>() &&
          use.instr().isOpc(HW_INSTANCE, HW_TRIGGER_DEF, HW_FLIP_FLOP,
                            HW_LATCH)) {
        return false;
      }
      // can only do inversion on storage elements
      if (inverse && use.instr().isOpc(HW_INSTANCE))
        return false;
      uses.emplace_back(use);
    }

    // If the reg we're storing to is a port of some kind try replacing the
    // other way around.
    if (!reg.iref().isOpc(HW_REGISTER_DEF)) {
      if (!ref.is<RegisterRef>())
        return false;
      if (!ref.as<RegisterRef>().iref().isOpc(HW_REGISTER_DEF))
        return false; // nothing to be done if both are ports.
      if (inverse)
        return false;
      ref.as<RegisterRef>().replaceAllUsesWith(reg);
      deleteMatchedInstr(instr);
      return PAT_TRUE;
    }

    auto inverseUse = [&](OperandRef use) {
      auto instr = use.instr();
      HWInstrBuilder build{ctx, instr};
      build.insert = build.insert.succ();
      switch (*instr.getDialectOpcode()) {
      case *HW_LOAD: {
        auto defW = instr.def(0)->as<WireRef>();
        auto invW = ctx.getStore<Wire>().create(*defW.getNumBits());
        replaceUses(defW, invW);
        build.buildInstrRaw(OP_NOT, 2).addRef(invW).other().addRef(defW);
        break;
      }

      case *HW_TRIGGER_DEF: {
        auto asTrigger = instr.as<TriggerIRef>();
        unsigned idx = use - *instr.other_begin();
        asTrigger.oref()->inverseMode(idx);
        break;
      }

      default:
        dyno_unreachable("register use polarity can't be inverted");
      }
    };

    if (ref.is<RegisterRef>()) {
      for (auto use : uses) {
        use.replace(ref);
        if (inverse)
          inverseUse(use);
        currentReplaced.emplace_back(use);
      }
    } else {
      assert(ref.is<ConstantRef>());
      assert(!inverse);
      HWInstrBuilder build{ctx};
      for (auto use : uses) {
        auto asLoad = use.instr().as<LoadIRef>();
        build.setInsertPoint(asLoad);
        auto splice = build.buildSplice(ref.as<ConstantRef>(), asLoad.getLen(),
                                        asLoad.getBase(), asLoad.terms());
        replaceUses(asLoad.value(), splice);
        deleteMatchedInstr(use.instr());
      }
    }

    deleteMatchedInstr(instr);
    return PAT_TRUE;
  }

  template <typename Ref> PatBool simplifyYieldValues(Ref instr) {
    SmallVec<HWValue, 32> yieldValues;
    if (instr.getNumYieldValues() == 0)
      return false;
    yieldValues.reserve(instr.getNumYieldValues() * instr.getNumCases());

    DynSymbSet<SmallVec<uint64_t, 1>, 1, ~0UL> equal(instr.getNumYieldValues());
    for (auto [front, yield] : instr.caseYields().mark_front()) {
      assert(yield);
      for (auto op : yield)
        yieldValues.emplace_back(op->template as<HWValue>());
      if (front)
        continue;
      for (auto [i, op] : Range{yield}.enumerate()) {
        if (op->template as<HWValue>() != yieldValues[i])
          equal[i] = 0;
      }
    }

    auto unused = [&](unsigned i) {
      assert(i < instr.getNumYieldValues());
      return instr.yieldValues()
                 .begin()[i]
                 ->template as<WireRef>()
                 .getNumUses() == 0;
    };

    auto cnt = Range{equal}.enumerate().count_if(
        [&](auto pair) { return pair.second || unused(pair.first); });

    // return if nothing changed
    if (cnt == 0)
      return false;

    auto rem = instr.getNumYieldValues() - cnt;

    HWInstrBuilder build{ctx};

    // re-build yield instrs
    for (auto yield : instr.caseYields()) {
      if (rem != 0) {
        build.setInsertPoint(HWInstrRef{yield}.parentBlock(ctx).end());
        auto ib = build.buildInstrRaw(yield.getDialectOpcode(), rem);
        ib.other();
        for (auto [i, op] : Range{yield}.enumerate()) {
          if (equal[i] || unused(i))
            continue;
          ib.addRef(op->template as<HWValue>());
        }
      }
      deleteMatchedInstr(yield);
    }

    // re-build parent instr
    build.setInsertPoint(instr);
    auto ib = build.buildInstrRaw(instr.getDialectOpcode(),
                                  instr.getNumOperands() - cnt);
    for (auto def : instr.defs()) {
      int idx = def - instr.getYieldValue(0);
      auto ref = def->fat();
      if (idx >= 0 && (unsigned)idx < instr.getNumYieldValues()) {
        if (unused(idx)) {
          // replace with nullref already, we don't want this double defined
          // in case replaceUses is called.
          def.replace(FatDynObjRef{nullref});
          continue;
        }
        if (equal[idx]) {
          def.replace(FatDynObjRef{nullref});
          replaceUses(ref.template as<WireRef>(), yieldValues[idx]);
          continue;
        }
      }
      def.replace(FatDynObjRef{nullref});
      ib.addRef(ref);
    }
    ib.other();
    ib.addRefs(instr.others().transform(
        [](size_t, OperandRef ref) { return ref->fat(); }));

    deleteMatchedInstr(instr);
    return PAT_TRUE;
  }

  template <typename RefT> PatBool simplifyAddressing(RefT instr) {
    HWInstrBuilder build{ctx, instr};
    if (instr.getNumTerms() == 0) {
      if (instr.hasBase() && instr.getBase() == 0) {
        auto ib = build.buildInstrRaw(instr.getDialectOpcode(),
                                      instr.getNumOperands() - 1);
        for (auto def : instr.defs()) {
          ib.addRef(def->fat());
          // def.replace(FatDynObjRef{nullref});
        }
        ib.other();
        for (auto use : instr.others()) {
          if (use == instr.base()) {
            continue;
          }
          ib.addRef(use->fat());
        }
        deleteMatchedInstr(instr);
        return PAT_TRUE;
      }
      return false;
    }

    bool change = false;

    auto checkedMul = [](auto lhs, auto rhs) {
      return checked_mul(lhs, rhs, "address does not fit into 32 bits");
    };

    uint32_t baseOffs = instr.getBase();
    SmallVec<AddressGenTerm, 4> terms;
    for (auto term : instr.terms()) {
      if (auto asConst = term.getIdx().template dyn_as<ConstantRef>()) {
        baseOffs += checkedMul(asConst.getExactVal(), term.getFact());
        change = true;
        continue;
      }

      auto asWire = term.getIdx().template as<WireRef>();
      if (asWire.getDefI().isOpc(OP_MUL) &&
          asWire.getDefI().getNumOthers() == 2 &&
          asWire.getDefI().other(1)->template is<ConstantRef>()) {
        auto mul = asWire.getDefI();
        auto constant = mul.other(1)->template as<ConstantRef>();
        auto max = term.getMax();
        if (max)
          *max = round_up_div(*max, constant.getExactVal());
        terms.emplace_back(mul.other(0)->template as<HWValue>(),
                           checkedMul(term.getFact(), constant.getExactVal()),
                           max);
        change = true;
        continue;
      }

      auto known = knownBits.getKnownBits(asWire);
      assert(known.getNumBits() == 32);
      if (auto numKnown = BigInt::trailingNonUnk(known);
          numKnown > 0 && numKnown != 32) {
        BigInt::resizeOp4S(known, known, numKnown);
        assert(!known.getIs4S());

        baseOffs += checkedMul(known.getExactVal(), term.getFact());

        auto newIdx = build.buildZExt(
            32, build.buildSplice(asWire, *asWire.getNumBits() - numKnown,
                                  numKnown));
        auto fact = 1u << numKnown;
        auto max = term.getMax();
        if (max)
          *max = round_up_div(*max, fact);
        terms.emplace_back(newIdx, checkedMul(term.getFact(), fact), max);
        change = true;
        continue;
      }

      terms.emplace_back(term);
    }

    if (!change)
      return false;

    unsigned termDiff = instr.getNumTerms() - terms.size();
    unsigned numOperands = instr.getNumOperands() - termDiff * 3;
    bool removeBaseOffs = terms.size() == 0 && baseOffs == 0;
    if (removeBaseOffs)
      numOperands--;
    auto ib = build.buildInstrRaw(instr.getDialectOpcode(), numOperands);

    for (auto def : instr.defs()) {
      ib.addRef(def->fat());
      // def.replace(FatDynObjRef{nullref});
    }
    ib.other();
    for (auto use : Range{instr.other_begin(),
                          instr.other_begin() + instr.addressGenBaseIndex()}) {
      ib.addRef(use->fat());
    }
    if (!removeBaseOffs)
      ib.addRef(ConstantRef::fromU32(baseOffs));
    for (auto term : terms)
      ib.addRef(term.getIdx())
          .addRef(ConstantRef::fromU32(term.getFact()))
          .addRef(ConstantRef::fromU32(term.getMax().value_or(~0)));

    deleteMatchedInstr(instr);
    return PAT_TRUE;
  }

  PatBool manual(InstrRef instr) {
    if (instr.getNumDefs() == 1 && instr.def(0)->is<WireRef>())
      if (auto trueV = knownBitsConstProp(instr))
        return trueV;

    if (instr.isOpc(HW_SPLICE))
      if (auto trueV = simplifyAddressing(instr.as<SpliceIRef>()))
        return trueV;
    if (instr.isOpc(HW_INSERT))
      if (auto trueV = simplifyAddressing(instr.as<InsertIRef>()))
        return trueV;
    if (instr.isOpc(HW_LOAD))
      if (auto trueV = simplifyAddressing(instr.as<LoadIRef>()))
        return trueV;
    if (instr.isOpc(HW_STORE, HW_STORE_DEFER))
      if (auto trueV = simplifyAddressing(instr.as<StoreIRef>()))
        return trueV;
    if (instr.isOpc(HW_GEP))
      if (auto trueV = simplifyAddressing(instr.as<GEPIRef>()))
        return trueV;

    switch (*instr.getDialectOpcode()) {
#define LAMBDA(opc, ib, cb, bib) case *opc:
      FOR_HW_COMM_OPS(LAMBDA)
#undef LAMBDA
      if (auto trueV = simplifyAndCanonicalizeCommOps(instr))
        return trueV;
      break;

    case *HW_STORE:
    case *HW_STORE_DEFER: {
      if (auto trueV = storeValConstProp(instr.as<StoreIRef>()))
        return trueV;
      break;
    }

    case *HW_LOAD: {
      if (auto trueV = nonTemporalLoadElim(instr.as<LoadIRef>()))
        return trueV;
      break;
    }

    case *HW_CONCAT:
    case *OP_TRUNC:
    case *HW_SPLICE:
    case *HW_INSERT: {
      if (auto trueV = simplifyBitAliases(instr))
        return trueV;
      break;
    }

    case *OP_YIELD: {
      auto parent = HWInstrRef{instr}.parentBlock(ctx).defI();
      if (parent.isOpc(OP_IF)) {
        if (auto trueV = simplifyYieldValues(instr.as<IfInstrRef>()))
          return trueV;
      } else if (parent.isOpc(OP_CASE, OP_CASE_DEFAULT, HW_CASE_Z, HW_CASE_X)) {
        auto swInstr =
            HWInstrRef{parent}.parentBlock(ctx).defI().as<SwitchInstrRef>();
        assert(swInstr.isOpc(OP_SWITCH));
        if (auto trueV = simplifyYieldValues(swInstr))
          return trueV;
      }
      break;
    }

    case *OP_IF: {
      if (auto trueV = simplifyYieldValues(instr.as<IfInstrRef>()))
        return trueV;
      break;
    }

    case *OP_SWITCH: {
      if (auto trueV = simplifyYieldValues(instr.as<SwitchInstrRef>()))
        return trueV;
      break;
    }

    case *HW_MUX: {
      if (auto trueV = liftMUX(instr))
        return trueV;
      break;
    }

    case *HW_ONEHOT_MUX: {
      if (auto trueV = mergeOneHotMux(instr))
        return trueV;
      if (auto trueV = optimizeOneHotMux(instr))
        return trueV;
      if (auto trueV = optimizeOneHotMuxUndef(instr))
        return trueV;
      if (auto trueV = liftOneHotMUX(instr))
        return trueV;
      break;
    }

    case *HW_ASSUME: {
      if (auto trueV = simplifyAssume(instr))
        return trueV;
      break;
    }

    case *HW_FLIP_FLOP:
    case *HW_FLIP_FLOP_SRST: {
      if (auto trueV = simplifyFlipFlopEnable(instr))
        return trueV;
      if (auto trueV = simplifyFlipFlopResets(instr))
        return trueV;
      if (auto trueV = findFlipFlopEnables(instr))
        return trueV;
      break;
    }

    default:
      break;
    }

    if (instr.isOpc(OP_ADD, OP_MUL, OP_AND, OP_OR, OP_XOR)) {
      if (auto trueV = reduceBitWidth(instr))
        return trueV;
    }

    if (instr.isOpc(OP_AND, OP_OR)) {
      if (auto trueV = simplifyDeMorgan(instr))
        return trueV;

      if (instr.def(0)->as<WireRef>().getNumBits() == 1)
        if (auto trueV = boolExprSimplify(instr))
          return trueV;
    }

    if (instr.isOpc(OP_ICMP_EQ, OP_ICMP_NE, /*OP_ICMP_CEQ, OP_ICMP_CNE,
                    OP_ICMP_WEQ, OP_ICMP_WNE, OP_ICMP_CZEQ, OP_ICMP_CZNE,
                    OP_ICMP_CXEQ, OP_ICMP_CXNE,*/
                    OP_ICMP_ULT, OP_ICMP_SLT, OP_ICMP_ULE, OP_ICMP_SLE,
                    OP_ICMP_UGT, OP_ICMP_SGT, OP_ICMP_UGE, OP_ICMP_SGE)) {
      if (auto trueV = reduceBitWidthICMP(instr))
        return trueV;
    }

    if (instr.isOpc(HW_CONCAT))
      if (auto trueV = coalesceConcatOfLoads(instr))
        return trueV;

    if (instr.isOpc(HW_CONCAT)) {
      if (auto trueV = sinkMUX(instr))
        return trueV;
      if (auto trueV = sinkOneHotMUX(instr))
        return trueV;
    }

    if (instr.isOpc(HW_INSERT))
      if (auto trueV = fuseInserts(instr.as<InsertIRef>()))
        return trueV;

    return false;
  }

  PatBool matchPatternsOnInstr(InstrRef instr) {
    currentMatched.clear();
    currentReplaced.clear();

    if (instr.getNumDefs() == 1) {
      if (!instr.def(0)->is<WireRef>())
        return false;
      if (instr.def(0)->as<WireRef>().getNumUses() == 0) {
        // DCE unused instruction
        deleteMatchedInstr(instr);
        return PatBool{"deleteDeadInstr"};
      }
    }

    if (auto trueV = manual(instr))
      return trueV;

    currentMatched.clear();
    currentReplaced.clear();

    if (instr.getNumDefs() > 1)
      return false;
    return PAT_BOOL(
        generated(ctx, config, currentMatched, currentReplaced, instr));
  }

  void recomputeAnalysesAtDefWire(WireRef wire) {
    knownBits.recomputeAt(wire);
    bitAlias.recomputeAt(wire);
  }

  void newInstrHook(InstrRef ref) {
    for (auto def : ref.defs()) {
      switch (*def->fat().getType()) {
      case *HW_WIRE: {
        auto asWire = def->as<WireRef>();
        assert(asWire.getNumDefs() == 1);
        recomputeAnalysesAtDefWire(asWire);
        for (auto use : asWire.uses())
          worklist.emplace_back(use.instr());
        break;
      }
      default:
        break;
      }
    }

    for (auto op : ref.others())
      if (auto wire = op->dyn_as<WireRef>())
        assert(wire.getNumDefs() == 1);
  }

  void oldInstrHook(InstrRef old, ArrayRef<InstrRef> newInstrs) {
    for (auto newInstr : newInstrs)
      ctx.getCtx<CoreDialectContext>().instrSourceLocInfo.copyDebugInfo(
          old, newInstr);
    if (TaggedIRef{old}.get()) {
      for (auto op : old) {

        // propagate unused values up the chain
        if (!op.isDef()) {
          if (auto asWire = op->dyn_as<WireRef>();
              asWire && asWire.hasSingleUse() && asWire.hasSingleDef()) {
            auto defI = asWire.getDefI();
            if (!TaggedIRef{defI}.get() && defI.getNumDefs() == 1)
              deleteMatchedInstr(defI);
          }
        }

        op.replace(FatDynObjRef<>{nullref});
      }
    } else {
      // if not explicitly ok to delete, re-inspect
      worklist.emplace_back(old);
    }
  }

  void replacedUseHook(OperandRef replaced) {
    worklist.emplace_back(replaced.instr());
  }

  void anyMatchHook() {
    // bitAlias.clearCache();
    // knownBits.clearCache();
  }

  void runOnInstr(InstrRef instr) {
    if (TaggedIRef{instr}.get())
      return;
    auto lastWorklistSize = worklist.size();

    PatBool result = matchPatternsOnInstr(instr);
    if (!result)
      return;

    anyMatchHook();

    DYNO_DBG({
      HWPrinter print{dbgs()};
      dbgs() << "initial instructions (" << result.function << "):\n";

      for (auto instr : currentMatched)
        print.printInstr(instr, ctx);

      dbgs() << "replaced with:\n";

      for (size_t i = lastWorklistSize, sz = worklist.size(); i < sz; i++)
        print.printInstr(worklist[i], ctx);
      if (lastWorklistSize == worklist.size()) {
        if (!currentReplaced.empty()) {
          dumpObj(currentReplaced[0]->fat());
          dbgs() << "\n";
        } else
          dbgs() << "<none>\n";
      }
      dbgs() << "\n";
    })

    auto newInstrs =
        ArrayRef<InstrRef>{worklist.begin() + lastWorklistSize, worklist.end()};
    for (size_t i = 0; i < currentMatched.size(); i++) {
      oldInstrHook(currentMatched[i], newInstrs);
    }
    for (auto operand : currentReplaced) {
      replacedUseHook(operand);
    }
    for (size_t i = lastWorklistSize, sz = worklist.size(); i < sz; i++)
      newInstrHook(worklist[i]);
  }

  void runOnProcess(ProcessIRef proc) {
    worklist.clear();
    bitAlias.clearCache();
    knownBits.clearCache();

    for (auto instr : HierBlockRange{proc.block()})
      worklist.emplace_back(instr);
    while (!worklist.empty()) {
      auto instr = worklist.pop_back_val();
      runOnInstr(instr);
    }
  }

  void runOnBlock(BlockRef block) {
    worklist.clear();
    bitAlias.clearCache();
    knownBits.clearCache();

    for (auto instr : block)
      worklist.emplace_back(instr);
    while (!worklist.empty()) {
      auto instr = worklist.pop_back_val();
      runOnInstr(instr);
    }
  }

  void runOnModule(ModuleIRef mod) {
    for (auto proc : mod.procs())
      runOnProcess(proc);
  }

  void destroyMarkedInstrs() {
    HWInstrBuilder build{ctx};
    for (auto instr : ctx.getStore<Instr>()) {
      if (TaggedIRef{instr}.get())
        build.destroyInstr(instr);
    }
  }

public:
  void runOnly(DialectOpcode opc, SmallVecImpl<InstrRef> &&_worklist) {
    this->worklist = std::move(_worklist);
    bitAlias.clearCache();
    knownBits.clearCache();

    while (!worklist.empty()) {
      auto instr = worklist.pop_back_val();
      if (!instr.isOpc(opc))
        continue;
      runOnInstr(instr);
    }
  }

  void runWrapper(auto &&runFunc) {
    for (auto instr : ctx.getStore<Instr>())
      TaggedIRef{instr}.get() = 0;

    ctx.getStore<Instr>().createHooks.emplace_back([&](InstrRef ref) {
      TaggedIRef{ref}.get() = 0;
      worklist.emplace_back(ref);
    });

    runFunc();

    ctx.getStore<Instr>().createHooks.pop_back();
    destroyMarkedInstrs();
  }

  void run() {
    runWrapper([&]() {
      for (auto mod : ctx.getCtx<HWDialectContext>().activeModules()) {
        runOnModule(mod.iref());
      }
    });
  }
  void runModule(ModuleIRef mod) {
    runWrapper([&]() { runOnModule(mod); });
  }
  void runInstr(InstrRef instr) {
    runWrapper([&]() { runOnInstr(instr); });
  }
  void runBlock(BlockRef block) {
    runWrapper([&]() { runOnBlock(block); });
  }

  static constexpr auto runFuncs =
      mk_tuple(&InstCombinePass::runModule, &InstCombinePass::runInstr,
               &InstCombinePass::runBlock, &InstCombinePass::run);

public:
  explicit InstCombinePass(Context &ctx)
      : ctx(ctx), cbuild(ConstantBuilder{ctx.getStore<Constant>()}),
        bitAlias(ctx), deriveBits(ctx), loopbackAnalysis(ctx) {}
  static InstCombinePass make(Context &ctx) { return InstCombinePass{ctx}; }
};

}; // namespace dyno
