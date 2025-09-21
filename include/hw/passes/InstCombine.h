#pragma once

#include "dyno/Constant.h"
#include "dyno/CustomInstr.h"
#include "dyno/HierBlockIterator.h"
#include "dyno/IDs.h"
#include "dyno/Instr.h"
#include "dyno/Opcode.h"
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
#include "hw/analysis/RegisterValue.h"
#include "op/IDs.h"
#include "op/StructuredControlFlow.h"
#include "support/Bits.h"
#include "support/Debug.h"
#include "support/DynBitSet.h"
#include "support/ErrorRecovery.h"
#include "support/Utility.h"
#include <algorithm>

namespace dyno {

bool generated(HWContext &ctx, SmallVecImpl<InstrRef> &matched,
               SmallVecImpl<OperandRef> &replaced, HWInstrRef);

class InstCombinePass {
  HWContext &ctx;
  SmallVec<InstrRef, 128> worklist;
  SmallVec<InstrRef, 8> currentMatched;
  SmallVec<OperandRef, 4> currentReplaced;
  ConstantBuilder cbuild;

  // todo: outside of pass
  KnownBitsAnalysis knownBits;
  DemandedBitsAnalysis demandedBits;
  BitAliasAnalysis bitAlias;

public:
  using TaggedIRef = CustomInstrRef<InstrRef, uint64_t>;

  struct Config {
    bool fuseCommutative = true;
    bool liftMUX = false;
  };
  Config config;

private:
  bool knownBitsConstProp(InstrRef instr) {
    auto wire = instr.def(0)->as<WireRef>();
    auto known = knownBits.getKnownBits(wire);
    if (known.getIs4S())
      return false;
    assert(known.getNumBits() == wire.getNumBits());
    replaceUses(wire, cbuild.val(known).get());
    deleteMatchedInstr(instr);
    return true;
  }

  void deleteMatchedInstr(InstrRef instr) {
    TaggedIRef{instr}.get() = 1;
    currentMatched.emplace_back(instr);
  }

  void replaceUses(WireRef wire, HWValue newVal) {
    assert(newVal.getNumBits() == wire.getNumBits());
    wire.replaceAllUsesWith(
        newVal, [&](OperandRef ref) { currentReplaced.emplace_back(ref); });
    knownBits.replaceAt(wire, newVal);
    bitAlias.replaceAt(wire, newVal);
  }

  bool reduceBitWidth(InstrRef instr) {
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

    WireRef intermWire = ctx.getWires().create(activeBits);

    HWInstrBuilder build{ctx};
    build.setInsertPoint(instr);
    auto ibAdd =
        build.buildInstrRaw(instr.getDialectOpcode(), instr.getNumOperands());
    ibAdd.addRef(intermWire).other();

    build.setInsertPoint(ibAdd.instr());

    for (auto op : instr.others()) {
      ibAdd.addRef(build.buildTrunc(activeBits, op->as<HWValue>()));
    }

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
          .addRef(ibAdd.instr().def(0)->as<WireRef>());
    } else {
      DialectOpcode opc;
      if (reduceViaInputs)
        opc = sign ? OP_SEXT : OP_ZEXT;
      else
        opc = OP_ANYEXT;
      auto tempW = ibAdd.instr().def(0)->as<WireRef>();
      assert(*outWire->numBits >= *tempW.getNumBits());
      build.buildInstrRaw(opc, 2).addRef(outWire).other().addRef(tempW);
    }

    // instr.def(0).replace(FatDynObjRef<>{nullref});
    deleteMatchedInstr(instr);
    return true;
  }

  bool liftMUX(InstrRef instr) {
    if (!config.liftMUX)
      return false;
    auto selV = instr.other(0)->as<HWValue>();
    auto trueV = instr.other(1)->as<HWValue>();
    auto falseV = instr.other(2)->as<HWValue>();

    auto bits = *trueV.getNumBits();

    auto [trueRepr, _] = bitAlias.getReprAliases(trueV);
    auto [falseRepr, __] = bitAlias.getReprAliases(falseV);

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
    return true;
  }

  bool fuseInserts(InsertIRef insert) {
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
      return true;
    }
    return false;
  }

  bool simplifyDeMorgan(InstrRef instr) {
    uint inverted = 0;
    for (auto op : instr.others()) {
      if (op->is<WireRef>() && op->as<WireRef>().getDefI().isOpc(OP_NOT))
        inverted++;
    }
    if (inverted <= round_up_div(instr.getNumOthers(), 2u))
      return false;

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
    return true;
  }

  bool simplifyBitAliases(InstrRef instr) {
    // Main function for canoncializing addressing. This gets quite ugly, we
    // have to match instrs to see if the existing canonical pattern exists
    // already (return false if it does) and else build it. Currently doing all
    // the compares manually. Maybe implement this w/ DSL or continuous CSE (
    // build abstract instructions and check if equal to actual) in the future.

    auto defW = instr.def(0)->as<WireRef>();
    auto [aliases, change] = bitAlias.getReprAliases(defW);
    assert(aliases.frags.size() != 0);
    HWInstrBuilder build{ctx, instr};

    auto operandEqualsFrag = [&](HWValue val,
                                 RegisterValueFragment &frag) -> HWValue {
      auto instr = !val.is<WireRef>() ? nullref : val.as<WireRef>().getDefI();
      auto fragRef = ctx.resolveObj(frag.ref);
      if (frag.srcAddr == 0 && frag.ref == val && frag.len == val.getNumBits())
        return nullref;
      if (frag.srcAddr == 0) {
        if (instr && instr.isOpc(OP_TRUNC) &&
            instr.other(0)->as<HWValue>() == frag.ref)
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
      return true;
    }

    auto matchInsert = [&]() -> Optional<int> {
      auto &front = aliases.frags[0];
      auto &mid = aliases.frags[1];
      auto &end = aliases.frags[2];
      if (front.ref != end.ref || front.srcAddr != 0 ||
          end.srcAddr != end.dstAddr)
        return nullopt;
      auto pad = ctx.resolveObj(front.ref).as<HWValue>();
      if (pad.getNumBits() != aliases.getLen())
        return nullopt;
      auto midRef = ctx.resolveObj(mid.ref).as<HWValue>();
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
        return *rv;

    SmallVec<HWValue, 16> operands;
    bool anyMismatch = false;

    InstrRef concat =
        (instr.isOpc(HW_CONCAT) && instr.getNumOthers() == aliases.frags.size())
            ? instr
            : nullref;
    for (uint i = 0; i < aliases.frags.size(); i++) {
      auto &frag = aliases.frags[aliases.frags.size() - i - 1];
      auto ref = ctx.resolveObj(frag.ref);
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
    return true;
  }

  bool coalesceConcatOfLoads(InstrRef concat) {
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
    return true;
  }

  bool reduceBitWidthICMP(InstrRef instr) {
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
        return true;
      }
      BigInt::ICmpPred pred =
          BigInt::ICmpPred(instr.getOpcode().num - OP_ICMP_EQ.opc.num);
      assert(pred >= BigInt::ICmpPred::ICMP_EQ &&
             pred <= BigInt::ICmpPred::ICMP_SGE);
      replaceUses(outWire, ConstantRef::fromBool(BigInt::icmpOp(
                               inputKnownBits[0], inputKnownBits[1], pred)));
      return true;
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
    return true;
  }

  bool simplifyAndCanonicalizeCommOps(InstrRef root) {
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
      //   return HWInstrBuilder{ctx, ctx.getCFG()[root]}.buildExt(
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
          return true;
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
          return true;
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
          return true;
        }
        case *OP_MUL: {
          deleteMatchedInstr(root);
          replaceUses(oldDefW, cbuild.one(*oldDefW->numBits).get());
          return true;
        }
        case *OP_AND: {
          deleteMatchedInstr(root);
          replaceUses(oldDefW, cbuild.ones(*oldDefW->numBits).get());
          return true;
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
        return true;
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
    build.setInsertPoint(ctx.getCFG()[root]);

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

    return true;
  }

  bool nonTemporalLoadElim(LoadIRef load) {
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

    return true;
  }

  bool storeValConstProp(StoreIRef instr) {
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
      return true;
    }

    auto inverseUse = [&](OperandRef use) {
      auto instr = use.instr();
      HWInstrBuilder build{ctx, instr};
      build.insert = build.insert.succ();
      switch (*instr.getDialectOpcode()) {
      case *HW_LOAD: {
        auto defW = instr.def(0)->as<WireRef>();
        auto invW = ctx.getWires().create(*defW.getNumBits());
        replaceUses(defW, invW);
        build.buildInstrRaw(OP_NOT, 2).addRef(invW).other().addRef(defW);
        break;
      }

      case *HW_TRIGGER_DEF: {
        auto asTrigger = instr.as<TriggerIRef>();
        uint idx = use - *instr.other_begin();
        asTrigger.oref()->inverseMode(idx);
        break;
      }

      case *HW_FLIP_FLOP: {
        auto asFF = instr.as<FlipFlopIRef>();
        asFF.flipPolarity(use);
        break;
      }

      case *HW_LATCH: // todo;

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
    return true;
  }

  template <typename Ref> bool simplifyYieldValues(Ref instr) {
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

    auto unused = [&](uint i) {
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
      if (idx >= 0 && (uint)idx < instr.getNumYieldValues()) {
        if (unused(idx))
          continue;
        if (equal[idx]) {
          replaceUses(def->template as<WireRef>(), yieldValues[idx]);
          continue;
        }
      }
      ib.addRef(def->fat());
    }
    ib.other();
    ib.addRefs(instr.others().transform(
        [](size_t, OperandRef ref) { return ref->fat(); }));

    deleteMatchedInstr(instr);
    return true;
  }

  template <typename RefT> bool simplifyAddressing(RefT instr) {
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
        return true;
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

    uint termDiff = instr.getNumTerms() - terms.size();
    uint numOperands = instr.getNumOperands() - termDiff * 3;
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
    return true;
  }

  bool manual(InstrRef instr) {
    if (instr.getNumDefs() == 1 && instr.def(0)->is<WireRef>())
      if (knownBitsConstProp(instr))
        return true;

    if (instr.isOpc(HW_SPLICE))
      if (simplifyAddressing(instr.as<SpliceIRef>()))
        return true;
    if (instr.isOpc(HW_INSERT))
      if (simplifyAddressing(instr.as<InsertIRef>()))
        return true;
    if (instr.isOpc(HW_LOAD))
      if (simplifyAddressing(instr.as<LoadIRef>()))
        return true;
    if (instr.isOpc(HW_STORE, HW_STORE_DEFER))
      if (simplifyAddressing(instr.as<StoreIRef>()))
        return true;

    switch (*instr.getDialectOpcode()) {
#define LAMBDA(opc, ib, cb, bib) case *opc:
      FOR_HW_COMM_OPS(LAMBDA)
#undef LAMBDA
      if (simplifyAndCanonicalizeCommOps(instr))
        return true;
      break;

    case *HW_STORE:
    case *HW_STORE_DEFER: {
      if (storeValConstProp(instr.as<StoreIRef>()))
        return true;
      break;
    }

    case *HW_LOAD: {
      if (nonTemporalLoadElim(instr.as<LoadIRef>()))
        return true;
      break;
    }

    case *HW_CONCAT:
    case *OP_TRUNC:
    case *HW_SPLICE:
    case *HW_INSERT: {
      if (simplifyBitAliases(instr))
        return true;
      break;
    }

    case *OP_YIELD: {
      auto parent = HWInstrRef{instr}.parentBlock(ctx).defI();
      if (parent.isOpc(OP_IF)) {
        if (simplifyYieldValues(instr.as<IfInstrRef>()))
          return true;
      } else if (parent.isOpc(OP_CASE, OP_CASE_DEFAULT, HW_CASE_Z, HW_CASE_X)) {
        auto swInstr =
            HWInstrRef{parent}.parentBlock(ctx).defI().as<SwitchInstrRef>();
        assert(swInstr.isOpc(OP_SWITCH));
        if (simplifyYieldValues(swInstr))
          return true;
      }
      break;
    }

    case *OP_IF: {
      if (simplifyYieldValues(instr.as<IfInstrRef>()))
        return true;
      break;
    }

    case *OP_SWITCH: {
      if (simplifyYieldValues(instr.as<SwitchInstrRef>()))
        return true;
      break;
    }

    case *HW_MUX: {
      if (liftMUX(instr))
        return true;
      break;
    }

    default:
      break;
    }

    if (instr.isOpc(OP_ADD, OP_MUL, OP_AND, OP_OR, OP_XOR)) {
      if (reduceBitWidth(instr))
        return true;
    }

    if (instr.isOpc(OP_AND, OP_OR))
      if (simplifyDeMorgan(instr))
        return true;

    if (instr.isOpc(OP_ICMP_EQ, OP_ICMP_NE, /*OP_ICMP_CEQ, OP_ICMP_CNE,
                    OP_ICMP_WEQ, OP_ICMP_WNE, OP_ICMP_CZEQ, OP_ICMP_CZNE,
                    OP_ICMP_CXEQ, OP_ICMP_CXNE,*/
                    OP_ICMP_ULT, OP_ICMP_SLT, OP_ICMP_ULE, OP_ICMP_SLE,
                    OP_ICMP_UGT, OP_ICMP_SGT, OP_ICMP_UGE, OP_ICMP_SGE)) {
      if (reduceBitWidthICMP(instr))
        return true;
    }

    if (instr.isOpc(HW_CONCAT))
      if (coalesceConcatOfLoads(instr))
        return true;

    if (instr.isOpc(HW_INSERT))
      if (fuseInserts(instr.as<InsertIRef>()))
        return true;

    return false;
  }

  bool matchPatternsOnInstr(InstrRef instr) {
    currentMatched.clear();
    currentReplaced.clear();

    if (instr.getNumDefs() == 1) {
      if (!instr.def(0)->is<WireRef>())
        return false;
      if (instr.def(0)->as<WireRef>().getNumUses() == 0) {
        // DCE unused instruction
        deleteMatchedInstr(instr);
        return true;
      }
    }

    if (manual(instr))
      return true;

    currentMatched.clear();
    currentReplaced.clear();

    if (instr.getNumDefs() > 1)
      return false;
    return generated(ctx, currentMatched, currentReplaced, instr);
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
      ctx.sourceLocInfo.copyDebugInfo(old, newInstr);
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

    if (!matchPatternsOnInstr(instr))
      return;

    anyMatchHook();

    DEBUG("instcombine", {
      HWPrinter print{dbgs()};
      dbgs() << "initial instructions:\n";

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
    for (auto instr : ctx.getInstrs()) {
      if (TaggedIRef{instr}.get())
        build.destroyInstr(instr);
    }
  }

public:
  void run() {
    for (auto instr : ctx.getInstrs())
      TaggedIRef{instr}.get() = 0;

    ctx.getInstrs().createHooks.emplace_back([&](InstrRef ref) {
      TaggedIRef{ref}.get() = 0;
      worklist.emplace_back(ref);
    });

    // ctx.getWires().createHooks.emplace_back(
    //     [&](WireRef wire) { knownBits.cache.clear(wire); });

    for (auto mod : ctx.activeModules()) {
      runOnModule(mod.iref());
    }

    ctx.getInstrs().createHooks.pop_back();
    // ctx.getWires().createHooks.pop_back();

    destroyMarkedInstrs();
  }

  void run(BlockRef block) {
    for (auto instr : ctx.getInstrs())
      TaggedIRef{instr}.get() = 0;
    ctx.getInstrs().createHooks.emplace_back([&](InstrRef ref) {
      TaggedIRef{ref}.get() = 0;
      worklist.emplace_back(ref);
    });
    runOnBlock(block);
    ctx.getInstrs().createHooks.pop_back();
    destroyMarkedInstrs();
  }

public:
  explicit InstCombinePass(HWContext &ctx)
      : ctx(ctx), cbuild(ctx.constBuild()), bitAlias(ctx) {}
};

}; // namespace dyno
