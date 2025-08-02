#pragma once

#include "dyno/Constant.h"
#include "dyno/CustomInstr.h"
#include "dyno/HierBlockIterator.h"
#include "dyno/Opcode.h"
#include "hw/FlipFlop.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Wire.h"
#include "hw/analysis/DemandedBits.h"
#include "hw/analysis/KnownBits.h"
#include "hw/analysis/BitAliasAnalysis.h"
#include "op/IDs.h"
#include "support/Debug.h"
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

private:
  bool knownBitsConstProp(InstrRef instr) {
    auto wire = instr.def(0)->as<WireRef>();
    auto known = knownBits.getKnownBits(wire);
    if (known.getIs4S())
      return false;
    currentReplaced.emplace_back(instr.def(0));
    wire.replaceAllUsesWith(cbuild.val(known).get());
    return true;
  }

  void deleteMatchedInstr(InstrRef instr) {
    TaggedIRef{instr}.get() = 1;
    currentMatched.emplace_back(instr);
  }

  void replaceUses(WireRef wire, HWValue newVal) {
    wire.replaceAllUsesWith(
        newVal, [&](OperandRef ref) { currentReplaced.emplace_back(ref); });
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
      build.buildInstrRaw(opc, 2).addRef(outWire).other().addRef(
          ibAdd.instr().def(0)->as<WireRef>());
    }

    instr.def(0).replace(FatDynObjRef<>{nullref});
    deleteMatchedInstr(instr);
    return true;
  }

  bool simplifyBitAliases(InstrRef instr) {
    auto defW = instr.def(0)->as<WireRef>();
    auto [aliases, change] = bitAlias.getReprAliases(defW);
    assert(aliases.frags.size() != 0);
    if (!change)
      return false;
    HWInstrBuilder build{ctx, instr};

    bool plainConcat = true;
    for (auto frag : aliases.frags)
      if (*ctx.resolveObj(frag.ref).as<HWValue>().getNumBits() != frag.len) {
        plainConcat = false;
        break;
      }

    if (plainConcat && aliases.frags.size() == 1) {
      HWValue val = ctx.resolveObj(aliases.frags.front().ref);
      replaceUses(defW, val);
      return true;
    }

    auto ib =
        build.buildInstrRaw(plainConcat ? HW_CONCAT : HW_SPLICE,
                            1 + (plainConcat ? 1 : 3) * aliases.frags.size());
    ib.addRef(instr.def(0)->as<WireRef>()).other();
    instr.def(0).replace(FatDynObjRef{nullref});
    for (auto frag : Range{aliases.frags}.reverse()) {
      ib.addRef(ctx.resolveObj(frag.ref));
      if (!plainConcat) {
        ib.addRef(ConstantRef::fromU32(frag.srcAddr));
        ib.addRef(ConstantRef::fromU32(frag.len));
      }
    }
    deleteMatchedInstr(instr);
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
      ibAdd.addRef(build.buildTrunc(activeBits, op->as<HWValue>()));
    }
    build.setInsertPoint(instr);

    instr.def(0).replace(FatDynObjRef<>{nullref});
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
          if (defI.isOpc(opc) && asWire.hasSingleUse()) {
            stack.emplace_back(defI);
            deleteMatchedInstr(defI);
            change = true;
            continue;
          }

          operands.emplace_back(operand->as<WireRef>());
          continue;
        }
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
          oldDefW.replaceAllUsesWith(cbuild.ones(*oldDefW->numBits).get());
          return true;
        }
        [[fallthrough]];
      case *OP_ADD:
        if (constants[0].valueEquals(0)) {
          change |= 1;
          constants.clear();
        }
        break;
      case *OP_AND:
        if (constants[0].valueEquals(0)) {
          deleteMatchedInstr(root);
          oldDefW.replaceAllUsesWith(cbuild.zero(*oldDefW->numBits).get());
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
          oldDefW.replaceAllUsesWith(cbuild.zero(*oldDefW->numBits).get());
          return true;
        }
        case *OP_MUL: {
          deleteMatchedInstr(root);
          oldDefW.replaceAllUsesWith(cbuild.one(*oldDefW->numBits).get());
          return true;
        }
        case *OP_AND: {
          deleteMatchedInstr(root);
          oldDefW.replaceAllUsesWith(cbuild.ones(*oldDefW->numBits).get());
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
          oldDefW.replaceAllUsesWith(constants[0]);
        else
          oldDefW.replaceAllUsesWith(operands[0]);
        return true;
      }
      dyno_unreachable("no 1-ary output value");
    }

    if (!change) {
      return false;
    }
    deleteMatchedInstr(root);
    HWInstrBuilder build{ctx};
    build.setInsertPoint(ctx.getCFG()[root]);

    std::stable_sort(operands.begin(), operands.end(),
                     [](WireRef lhs, WireRef rhs) {
                       return lhs.getSingleDef()->instr().getDialectOpcode() <
                              rhs.getSingleDef()->instr().getDialectOpcode();
                     });

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
    if (store.parentProc(ctx) != proc)
      return false;

    replaceUses(load.value(), store.value());
    deleteMatchedInstr(load);

    return true;
  }

  bool storeValConstProp(StoreIRef instr) {
    if (instr.hasRange())
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
      if (load.hasRange())
        return false;
      ref = load.reg();
    }

    RegisterRef reg = instr.reg();
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

    auto inverseUse = [&](OperandRef use) {
      auto instr = use.instr();
      HWInstrBuilder build{ctx, instr};
      build.insert = build.insert.succ();
      switch (*instr.getDialectOpcode()) {
      case *HW_LOAD: {
        auto defW = instr.def(0)->as<WireRef>();
        defW.replaceAllUsesWith(build.buildNot(defW), [&](OperandRef r) {
          currentReplaced.emplace_back(r);
        });
        break;
      }

      case *HW_TRIGGER_DEF: {
        auto asTrigger = instr.as<TriggerIRef>();
        uint idx = use - instr.other_begin();
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
      for (auto use : uses) {
        deleteMatchedInstr(use.instr());
        use.instr().def(0)->as<WireRef>().replaceAllUsesWith(
            ref, [&](OperandRef r) { currentReplaced.emplace_back(r); });
      }
    }

    deleteMatchedInstr(instr);
    return true;
  }

  bool manual(InstrRef instr) {
    if (instr.getNumDefs() == 1 && instr.def(0)->is<WireRef>())
      if (knownBitsConstProp(instr))
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
    case *HW_SPLICE: {
      if (simplifyBitAliases(instr))
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

    if (instr.isOpc(OP_ICMP_EQ, OP_ICMP_NE, /*OP_ICMP_CEQ, OP_ICMP_CNE,
                    OP_ICMP_WEQ, OP_ICMP_WNE, OP_ICMP_CZEQ, OP_ICMP_CZNE,
                    OP_ICMP_CXEQ, OP_ICMP_CXNE,*/
                    OP_ICMP_ULT, OP_ICMP_SLT, OP_ICMP_ULE, OP_ICMP_SLE,
                    OP_ICMP_UGT, OP_ICMP_SGT, OP_ICMP_UGE, OP_ICMP_SGE)) {
      if (reduceBitWidthICMP(instr))
        return true;
    }

    return false;
  }

  bool runOnInstr(InstrRef instr) {
    if (instr.getNumDefs() > 1)
      return false;
    if (instr.getNumDefs() == 1) {
      if (!instr.def(0)->is<WireRef>())
        return false;
      if (instr.def(0)->as<WireRef>().getNumUses() == 0) {
        // DCE unused instruction
        TaggedIRef{instr}.get() = 1;
        return false;
      }
    }

    currentMatched.clear();
    currentReplaced.clear();

    if (manual(instr))
      return true;

    currentMatched.clear();
    currentReplaced.clear();

    return generated(ctx, currentMatched, currentReplaced, instr);
  }

  void newInstrHook(InstrRef ref) {
    TaggedIRef{ref}.get() = 0;
    for (auto def : ref.defs()) {
      switch (*def->fat().getType()) {
      case *HW_WIRE: {
        auto asWire = def->as<WireRef>();
        for (auto use : asWire.uses())
          worklist.emplace_back(use.instr());
        break;
      }
      default:
        break;
      }
    }
  }

  void oldInstrHook(InstrRef old, ArrayRef<InstrRef> newInstrs) {
    for (auto newInstr : newInstrs)
      ctx.sourceLocInfo.copyDebugInfo(old, newInstr);
    if (TaggedIRef{old}.get())
      for (auto op : old) {
        op.replace(FatDynObjRef<>{nullref});
      }
  }

  void replacedUseHook(OperandRef replaced) {
    worklist.emplace_back(replaced.instr());
  }

  void runOnProcess(ProcessIRef proc) {
    worklist.clear();
    for (auto instr : HierBlockRange{proc.block()})
      worklist.emplace_back(instr);
    while (!worklist.empty()) {
      auto instr = worklist.pop_back_val();
      if (TaggedIRef{instr}.get())
        continue;
      auto lastWorklistSize = worklist.size();
      DEBUG("instcombine", { dumpInstr(instr); })
      if (!runOnInstr(instr))
        continue;
      DEBUG("instcombine", {
        dbgs() << "replaced with:\n";
        for (size_t i = lastWorklistSize, sz = worklist.size(); i < sz; i++)
          dumpInstr(worklist[i]);
        if (lastWorklistSize == worklist.size())
          dbgs() << "<none>\n";
      })

      auto newInstrs = ArrayRef<InstrRef>{worklist.begin() + lastWorklistSize,
                                          worklist.end()};
      for (auto instr : currentMatched) {
        oldInstrHook(instr, newInstrs);
      }
      for (auto operand : currentReplaced) {
        replacedUseHook(operand);
      }
      for (size_t i = lastWorklistSize, sz = worklist.size(); i < sz; i++)
        newInstrHook(worklist[i]);
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

    ctx.getInstrs().createHooks.emplace_back(
        [&](InstrRef ref) { worklist.emplace_back(ref); });

    for (auto mod : ctx.activeModules()) {
      runOnModule(mod.iref());
    }

    ctx.getInstrs().createHooks.pop_back();

    destroyMarkedInstrs();
  }

public:
  explicit InstCombinePass(HWContext &ctx)
      : ctx(ctx), cbuild(ctx.constBuild()), bitAlias(ctx) {}
};

}; // namespace dyno
