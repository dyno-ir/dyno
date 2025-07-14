#pragma once

#include "dyno/Constant.h"
#include "dyno/CustomInstr.h"
#include "dyno/HierBlockIterator.h"
#include "dyno/Opcode.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/Wire.h"
#include "hw/analysis/DemandedBits.h"
#include "hw/analysis/KnownBits.h"
#include "op/IDs.h"
#include "op/StructuredControlFlow.h"
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
    TaggedIRef{instr}.get() = 1;

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
      for (auto operand : instr.others()) {
        if (auto asWire = operand->dyn_as<WireRef>()) {
          auto defI = asWire.getSingleDef()->instr();
          if (defI.isOpc(opc) && asWire.hasSingleUse()) {
            stack.emplace_back(defI);
            currentMatched.emplace_back(defI);
            TaggedIRef{defI}.get() = 1;
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
          TaggedIRef{root}.get() = 1;
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
          TaggedIRef{root}.get() = 1;
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

    TaggedIRef{root}.get() = 1;
    if (operands.size() + constants.size() <= 1) {
      if (operands.size() + constants.size() == 0) {
        switch (*root.getDialectOpcode()) {
        case *OP_XOR:
        case *OP_ADD:
        case *OP_OR: {
          oldDefW.replaceAllUsesWith(cbuild.zero(*oldDefW->numBits).get());
          return true;
        }
        case *OP_MUL: {
          oldDefW.replaceAllUsesWith(cbuild.one(*oldDefW->numBits).get());
          return true;
        }
        case *OP_AND: {
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
        if (constants.size() == 1)
          oldDefW.replaceAllUsesWith(constants[0]);
        else
          oldDefW.replaceAllUsesWith(operands[0]);
        return true;
      }
      dyno_unreachable("no 1-ary output value");
    }

    if (!change) {
      TaggedIRef{root}.get() = 0;
      return false;
    }
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

    // todo: delete non-root
    for (auto op : root)
      op.replace(FatDynObjRef<>{nullref});
    return true;
  }

  bool manual(InstrRef instr) {
    switch (*instr.getDialectOpcode()) {
#define LAMBDA(opc, ib, cb, bib) case *opc:
      FOR_HW_COMM_OPS(LAMBDA)
#undef LAMBDA
      if (simplifyAndCanonicalizeCommOps(instr))
        return true;
    default:
    }

    if (instr.isOpc(OP_ADD, OP_MUL, OP_AND, OP_OR, OP_XOR)) {
      if (reduceBitWidth(instr))
        return true;
    }

    if (instr.getNumDefs() == 1 && instr.def(0)->is<WireRef>())
      if (knownBitsConstProp(instr))
        return true;

    return false;
  }

  bool runOnInstr(InstrRef instr) {
    if (instr.getNumDefs() > 1)
      return false;
    if (instr.getNumDefs() == 1) {
      if (!instr.def(0)->is<WireRef>())
        return false;
      if (instr.def(0)->as<WireRef>().getNumUses() == 0)
        return false;
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
      ctx.dbgInfo.copyDebugInfo(old, newInstr);
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
      oldInstrHook(instr, newInstrs);
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

    for (auto mod : ctx.getModules()) {
      runOnModule(mod.iref());
    }

    ctx.getInstrs().createHooks.pop_back();

    destroyMarkedInstrs();
  }

public:
  explicit InstCombinePass(HWContext &ctx)
      : ctx(ctx), cbuild(ctx.constBuild()) {}
};

}; // namespace dyno
