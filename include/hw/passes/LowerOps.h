#pragma once

#include "dyno/Constant.h"
#include "dyno/Obj.h"
#include "hw/AutoDebugInfo.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/analysis/DelayAnalysis.h"
#include "op/IDs.h"
#include "support/Bits.h"
#include "support/ErrorRecovery.h"
#include "support/Utility.h"
#include <cstdint>
#include <iterator>
namespace dyno {

class LowerOpsPass {
  HWContext &ctx;
  HWInstrBuilder build;
  ConstantBuilder cbuild;
  DelayAnalysis delay;

  SmallVec<InstrRef, 32> worklist;
  ObjMapVec<Instr, bool> destroyMap;

  AutoCopyDebugInfoStack autoDebugInfo;

public:
  struct Config {
    bool lowerMultiInputAdd = true;
    bool lowerAddCompress = true;
    bool lowerSimpleAdd = true;
    bool lowerSub = true;
    bool lowerMultiInputBitwise = true;
    bool lowerEqualityICMP = true;
    bool lowerOrderingICMP = true;
    bool lowerShift = true;
  };
  Config config;

private:
  auto getOperandsSortedByDelay(InstrRef instr) {
    SmallVec<std::pair<HWValue, uint32_t>, 8> operands;
    for (auto op : instr.others()) {
      Optional<uint32_t> delayEst = nullopt;
      if (op->is<ConstantRef>())
        delayEst = 0;
      else {
        auto asWire = op->as<WireRef>();
        delayEst = delay.getDelayOf(asWire);
      }
      operands.emplace_back(op->as<HWValue>(), delayEst.value_or(UINT32_MAX));
    }
    std::sort(operands.begin(), operands.end(),
              [](const auto &lhs, const auto &rhs) {
                return lhs.second > rhs.second;
              });
    return operands;
  }

  // given sorted list of operand/delay returns the idx of single bit operand
  // with maximum delay if it exists, otherwise nullref. "single bit" means
  // actually 1 bit wire or zero extended from one bit.
  Optional<uint32_t>
  findMaxDelaySingleBit(ArrayRef<std::pair<HWValue, uint32_t>> sorted) {
    auto begin = std::make_reverse_iterator(sorted.end());
    auto end = std::make_reverse_iterator(sorted.begin());
    auto it =
        std::find_if(begin, end, [&](const std::pair<HWValue, uint32_t> &pair) {
          if (auto wire = pair.first.dyn_as<WireRef>()) {
            if (wire.getNumBits() == 1)
              return true;
            if (wire.getDefI().isOpc(OP_ZEXT) &&
                wire.getDefI().other(0)->as<HWValue>().getNumBits() == 1)
              return true;
          }
          if (auto asConst = pair.first.dyn_as<ConstantRef>()) {
            return asConst.valueEquals(1);
          }
          return false;
        });

    if (it == end)
      return nullopt;

    return &*it - sorted.begin();
  }

  void lowerMultiInputAdd(InstrRef add) {
    const uint maxCompressIns = 3;

    if (add.getNumOthers() <= 2)
      return;

    build.setInsertPoint(HWInstrRef{add}.iter(ctx));

    auto operands = getOperandsSortedByDelay(add);
    HWValue carryBit = nullref;
    uint32_t carryBitDelay = 0;
    {
      auto carryBitIdx = findMaxDelaySingleBit(operands);
      if (carryBitIdx) {
        std::tie(carryBit, carryBitDelay) = operands[*carryBitIdx];
        operands.erase(operands.begin() + *carryBitIdx);
      }
    }

    auto numBits = *add.def(0)->as<WireRef>().getNumBits();
    while (operands.size() > 2) {
      auto compressIns = std::min(maxCompressIns, operands.size());
      auto ibuild = build.buildInstrRaw(HW_ADD_COMPRESS, compressIns + 2);
      auto sum = ctx.getWires().create(numBits);
      auto carry = ctx.getWires().create(numBits);
      ibuild.addRef(sum);
      ibuild.addRef(carry);
      ibuild.other();
      uint32_t worstDelay = 0;
      for (size_t i = 0; i < compressIns; i++) {
        auto [val, dl] = operands.pop_back_val();
        ibuild.addRef(val);
        worstDelay = dl;
      }
      auto comprDelay = delay.getDefDelay(ibuild.instr().def(0), nullptr);
      if (!comprDelay)
        worstDelay = UINT32_MAX;
      else if (worstDelay != UINT32_MAX)
        worstDelay += *comprDelay;

      auto it =
          std::find_if(operands.begin(), operands.end(), [&](const auto &pair) {
            return pair.second < worstDelay;
          });
      uint pos = it - operands.begin();

      operands.resize(operands.size() + 2);
      std::move_backward(operands.begin() + pos, operands.end() - 2,
                         operands.end());
      operands[pos] = std::make_pair(carry, worstDelay);
      operands[pos + 1] = std::make_pair(sum, worstDelay);
    }

    if (carryBit) {
      auto ibuild = build.buildInstrRaw(HW_ADD_CARRY, 4);
      ibuild.addRef(add.def(0)->as<WireRef>()).other();
      ibuild.addRef(operands[0].first);
      ibuild.addRef(operands[1].first);
      ibuild.addRef(build.buildTrunc(1, carryBit));
    } else {
      auto ibuild = build.buildInstrRaw(OP_ADD, 3);
      ibuild.addRef(add.def(0)->as<WireRef>()).other();
      ibuild.addRef(operands[0].first);
      ibuild.addRef(operands[1].first);
    }

    add.def(0).replace(FatDynObjRef<>{nullref});
    destroyMap[add] = 1;
  }

  void lowerAddCompress(InstrRef instr) {
    assert(instr.getNumOperands() == 5 && "only 3 input compress implemented");

    build.setInsertPoint(instr);

    auto valA = instr.other(0)->as<HWValue>();
    auto valB = instr.other(1)->as<HWValue>();
    auto valC = instr.other(2)->as<HWValue>();

    auto ibXOR = build.buildInstrRaw(OP_XOR, 4);
    ibXOR.addRef(instr.def(0)->as<WireRef>());
    ibXOR.other();
    ibXOR.addRef(valA);
    ibXOR.addRef(valB);
    ibXOR.addRef(valC);

    auto temp =
        build.buildOr(build.buildAnd(valA, valB), build.buildAnd(valA, valC),
                      build.buildAnd(valB, valC));

    build.buildInstrRaw(OP_SLL, 3)
        .addRef(instr.def(1)->as<WireRef>())
        .other()
        .addRef(temp)
        .addRef(cbuild.oneLike(temp).get());

    instr.def(0).replace(FatDynObjRef<>{nullref});
    instr.def(1).replace(FatDynObjRef<>{nullref});
    destroyMap[instr] = 1;
  }

  struct AddPair {
    HWValue gen;
    HWValue prop;
  };

  AddPair combineAddPairs(const AddPair &lhs, const AddPair &rhs) {
    return AddPair{build.buildOr(lhs.gen, build.buildAnd(lhs.prop, rhs.gen)),
                   build.buildAnd(lhs.prop, rhs.prop)};
  }

  void lowerAddBrentKung(MutArrayRef<AddPair> pairs) {
    uint32_t bits = pairs.size();
    uint32_t step = 2;
    // 1. build sparse reduction tree
    while (step <= bits) {
      uint32_t shift = step / 2;
      for (uint32_t i = step - 1; i < bits; i += step) {
        pairs[i] = combineAddPairs(pairs[i], pairs[i - shift]);
      }
      step *= 2;
    }
    uint32_t clogBits = clog2(bits);
    uint32_t bitsPow2 = 1u << clogBits;
    step = bitsPow2 / 2;

    // 2. fixup the bits that were left out in every step
    while (step > 1) {
      uint32_t shift = step / 2;
      for (int i = bitsPow2 - shift - 1; i >= (int)step; i -= step) {
        if (i >= (int)pairs.size())
          continue;
        pairs[i] = combineAddPairs(pairs[i], pairs[i - shift]);
      }
      step /= 2;
    }
  }

  void lowerAdd(InstrRef instr) {
    auto lhs = instr.other(0)->as<HWValue>();
    auto rhs = instr.other(1)->as<HWValue>();

    build.setInsertPoint(instr);

    HWValue gen = build.buildAnd(lhs, rhs);
    HWValue prop = build.buildXor(lhs, rhs);
    uint32_t bits = *lhs.getNumBits();

    SmallVec<AddPair, 32> arr;
    arr.reserve(bits);
    for (uint32_t i = 0; i < bits; i++) {
      HWValue propBit = build.buildSplice(prop, BitRange{i, 1});
      HWValue genBit = build.buildSplice(gen, BitRange{i, 1});
      arr.emplace_back(genBit, propBit);
    }

    SmallVec<AddPair, 32> processed(arr);
    lowerAddBrentKung(processed);

    auto ibCC = build.buildInstrRaw(HW_CONCAT, 1 + bits);
    build.setInsertPoint(ibCC.instr());
    ibCC.addRef(instr.def(0)->as<WireRef>()).other();
    for (uint32_t i = bits; i-- > 0;) {
      HWValue sum;
      if (i == 0)
        sum = arr[0].prop;
      else {
        sum = build.buildXor(arr[i].prop, processed[i - 1].gen);
      }
      ibCC.addRef(sum);
    }

    instr.def(0).replace(FatDynObjRef<>{nullref});
    destroyMap[instr] = 1;
  }

  void lowerBitwise(InstrRef instr) {
    const uint maxFactor = 2;

    if (instr.getNumOthers() <= 2)
      return;

    auto operands = getOperandsSortedByDelay(instr);

    build.setInsertPoint(HWInstrRef{instr}.iter(ctx));
    auto numBits = *instr.def(0)->as<WireRef>().getNumBits();
    InstrRef last;
    while (operands.size() > 1) {
      auto compressIns = std::min(maxFactor, operands.size());
      auto ibuild =
          build.buildInstrRaw(instr.getDialectOpcode(), 1 + compressIns);
      auto sum = ctx.getWires().create(numBits);
      ibuild.addRef(sum);
      ibuild.other();
      uint32_t worstDelay = 0;
      for (size_t i = 0; i < compressIns; i++) {
        auto [val, dl] = operands.pop_back_val();
        ibuild.addRef(val);
        worstDelay = dl;
      }
      auto comprDelay = delay.getDefDelay(ibuild.instr().def(0), nullptr);
      if (!comprDelay)
        worstDelay = UINT32_MAX;
      else if (worstDelay != UINT32_MAX)
        worstDelay += *comprDelay;

      auto it =
          std::find_if(operands.begin(), operands.end(), [&](const auto &pair) {
            return pair.second < worstDelay;
          });
      operands.insert(it, std::make_pair(sum, worstDelay));
      last = ibuild.instr();
    }

    instr.def(0)->as<WireRef>().replaceAllUsesWith(last.def(0)->as<HWValue>());
    destroyMap[instr] = 1;
  }

  // probably a good idea to express this and stuff like shift in terms of high
  // level instructions still. that way we can re run instcomb to handle cases
  // like (a - b) > 10 to a simple sum.
  void lowerOrderingICMP(InstrRef instr) {
    auto numBits = *instr.other(0)->as<HWValue>().getNumBits();
    build.setInsertPoint(instr);
    auto ibuild = build.buildInstrRaw(OP_SUB, 3);
    auto sumWire = ctx.getWires().create(numBits);
    ibuild.addRef(sumWire);
    ibuild.other();

    build.setInsertPoint(ibuild.instr());

    HWValue lhs = build.buildZExt(numBits + 1, instr.other(0)->as<HWValue>());
    HWValue rhs = build.buildZExt(numBits + 1, instr.other(1)->as<HWValue>());
    bool invertResult = false;
    bool isSigned = false;
    switch (*instr.getDialectOpcode()) {
    case *OP_ICMP_SLT:
      isSigned = true;
      [[fallthrough]];
    case *OP_ICMP_ULT:
      break;

    case *OP_ICMP_SLE:
      isSigned = true;
      [[fallthrough]];
    case *OP_ICMP_ULE:
      std::swap(lhs, rhs);
      invertResult = true;
      break;

    case *OP_ICMP_SGT:
      isSigned = true;
      [[fallthrough]];
    case *OP_ICMP_UGT:
      std::swap(lhs, rhs);
      break;

    case *OP_ICMP_SGE:
      isSigned = true;
      [[fallthrough]];
    case *OP_ICMP_UGE:
      invertResult = true;
      break;

    default:
      dyno_unreachable("unknown opcode");
    }
    ibuild.addRef(lhs);
    ibuild.addRef(rhs);

    build.setInsertPoint(instr);

    auto carryWire = build.buildSplice(sumWire, BitRange{numBits - 1, 1});
    HWValue out;
    if (isSigned) {
      HWValue signBit = build.buildSplice(sumWire, BitRange{numBits - 2, 1});
      out = build.buildXor(signBit, carryWire);
    }
    out = invertResult ? build.buildNot(carryWire) : carryWire;
    instr.def(0)->as<WireRef>().replaceAllUsesWith(out);
    destroyMap[instr] = 1;
  }

  HWValue shiftByConstant(DialectOpcode opcode, HWValue value, uint32_t shamt) {
    uint32_t valueBits = *value.getNumBits();
    HWValue val;
    if (shamt == 0)
      val = value;
    else if (opcode.is(OP_SLL)) {
      val = build.buildSplice(value, BitRange{0, valueBits - shamt},
                              cbuild.zero(shamt).get(), BitRange{0, shamt});
    } else if (opcode.is(OP_SRL)) {
      val = build.buildSplice(cbuild.zero(shamt).get(), BitRange{0, shamt},
                              value, BitRange{shamt, valueBits - shamt});
    } else if (opcode.is(OP_SRA)) {
      auto upper = build.buildSplice(value, BitRange{shamt, valueBits - shamt});
      auto signBit = build.buildSplice(value, BitRange{valueBits - 1, 1});
      auto signPat = build.buildRepeat(signBit, ConstantRef::fromU32(shamt));
      val = build.buildConcat(std::to_array({signPat, upper}));
    } else
      dyno_unreachable("invalid opcode");
    return val;
  }

  void lowerShift(InstrRef instr) {
    auto value = instr.other(0)->as<HWValue>();
    auto shamt = instr.other(1)->as<HWValue>();
    if (auto asConstant = shamt.dyn_as<ConstantRef>()) {
      auto shamtC = asConstant.getLimitedVal();
      if (!shamtC)
        report_fatal_error("shift amount too large");
      auto val = shiftByConstant(instr.getDialectOpcode(), value, *shamtC);
      instr.def(0)->as<WireRef>().replaceAllUsesWith(val);
      destroyMap[instr] = 1;
      return;
    }

    auto valueBits = *value.getNumBits();
    auto inBoundsShamtBits = clog2(valueBits);

    build.setInsertPoint(instr);

    HWValue shiftVal = value;
    for (uint i = 0; i < inBoundsShamtBits; i++) {
      auto shifted =
          shiftByConstant(instr.getDialectOpcode(), shiftVal, 1u << i);
      auto cond = build.buildSplice(shamt, BitRange{i, 1});
      shiftVal = build.buildMux(cond, shifted, shiftVal);
    }

    auto oobBits = *shamt.getNumBits() - inBoundsShamtBits;
    auto oob = build.buildICmp(
        build.buildSplice(shamt, BitRange{inBoundsShamtBits, oobBits}),
        cbuild.zero(oobBits).get(), BigInt::ICmpPred::ICMP_NE);
    auto oobVal =
        instr.isOpc(OP_SRA)
            ? build.buildRepeat(
                  build.buildSplice(value, BitRange{valueBits - 1, 1}),
                  ConstantRef::fromU32(oobBits))
            : cbuild.zeroLike(value).get();
    instr.def(0)->as<WireRef>().replaceAllUsesWith(
        build.buildMux(oob, oobVal, shiftVal));
    destroyMap[instr] = 1;
  }

  void lowerEqualityICMP(InstrRef instr) {
    auto lhs = instr.other(0)->as<HWValue>();
    auto rhs = instr.other(1)->as<HWValue>();
    auto bits = *lhs.getNumBits();

    build.setInsertPoint(instr);
    auto bitsMask = build.buildXor(lhs, rhs);
    auto ibOR = build.buildInstrRaw(OP_OR, 1 + bits);

    if (instr.isOpc(OP_ICMP_EQ)) {
      auto tmp = ctx.getWires().create(1);
      ibOR.addRef(tmp);
      build.buildInstrRaw(OP_NOT, 2)
          .addRef(instr.def(0)->as<WireRef>())
          .other()
          .addRef(tmp);
    } else {
      ibOR.addRef(instr.def(0)->as<WireRef>());
    }
    ibOR.other();
    instr.def(0).replace(FatDynObjRef<>{nullref});
    build.setInsertPoint(ibOR.instr());

    for (uint i = 0; i < bits; i++) {
      auto bit = build.buildSplice(bitsMask, BitRange{i, 1});
      ibOR.addRef(bit);
    }

    destroyMap[instr] = 1;
  }

  void lowerSub(InstrRef instr) {
    // simple situations like here inline DSL would be nice
    build.setInsertPoint(instr);
    auto lhs = instr.other(0)->as<HWValue>();
    auto rhs = instr.other(1)->as<HWValue>();
    auto addRes =
        build.buildAdd(lhs, build.buildNot(rhs), cbuild.oneLike(lhs).get());
    instr.def(0)->as<WireRef>().replaceAllUsesWith(addRes);
    destroyMap[instr] = 1;
  }

  void runOnInstr(InstrRef instr) {
    auto token = autoDebugInfo.addWithToken(instr);

    switch (*instr.getDialectOpcode()) {
    case *OP_ADD:
      if (instr.getNumOthers() >= 3) {
        if (config.lowerMultiInputAdd)
          lowerMultiInputAdd(instr);
      } else {
        if (config.lowerSimpleAdd)
          lowerAdd(instr);
      }
      break;

    case *HW_ADD_COMPRESS:
      lowerAddCompress(instr);
      break;

    case *OP_SUB:
      lowerSub(instr);
      break;

    case *OP_ICMP_ULT:
    case *OP_ICMP_SLT:
    case *OP_ICMP_ULE:
    case *OP_ICMP_SLE:
    case *OP_ICMP_UGT:
    case *OP_ICMP_SGT:
    case *OP_ICMP_UGE:
    case *OP_ICMP_SGE:
      lowerOrderingICMP(instr);
      break;

    case *OP_ICMP_EQ:
    case *OP_ICMP_NE:
      lowerEqualityICMP(instr);
      break;

    case *OP_AND:
    case *OP_OR:
    case *OP_XOR:
      lowerBitwise(instr);
      break;

    case *OP_SLL:
    case *OP_SRL:
    case *OP_SRA:
      lowerShift(instr);
      return;
    }
  }

public:
  void run() {
    auto isLoweringInstr = [&](InstrRef instr) {
      if (instr.isOpc(OP_ADD))
        return config.lowerMultiInputAdd || config.lowerSimpleAdd;

      if (instr.isOpc(HW_ADD_COMPRESS))
        return config.lowerAddCompress;

      if (instr.isOpc(OP_SUB))
        return config.lowerSub;

      if (instr.isOpc(OP_ICMP_ULT, OP_ICMP_SLT, OP_ICMP_ULE, OP_ICMP_SLE,
                      OP_ICMP_UGT, OP_ICMP_SGT, OP_ICMP_UGE, OP_ICMP_SGE))
        return config.lowerOrderingICMP;

      if (instr.isOpc(OP_ICMP_EQ, OP_ICMP_NE))
        return config.lowerEqualityICMP;

      if (instr.isOpc(OP_SLL, OP_SRL, OP_SRA))
        return config.lowerShift;

      if (instr.isOpc(OP_AND, OP_OR, OP_XOR))
        return config.lowerMultiInputBitwise;

      return false;
    };

    destroyMap.clear();
    destroyMap.resize(ctx.getInstrs().numIDs());
    ctx.getInstrs().createHooks.emplace_back([&](InstrRef ref) {
      if (isLoweringInstr(ref))
        worklist.emplace_back(ref);
      destroyMap.get_ensure(ref) = 0;
    });

    for (auto instr : ctx.getInstrs()) {
      if (isLoweringInstr(instr))
        worklist.emplace_back(instr);
    }

    while (!worklist.empty()) {
      runOnInstr(worklist.pop_back_val());
    }

    ctx.getInstrs().createHooks.pop_back();

    for (auto [obj, destroy] : destroyMap) {
      if (!destroy || !ctx.getInstrs().exists(obj))
        continue;
      build.destroyInstr(ctx.getInstrs().resolve(obj));
    }
  }
  explicit LowerOpsPass(HWContext &ctx)
      : ctx(ctx), build(ctx), cbuild(ctx.getConstants()), autoDebugInfo(ctx) {}
};

}; // namespace dyno
