#pragma once

#include "dyno/Constant.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/HWValue.h"
#include "hw/analysis/DelayAnalysis.h"
#include "op/IDs.h"
#include "support/ErrorRecovery.h"
#include "support/Utility.h"
#include <cstdint>
namespace dyno {

class LowerOpsPass {
  HWContext &ctx;
  HWInstrBuilder build;
  ConstantBuilder cbuild;
  DelayAnalysis delay;

  SmallVec<InstrRef, 32> worklist;
  ObjMapVec<Instr, bool> destroyMap;

  void lowerAdd(InstrRef add) {
    const uint maxCompressIns = 3;

    if (add.getNumOperands() <= 2)
      return;

    SmallVec<std::pair<HWValue, uint32_t>, 8> operands;
    for (auto op : add.others()) {
      Optional<uint32_t> delayEst = nullopt;
      if (op->is<ConstantRef>())
        delayEst = 0;
      else {
        auto asWire = op->as<WireRef>();
        delayEst = delay.getDelayOf(asWire);
      }
      operands.emplace_back(op->as<HWValue>(), delayEst.value_or(UINT32_MAX));
    }
    if (operands.size() > maxCompressIns)
      std::sort(operands.begin(), operands.end(),
                [](const auto &lhs, const auto &rhs) {
                  return lhs.second > rhs.second;
                });

    build.setInsertPoint(HWInstrRef{add}.iter(ctx));
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

    auto ibuild = build.buildInstrRaw(OP_ADD, 3);
    ibuild.addRef(add.def(0)->as<WireRef>()).other();
    ibuild.addRef(operands[0].first);
    ibuild.addRef(operands[1].first);

    add.def(0).replace(FatDynObjRef<>{nullref});
    build.destroyInstr(add);
  }

  // probably a good idea to express this and stuff like shift in terms of high
  // level instructions still. that way we can re run instcomb to handle cases
  // like (a - b) > 10 to a simple sum.
  void lowerOrderingICMP(InstrRef instr) {
    auto numBits = *instr.other(0)->as<HWValue>().getNumBits();
    build.setInsertPoint(instr);
    auto ibuild = build.buildInstrRaw(HW_ADD_CARRY, 5);
    auto carryWire = ctx.getWires().create(1);
    ibuild.addRef(carryWire);
    auto sumWire = ctx.getWires().create(numBits);
    ibuild.addRef(sumWire);
    ibuild.other();

    HWValue lhs = instr.other(0)->as<HWValue>();
    HWValue rhs = instr.other(1)->as<HWValue>();
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

    build.setInsertPoint(build.insert.pred());
    ibuild.addRef(lhs);
    ibuild.addRef(build.buildXNor(rhs));
    ibuild.addRef(ConstantRef::fromBool(1));

    build.setInsertPoint(instr);

    HWValue out;
    if (isSigned) {
      HWValue signBit = build.buildSplice(sumWire, BitRange{numBits - 1, 1});
      out = invertResult ? build.buildXNor(signBit, carryWire)
                         : build.buildXor(signBit, carryWire);
    } else {
      out = invertResult ? build.buildXNor(carryWire) : carryWire;
    }
    instr.def(0)->as<WireRef>().replaceAllUsesWith(out);
    build.destroyInstr(instr);
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
    build.buildMux(oob, oobVal, shiftVal);
    instr.def(0)->as<WireRef>().replaceAllUsesWith(shiftVal);
    build.destroyInstr(instr);
  }

  void runOnInstr(InstrRef instr) {
    switch (*instr.getDialectOpcode()) {
    case *OP_ADD:
      lowerAdd(instr);
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

    case *OP_SLL:
    case *OP_SRL:
    case *OP_SRA:
      lowerShift(instr);
      return;
    }
  }

public:
  void run() {
    auto isLoweringInstr = [](InstrRef instr) {
      return instr.isOpc(OP_ADD, OP_ICMP_ULT, OP_ICMP_SLT, OP_ICMP_ULE,
                         OP_ICMP_SLE, OP_ICMP_UGT, OP_ICMP_SGT, OP_ICMP_UGE,
                         OP_ICMP_SGE, OP_SLL, OP_SRL, OP_SRA);
    };

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
      : ctx(ctx), build(ctx), cbuild(ctx.getConstants()) {}
};

}; // namespace dyno
