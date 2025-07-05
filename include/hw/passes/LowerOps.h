#pragma once

#include "dyno/Constant.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/HWValue.h"
#include "hw/analysis/DelayAnalysis.h"
#include "support/Utility.h"
#include <cstdint>
namespace dyno {

class LowerOpsPass {
  HWContext &ctx;
  HWInstrBuilder build;
  DelayAnalysis delay;

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
    }
  }

public:
  void run() {
    for (auto instr : ctx.getInstrs())
      runOnInstr(instr);
  }
  explicit LowerOpsPass(HWContext &ctx) : ctx(ctx), build(ctx) {}
};

}; // namespace dyno
