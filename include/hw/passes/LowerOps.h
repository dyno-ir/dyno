#pragma once

#include "dyno/Constant.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/analysis/DelayAnalysis.h"
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

  void runOnInstr(InstrRef instr) {
    switch (*instr.getDialectOpcode()) {
    case *OP_ADD:
      lowerAdd(instr);
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
