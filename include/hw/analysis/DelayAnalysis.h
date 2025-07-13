#pragma once

#include "dyno/DialectInfo.h"
#include "dyno/Interface.h"
#include "dyno/Opcode.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/Wire.h"
#include "op/IDs.h"
#include "support/Optional.h"
#include <cmath>

namespace dyno {

// just skeleton, real modeling tbd
class DelayAnalysis {

  static inline DialectOpcodeInterface<NUM_DIALECTS, Optional<uint8_t>> delay;

public:
  void setupDelayTable() {
    // todo constexpr
    delay.set(OP_AND, 1);
    delay.set(OP_OR, 1);
    delay.set(OP_XOR, 2);
    delay.set(OP_XNOR, 2);
    delay.set(OP_SEXT, 0);
    delay.set(OP_ZEXT, 0);
    delay.set(OP_ANYEXT, 0);
    delay.set(OP_TRUNC, 0);
    delay.set(HW_CONCAT, 0);
    delay.set(HW_REPEAT, 0);
    delay.set(HW_SPLICE, 0);
    delay.set(HW_MUX, 2);
    delay.set(HW_ADD_COMPRESS, 2);
  }

  DelayAnalysis() { setupDelayTable(); }

  uint32_t getAddDelay(uint32_t bits, uint32_t operands) {
    if (operands <= 1)
      return 0;
    const uint compressInputs = 3;
    const uint compressDelay = 2;

    uint reductionDelay =
        (uint)std::ceil(std::log2(operands / 2) /
                        std::log2(operands / compressInputs)) *
        compressDelay;
    uint carryDelay = std::log2(bits);
    return reductionDelay + carryDelay;
  }

  uint32_t getFanoutDelay(uint32_t fanout) {
    if (fanout <= 4)
      return 0;
    return std::log2(fanout);
  }

  uint32_t getDecoderDelay(uint32_t bitsOut) { return log2(bitsOut); }

  Optional<uint32_t> getDefDelay(OperandRef def, SmallVecImpl<WireRef> *stack) {
    WireRef wire = def->as<WireRef>();
    InstrRef instr = def.instr();
    auto bits = *wire.getNumBits();

    auto pushUses = [stack](InstrRef instr) {
      if (!stack)
        return;
      for (auto use : instr.others())
        if (auto asWire = use->dyn_as<WireRef>())
          stack->emplace_back(asWire);
    };

    switch (*instr.getDialectOpcode()) {
    case *OP_SUB:
    case *OP_ADD: {
      pushUses(instr);
      return getAddDelay(bits, instr.getNumOthers());
    }

    case *OP_ICMP_EQ:
    case *OP_ICMP_NE:
    case *OP_ICMP_CEQ:
    case *OP_ICMP_CNE:
    case *OP_ICMP_WEQ:
    case *OP_ICMP_WNE:
    case *OP_ICMP_CZEQ:
    case *OP_ICMP_CZNE:
    case *OP_ICMP_CXEQ:
    case *OP_ICMP_CXNE: {
      pushUses(instr);
      return std::log2(*instr.operand(1)->as<HWValue>().getNumBits());
    }

    case *OP_ICMP_ULT:
    case *OP_ICMP_SLT:
    case *OP_ICMP_ULE:
    case *OP_ICMP_SLE:
    case *OP_ICMP_UGT:
    case *OP_ICMP_SGT:
    case *OP_ICMP_UGE:
    case *OP_ICMP_SGE: {
      pushUses(instr);
      return getAddDelay(*instr.operand(1)->as<HWValue>().getNumBits(), 2);
    }

    case *OP_SLL:
    case *OP_SRL:
    case *OP_SRA: {
      pushUses(instr);
      auto bits = *instr.operand(1)->as<HWValue>().getNumBits();
      return getFanoutDelay(bits) + getDecoderDelay(bits) + *delay[HW_MUX];
    }

    case *OP_IF:
    case *OP_SWITCH:
    case *OP_FOR:
    case *OP_WHILE:
    case *OP_DO_WHILE:
      return nullopt;

    default: {
      pushUses(instr);
      auto val = delay[instr.getDialectOpcode()];
      if (val)
        return *val;
      return nullopt;
    }

    case *HW_LOAD:
      return 0;
    }
  }

  Optional<uint32_t> getDelayOf(WireRef wire) {
    SmallVec<WireRef, 32> stack{wire};
    SmallVec<std::pair<uint32_t, uint32_t>, 16> accs{{~0u, 0}};

    while (!stack.empty()) {
      auto curWire = stack.pop_back_val();
      auto oldStackSize = stack.size();

      if (oldStackSize == accs.back().first) {
        auto maxDelay = accs.pop_back_val().second;
        accs.back().second += maxDelay;
      }

      auto val = getDefDelay(curWire.getDef(), &stack);
      if (!val)
        return nullopt;
      accs.back().second =
          std::max(accs.back().second,
                   *val + getFanoutDelay(curWire->defUse.getNumUses()));

      if (oldStackSize != stack.size())
        accs.emplace_back(oldStackSize, 0);
    }

    return accs.front().second;
  }
};

}; // namespace dyno
