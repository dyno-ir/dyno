#pragma once

#include "dyno/AnalysisCache.h"
#include "dyno/Constant.h"
#include "dyno/Instr.h"
#include "dyno/Opcode.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/Wire.h"
#include "op/IDs.h"
#include <cmath>

namespace dyno {

class DelayAnalysis {

  static inline DialectOpcodeInterface<NUM_DIALECTS, Optional<uint8_t>> delay;

  struct Frame {
    HWValue value;
    unsigned idx;
    uint32_t acc;
  };
  SmallVec<Frame, 16> stack;
  Optional<uint32_t> retVal;
  AnalysisCache<ObjRef<Wire>, Optional<uint32_t>> cache;

  void setupDelayTable() {
    // todo constexpr
    delay.set(OP_AND, 1);
    delay.set(OP_OR, 1);
    delay.set(OP_XOR, 2);
    delay.set(OP_NOT, 0);
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

  uint32_t getAddDelay(uint32_t bits, uint32_t operands) {
    if (operands <= 1)
      return 0;
    const unsigned compressInputs = 3;
    const unsigned compressDelay = 2;

    unsigned reductionDelay =
        (unsigned)std::ceil(std::log2(operands / 2) /
                            std::log2(operands / compressInputs)) *
        compressDelay;
    unsigned carryDelay = std::log2(bits);
    return reductionDelay + carryDelay;
  }

  uint32_t getFanoutDelay(uint32_t fanout) {
    if (fanout <= 4)
      return 0;
    return std::log2(fanout);
  }

  uint32_t getDecoderDelay(uint32_t bitsOut) { return log2(bitsOut); }

public:
  DelayAnalysis() { setupDelayTable(); }

  Optional<uint32_t> getDefDelay(OperandRef def) {
    WireRef wire = def->as<WireRef>();
    InstrRef instr = def.instr();
    auto bits = *wire.getNumBits();

    switch (*instr.getDialectOpcode()) {
    case *OP_SUB:
    case *OP_ADD: {
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
      return getAddDelay(*instr.operand(1)->as<HWValue>().getNumBits(), 2);
    }

    case *OP_SLL:
    case *OP_SRL:
    case *OP_SRA: {
      if (instr.other(1)->is<ConstantRef>())
        return 0;
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
      auto val = delay[instr.getDialectOpcode()];
      if (val)
        return *val;
      return nullopt;
    }

    case *HW_LOAD:
      return 0;
    }
  }

  Optional<uint32_t> getDelayOf(HWValue rootVal) {

    stack.emplace_back(rootVal, 0);
    while (!stack.empty()) {
      auto &frame = stack.back();
      if (auto asConst = frame.value.dyn_as<ConstantRef>()) {
        retVal = 0;
        stack.pop_back();
        continue;
      }
      auto wire = frame.value.as<WireRef>();
      auto instr = wire.getDefI();

      if (auto val = cache.find(wire.as<WireRef>())) {
        retVal = *val;
        stack.pop_back();
        continue;
      }

      // if delay is unknown no need to find max operand delay
      if (frame.idx == 0 && !getDefDelay(wire.getDef())) {
        retVal = nullopt;
        stack.pop_back();
        continue;
      }

      // select maximum delay use
      auto maxVal =
          retVal ? std::max(*retVal, frame.acc) : Optional<uint32_t>{nullopt};

      if (frame.idx == 0)
        maxVal = 0;

      if (maxVal && frame.idx != instr.getNumOthers() &&
          instr.other(frame.idx)->is<HWValue>()) {
        frame.acc = *maxVal;
        stack.emplace_back(instr.other(frame.idx++)->as<HWValue>(), 0, 0);
      } else {
        retVal = maxVal;
        if (retVal)
          *retVal += *getDefDelay(wire.getDef());
        cache.insert(stack.back().value.as<WireRef>(), retVal);
        stack.pop_back();
      }
    }

    return retVal;
  }

  void clearCache() { cache.clearAll(); }
};

}; // namespace dyno
