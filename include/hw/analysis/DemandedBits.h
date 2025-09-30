#pragma once

#include "dyno/Constant.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/Wire.h"
#include "hw/analysis/KnownBits.h"
#include "op/IDs.h"

namespace dyno {

template <typename AccT, auto NextFunc> class DownwardAnalysis {
  struct Frame {
    FatDynObjRef<InstrDefUse> ref;
    unsigned idx;
    AccT acc;

    Frame(FatDynObjRef<> ref) : ref(ref), idx(0) {}
    Frame() = default;
  };

public:
  AccT get(FatDynObjRef<InstrDefUse> ref) {
    SmallVec<Frame, 8> stack{Frame{ref}};
    AccT retVal;

    while (!stack.empty()) {
      auto &frame = stack.back();

      if (frame.idx == 0)
        frame.acc.setMostOptimistic();
      else
        frame.acc.merge(frame.ref, retVal);

      if (frame.idx != 0 && frame.acc.isMostPessimistic()) {
        retVal.setMostPessimistic();
        stack.pop_back();
        continue;
      }

      auto next = NextFunc(frame.ref, frame.idx, frame.acc);

      if (next) {
        frame.idx++;
        stack.emplace_back(next);
      } else {
        if (stack.size() != 1) {
          auto &prev = stack[stack.size() - 2];
          stack.back().acc.finalize(prev.ref, prev.idx - 1, frame.ref);
        }
        retVal = std::move(stack.back().acc);
        stack.pop_back();
      }
    }

    return retVal;
  }
};

struct DemandedBitsAnalysis {

  struct DemandedBitsAcc {
    BigInt demanded;

    void merge(FatDynObjRef<> ref, const DemandedBitsAcc &other) {
      if (other.isMostPessimistic()) {
        setMostPessimistic();
        return;
      }
      if (other.isMostOptimistic()) {
        return;
      }

      demanded |= other.demanded;
    }

    void ensureSize(unsigned size) {
      if (isMostPessimistic()) {
        demanded = BigInt::fromI64(-1, size);
        return;
      }
      if (isMostOptimistic()) {
        demanded = BigInt::fromI64(0, size);
        return;
      }
      assert(demanded.getNumBits() == size);
    }

    void finalize(FatDynObjRef<InstrDefUse> use, unsigned useIdx,
                  FatDynObjRef<InstrDefUse> def) {
      auto wire = def.as<WireRef>();
      auto instr = wire.getDefI();
      ensureSize(*wire->numBits);

      switch (*instr.getDialectOpcode()) {
      case *OP_TRUNC:
      case *OP_SEXT:
      case *OP_ZEXT:
      case *OP_ANYEXT:
        demanded.resizeOp(demanded, demanded,
                          *instr.other(0)->as<HWValue>().getNumBits());
        break;

      case *HW_CONCAT: {
        OperandRef op = *use->getUse(useIdx);
        assert(op.instr() == instr);
        uint32_t priorBits = 0;
        for (auto &otherOp : Range{op + 1, *instr.other_end()})
          priorBits += *otherOp.as<HWValue>().getNumBits();
        uint32_t len = *op->as<HWValue>().getNumBits();
        BigInt::rangeSelectOp(demanded, demanded, priorBits, len);
        break;
      }
        // case *OP_AND:
        //   KnownBitsAnalysis knownBits;
        //   knownBits.getKnownBits(HWValue rootVal)
        //   break;
      }
    }

    bool isMostPessimistic() const { return demanded.valueEqualsS(-1); }
    bool isMostOptimistic() const { return demanded.valueEquals(0); }

    void setMostPessimistic() { demanded.set(1u, 1); }
    void setMostOptimistic() { demanded.set(0u, 1); }
  };

  static FatDynObjRef<> demandedBitsNextFunc(FatDynObjRef<InstrDefUse> ref,
                                             unsigned idx, DemandedBitsAcc &acc) {
    auto asWire = ref.as<WireRef>();
    if (idx == asWire.getNumUses())
      return nullref;

    auto operand = asWire.use_begin()[idx];
    auto instr = operand.instr();

    switch (*instr.getDialectOpcode()) {
    case *HW_CONCAT:
    case *OP_TRUNC:
    case *OP_SEXT:
    case *OP_ZEXT:
    case *OP_ANYEXT:
    case *OP_AND:
    case *OP_OR:
    case *OP_XOR:
    case *OP_NOT:
      return instr.def(0)->as<WireRef>();
      break;

    default:
      acc.setMostPessimistic();
      return nullref;
    }
  }
  DownwardAnalysis<DemandedBitsAcc, demandedBitsNextFunc> base;

  BigInt getDemandedBits(WireRef wire) { return base.get(wire).demanded; }
};

}; // namespace dyno
