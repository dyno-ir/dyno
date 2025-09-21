#pragma once

#include "dyno/AnalysisCache.h"
#include "dyno/Constant.h"
#include "dyno/Instr.h"
#include "dyno/Opcode.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Wire.h"
#include "hw/analysis/CacheInvalidation.h"
#include "op/IDs.h"
#include "support/Debug.h"
#include "support/DenseMap.h"
#include <concepts>
namespace dyno {

class KnownBitsAnalysis : public CacheInvalidation<KnownBitsAnalysis> {

  struct KnownBitsVal {
    BigInt val;
    // BigInt unknown;
    // void prop() {
    //   if (!val.getIs4S()) {
    //     unknown = PatBigInt::fromFourState(FourState::S0, val.getNumBits());
    //     return;
    //   }
    //   BigInt out = PatBigInt::fromFourState(FourState::SX, val.getNumBits());
    //   BigInt::bitsExactEqual4S(out, out, val);
    //   unknown &= out;
    // }

    void
    merge(const KnownBitsVal &other,
          std::invocable<BigInt &, const BigInt &, const BigInt &> auto func) {
      func(val, val, other.val);
    }
    void merge(const KnownBitsVal &other,
               std::invocable<const BigInt &, const BigInt &> auto func) {
      val = func(val, other.val);
    }

    bool operator==(const KnownBitsVal &other) const {
      return val == other.val;
    }
    bool operator==(const BigInt &other) const { return val == other; }
  };

  struct Frame {
    HWValue value;
    uint idx;
    KnownBitsVal acc;
  };
  SmallVec<Frame, 16> stack;
  KnownBitsVal retVal;

  void pushNextOrReturn(Frame &frame, InstrRef instr, bool inv = false) {
    auto n = instr.getNumOthers();
    if (frame.idx != instr.getNumOthers()) {
      auto idx = inv ? (n - frame.idx - 1) : frame.idx;
      frame.idx++;
      stack.emplace_back(instr.other(idx)->as<HWValue>(), 0);
    } else {
      retVal = stack.back().acc;
      assert(retVal.val.getNumBits() ==
             stack.back().value.as<WireRef>().getNumBits());
      cache.insert(stack.back().value.as<WireRef>(), retVal);
      stack.pop_back();
    }
  }

  template <typename T>
  void getArithKnownBits(Frame &frame, InstrRef instr, T func) {
    if (frame.idx == 0)
      ;
    else if (frame.idx == 1)
      frame.acc = retVal;
    else
      frame.acc.merge(retVal, func);
    pushNextOrReturn(frame, instr);
  }

  void getICMPKnownBits(Frame &frame, InstrRef instr, BigInt::ICmpPred pred) {
    if (frame.idx == 0)
      ;
    else if (frame.idx == 1)
      frame.acc = retVal;
    else {
      frame.acc.val = ConstantRef::fromFourState(
          BigInt::icmpOp4S(frame.acc.val, retVal.val, pred));
    }
    pushNextOrReturn(frame, instr);
  }

public:
  AnalysisCache<ObjRef<Wire>, KnownBitsVal> cache;

  BigInt getKnownBits(HWValue rootVal) {
    retVal.val = BigInt{};
    stack.emplace_back(rootVal, 0);
    while (!stack.empty()) {

      if (retVal.val.getWords().data() != nullptr && retVal.val.getNumBits()) {
        auto tst = retVal;
        tst.val.normalize();
        assert(tst.val == retVal.val);
      }

      auto &frame = stack.back();
      if (auto asConst = frame.value.dyn_as<ConstantRef>()) {
        retVal = KnownBitsVal{asConst};
        stack.pop_back();
        continue;
      }
      auto wire = frame.value.as<WireRef>();
      auto instr = wire.getDefI();

      if (auto val = cache.find(wire.as<WireRef>())) {
        retVal = KnownBitsVal{*val};
        stack.pop_back();
        continue;
      }

      switch (*instr.getDialectOpcode()) {

#define LAMBDA(opc, buildF, cbuildF, bigIntF)                                  \
  case *opc:                                                                   \
    getArithKnownBits(frame, instr, bigIntF<BigInt, BigInt>);                  \
    break;
        FOR_HW_SIMPLE_OPS(LAMBDA)
#undef LAMBDA
#define LAMBDA(opc, pred)                                                      \
  case *opc:                                                                   \
    getICMPKnownBits(frame, instr, pred);                                      \
    break;
        FOR_OP_ALL_COMPARE_OPS(LAMBDA)
#undef LAMBDA

      case *HW_CONCAT: {
        if (frame.idx == 0)
          ;
        else if (frame.idx == 1)
          frame.acc = retVal;
        else {
          BigInt::concatOp4S(frame.acc.val, retVal.val, frame.acc.val);
        }
        pushNextOrReturn(frame, instr, true);
        break;
      }

      case *OP_NOT: {
        if (frame.idx == 0) {
          frame.idx++;
          stack.emplace_back(instr.other(0)->as<HWValue>(), 0);
        } else {
          BigInt::notOp4S(retVal.val, retVal.val);
          assert(retVal.val.getNumBits() == wire.getNumBits());
          cache.insert(wire, retVal);
          stack.pop_back();
        }
        break;
      }

      case *OP_TRUNC: {
        if (frame.idx == 0) {
          frame.idx++;
          stack.emplace_back(instr.other(0)->as<HWValue>(), 0);
        } else {
          BigInt::resizeOp4S(retVal.val, retVal.val, *wire.getNumBits());
          assert(retVal.val.getNumBits() == wire.getNumBits());
          cache.insert(wire, retVal);
          stack.pop_back();
        }
        break;
      }
      case *OP_SEXT:
      case *OP_ZEXT: {
        if (frame.idx == 0) {
          frame.idx++;
          stack.emplace_back(instr.other(0)->as<HWValue>(), 0);
        } else {
          assert(*wire.getNumBits() >= retVal.val.getNumBits());
          uint delta = *wire.getNumBits() - retVal.val.getNumBits();
          PatBigInt lhs = instr.isOpc(OP_ZEXT)
                              ? PatBigInt::fromFourState(FourState::S0, delta)
                              : PatBigInt::fromSign(retVal.val, delta);
          BigInt::concatOp4S(retVal.val, lhs, retVal.val);
          assert(retVal.val.getNumBits() == wire.getNumBits());
          cache.insert(wire, retVal);
          stack.pop_back();
        }
        break;
      }

      case *HW_SPLICE: {
        auto asSplice = instr.as<SpliceIRef>();
        if (frame.idx == 0) {
          if (!asSplice.isConstantOffs()) {
            retVal = KnownBitsVal{PatBigInt::undef(*wire.getNumBits())};
            stack.pop_back();
            break;
          }
          frame.idx++;
          stack.emplace_back(instr.other(0)->as<HWValue>(), 0);
        } else {
          auto len = asSplice.getLen();
          int64_t oobLen = int64_t(asSplice.getLen() + asSplice.getBase()) -
                           retVal.val.getNumBits();
          if (asSplice.getBase() >= retVal.val.getNumBits()) {
            retVal.val = PatBigInt::undef(len);
          } else if (oobLen > 0) {
            BigInt::rangeSelectOp4S(retVal.val, retVal.val, asSplice.getBase(),
                                    len - oobLen);
            BigInt::concatOp4S(retVal.val, PatBigInt::undef(oobLen),
                               retVal.val);
          } else {
            BigInt::rangeSelectOp4S(retVal.val, retVal.val, asSplice.getBase(),
                                    len);
          }
          assert(retVal.val.getNumBits() == wire.getNumBits());
          cache.insert(wire, retVal);
          stack.pop_back();
        }
        break;
      }

      case *HW_INSERT: {
        auto asInsert = instr.as<InsertIRef>();
        if (frame.idx == 0) {
          if (!asInsert.isConstantOffs()) {
            retVal = KnownBitsVal{PatBigInt::undef(*wire.getNumBits())};
            stack.pop_back();
            break;
          }
          frame.idx++;
          stack.emplace_back(asInsert.in()->as<HWValue>(), 0);
        } else if (frame.idx == 1) {
          frame.acc = std::move(retVal);
          frame.idx++;
          stack.emplace_back(asInsert.val()->as<HWValue>(), 0);
        } else {
          BigInt::insertOp4S(frame.acc.val, frame.acc.val, retVal.val,
                             asInsert.getBase());
          retVal = std::move(frame.acc);
          cache.insert(wire, retVal);
          stack.pop_back();
        }
        break;
      }

      case *HW_MUX: {
        if (frame.idx == 0) {
          frame.idx++;
          stack.emplace_back(instr.other(0)->as<HWValue>(), 0);
        } else if (frame.idx == 1) {
          frame.idx++;
          assert(retVal.val.getNumBits() == 1);
          if (retVal.val.getIs4S()) {
            retVal = KnownBitsVal{PatBigInt::undef(*wire.getNumBits())};
            stack.pop_back();
          } else {
            stack.emplace_back(
                instr.other(1 + retVal.val.valueEquals(0))->as<HWValue>());
          }
        } else {
          // retVal = retVal;
          cache.insert(wire, retVal);
          stack.pop_back();
        }
        break;
      }

      default:
        retVal = KnownBitsVal{PatBigInt::undef(*wire.getNumBits())};
        stack.pop_back();
        break;
      }
    }

    return retVal.val;
  }

  auto get(HWValue root) { return getKnownBits(root); }

  auto getKnownBitsWith(HWValue val,
                        ArrayRef<std::pair<ObjRef<Wire>, BigInt>> assignments) {
    cache.clearAll();
    for (auto [wire, val] : assignments)
      cache.insert(wire, KnownBitsVal{val});
    auto rv = getKnownBits(val);
    cache.clearAll();
    return rv;
  }
};

}; // namespace dyno
