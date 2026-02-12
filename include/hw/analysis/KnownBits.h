#pragma once

#include "dyno/AnalysisCache.h"
#include "dyno/Constant.h"
#include "dyno/Context.h"
#include "dyno/Instr.h"
#include "dyno/Opcode.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Wire.h"
#include "hw/analysis/CacheInvalidation.h"
#include "op/IDs.h"
#include "support/Bits.h"
#include "support/Debug.h"
#include "support/DenseMap.h"
#include "support/Utility.h"
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

    KnownBitsVal(BigInt &&val) : val(std::move(val)) {}
    KnownBitsVal(const BigInt &val) : val(val) {}
    KnownBitsVal() = default;
  };

  struct Frame {
    HWValue value;
    unsigned idx;
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
      retVal = std::move(stack.back().acc);
      assert(retVal.val.getNumBits() ==
             stack.back().value.as<WireRef>().getNumBits());
      cache.insert(stack.back().value.as<WireRef>(), retVal.val);
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
  AnalysisCache<ObjRef<Wire>, BigInt> cache;

  BigInt getKnownBits(HWValue rootVal) {
    retVal.val = BigInt{};
    stack.emplace_back(rootVal, 0);
    while (!stack.empty()) {

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
          cache.insert(wire, retVal.val);
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
          cache.insert(wire, retVal.val);
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
          unsigned delta = *wire.getNumBits() - retVal.val.getNumBits();
          PatBigInt lhs = instr.isOpc(OP_ZEXT)
                              ? PatBigInt::fromFourState(FourState::S0, delta)
                              : PatBigInt::fromSign(retVal.val, delta);
          BigInt::concatOp4S(retVal.val, lhs, retVal.val);
          assert(retVal.val.getNumBits() == wire.getNumBits());
          cache.insert(wire, retVal.val);
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
          cache.insert(wire, retVal.val);
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

          if (asInsert.getBase() < asInsert.getMemoryLen()) {
            int64_t oobLen = int64_t(asInsert.getBase() + asInsert.getLen()) -
                             asInsert.getMemoryLen();
            BigInt::resizeOp4S(retVal.val, retVal.val,
                               asInsert.getLen() - oobLen);
            BigInt::insertOp4S(frame.acc.val, frame.acc.val, retVal.val,
                               asInsert.getBase());
          }

          retVal = std::move(frame.acc);
          cache.insert(wire, retVal.val);
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
          cache.insert(wire, retVal.val);
          stack.pop_back();
        }
        break;
      }

      case *HW_ONEHOT_MUX: {
        if (frame.idx != 0) {
          if (frame.idx & 1) {
            // retVal = retVal;
            cache.insert(wire, retVal.val);
            stack.pop_back();
            break;
          } else if (retVal.val.valueEquals(1)) {
            assert(retVal.val.getNumBits() == 1);
            frame.idx--;
            stack.emplace_back(instr.other(frame.idx)->as<HWValue>());
            break;
          }
        }

        if (frame.idx == instr.getNumOthers()) {
          retVal.val =
              PatBigInt::undef(*instr.def(0)->as<WireRef>().getNumBits());
          cache.insert(wire, retVal.val);
          stack.pop_back();
          break;
        }

        // skip values if not selected
        stack.emplace_back(instr.other(frame.idx)->as<HWValue>());
        frame.idx += 2;
        break;
      }

      default:
        retVal = KnownBitsVal{PatBigInt::undef(*wire.getNumBits())};
        cache.insert(wire, retVal.val);
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
      cache.insert(wire, val);
    auto rv = getKnownBits(val);
    cache.clearAll();
    return rv;
  }
};

class DeriveBitsAnalysis {
  Context &ctx;
  SmallVec<std::pair<WireRef, BigInt *>, 64> stack;
  struct Change {
    BigInt bigInt;
    ObjRef<Wire> wire;
  };

public:
  KnownBitsAnalysis &knownBits;
  // returns false if the assignment is contradictory
  bool propKnownValueUp(WireRef wire, BigInt &&value) {
    auto &ref = knownBits.cache.insertOrAssign(wire, std::move(value));
    stack.emplace_back(wire, &ref);

    while (!stack.empty()) {
      auto [obj, bigInt] = stack.pop_back_val();
      auto curWire = ctx.resolve(obj);
      auto instr = curWire.getDefI();

      switch (*instr.getDialectOpcode()) {
      case *OP_OR:
      case *OP_AND: {
        auto propState =
            instr.getDialectOpcode().is(OP_AND) ? FourState::S1 : FourState::S0;
        BigInt oneBits;
        BigInt::bitsExactEqual4S(
            oneBits, *bigInt,
            PatBigInt::fromFourState(propState, bigInt->getNumBits()));
        // nothing to do
        if (oneBits.valueEquals(0))
          break;
        for (auto op : instr.others()) {
          if (auto asConst = op->dyn_as<ConstantRef>()) {
            // contradiction
            if ((asConst & oneBits) != oneBits)
              return false;
            continue;
          }
          auto asWire = op->as<WireRef>();
          auto &val = knownBits.cache.findOrInsert(
              asWire, PatBigInt::undef(*asWire.getNumBits()));

          auto oldV = val;

          // check for contradiction
          BigInt zeroBits;
          BigInt::bitsExactEqual4S(
              zeroBits, val,
              PatBigInt::fromFourState(!propState, val.getNumBits()));
          if (!(zeroBits & oneBits).valueEquals(0))
            return false;

          if (instr.getDialectOpcode().is(OP_AND))
            // 1 bits in output are one in all operands
            val |= oneBits;
          else
            val &= ~oneBits;

          assert(val.getNumBits() == wire.getNumBits());

          if (val != oldV) {
            stack.emplace_back(asWire, &val);
            // todo: recompute uses, but without pessimizing other results...
          }
        }
        break;
      }
      case *OP_NOT: {
        if (auto asConst = instr.other(0)->dyn_as<ConstantRef>()) {
          BigInt rev;
          BigInt::notOp4S(rev, *bigInt);
          if (!BigInt::icmpOp4S(asConst, rev, BigInt::ICMP_CXEQ))
            return false;
        } else if (auto asWire = instr.other(0)->dyn_as<WireRef>()) {
          auto &val = knownBits.cache.findOrInsert(
              asWire, PatBigInt::undef(*asWire.getNumBits()));

          static constexpr auto lambda = [](uint32_t lhs, uint32_t rhs) {
            auto undef = rhs & BigInt::REP10;
            auto mask = undef | (undef >> 1);
            // keep what's undefined in rhs
            lhs &= mask;
            mask = ~mask;

            // invert defined bits in rhs
            auto defLow = mask & ~((mask & BigInt::REP01) << 1);
            rhs ^= defLow;

            // assign masked
            lhs |= (rhs & mask);

            return lhs;
          };

          BigInt newV;
          BigInt::bitwiseOp4S<lambda,
                              BigInt::bitwise2S4SAdapt<lambda, BigInt, BigInt>>(
              newV, val, *bigInt);
          if (!BigInt::icmpOp4S(newV, val, BigInt::ICMP_CXEQ))
            return false;

          if (val != newV) {
            val = std::move(newV);
            stack.emplace_back(asWire, &val);
          }
        } else
          dyno_unreachable("invalid operand type");
        break;
      }
      }
    }

    return true;
  }

public:
  explicit DeriveBitsAnalysis(Context &ctx, KnownBitsAnalysis &knownBits)
      : ctx(ctx), knownBits(knownBits) {}
};

}; // namespace dyno
