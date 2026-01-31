#pragma once

#include "dyno/Constant.h"
#include "dyno/Context.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "dyno/Opcode.h"
#include "hw/HWContext.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/Wire.h"
#include "hw/analysis/RegisterValue.h"
#include "op/IDs.h"

namespace dyno {
class LinearExpressionAnalysis {
  Context &ctx [[maybe_unused]];

  class LinearExpr {
  public:
    BigInt base;
    SmallDenseMap<ObjRef<Wire>, BigInt> terms;

    void setEqualWire(WireRef wire) {
      base.set(0u, 0);
      terms = SmallDenseMap<ObjRef<Wire>, BigInt>{};
      terms.insert(wire, BigInt::fromU64(1, *wire.getNumBits()));
    }
    void setEqualConst(ConstantRef constant) {
      base = constant;
      terms = SmallDenseMap<ObjRef<Wire>, BigInt>{};
    }

    // switches to fallback wire if expr becomes quadratic or above
    void multiply(const LinearExpr &other, WireRef fallback) {
      if (other.terms.empty() && !other.base.getIs4S()) {
        base *= other.base;
        for (auto [wire, fact] : terms) {
          fact *= other.base;
        }
        return;
      }
      setEqualWire(fallback);
    }
    void shiftLeft(const LinearExpr &other, WireRef fallback) {
      if (other.terms.empty() && !other.base.getIs4S() &&
          other.base.getLimitedVal()) {
        base *= other.base;
        for (auto [wire, fact] : terms) {
          BigInt::shlOp(fact, fact, other.base.getExactVal());
        }
        return;
      }
      setEqualWire(fallback);
    }

    void add(const LinearExpr &other) {
      base += other.base;
      for (auto [obj, fact] : other.terms) {
        auto [found, it] =
            terms.findOrInsert(obj, [&] { return std::move(fact); });
        if (found)
          it.val() += fact;
      }
    }

    void resize(unsigned newLen, bool sign = false) {
      BigInt::resizeOp4S(base, base, newLen, sign);
      for (auto [wire, fact] : terms)
        BigInt::resizeOp4S(fact, fact, newLen, sign);
    }
  };

  struct Frame {
    OperandRef ref;
    LinearExpr acc;
  };

  std::optional<OperandRef> nextFunction(LinearExpr &&retVal, Frame &frame) {
    auto ref = frame.ref;
    auto instr = frame.ref.instr();
    bool first = frame.ref.isDef();
    if (first) {
      frame.acc = LinearExpr{};
      frame.ref = *instr.other_begin();
    }

    switch (*instr.getDialectOpcode()) {
    case *OP_MUL: {
      if (first)
        break;
      if (frame.ref == *instr.other_begin())
        frame.acc = std::move(retVal);
      else
        frame.acc.multiply(retVal, instr.def(0)->as<WireRef>());
      frame.ref++;
      break;
    }
    case *OP_SLL: {
      if (first)
        break;
      if (frame.ref == *instr.other_begin())
        frame.acc = std::move(retVal);
      else
        frame.acc.shiftLeft(retVal, instr.def(0)->as<WireRef>());
      frame.ref++;
      break;
    }
    case *OP_ADD: {
      if (first)
        break;
      if (frame.ref == *instr.other_begin()) {
        frame.acc = std::move(retVal);
      } else
        frame.acc.add(retVal);
      frame.ref++;
      break;
    }
    case *OP_TRUNC:
    case *OP_ZEXT:
    case *OP_SEXT: {
      if (first)
        break;
      auto outLen = *instr.def(0)->as<WireRef>().getNumBits();
      frame.acc = std::move(retVal);
      frame.acc.resize(outLen);
      return std::nullopt;
    }

    default: {
      frame.acc.setEqualWire(ref->as<WireRef>());
      return std::nullopt;
    }
    }
    return frame.ref == instr.end() ? std::nullopt
                                    : std::make_optional(frame.ref);
  }

  SmallVec<Frame, 16> stack;
  LinearExpr retVal;

public:
  LinearExpressionAnalysis(Context &ctx) : ctx(ctx) {}
  auto getLinearExpression(WireRef wire) {
    stack.emplace_back(wire.getDef());

    unsigned maxLevel = 0;

    while (!stack.empty()) {
      maxLevel = std::max(maxLevel, stack.size());
      auto &frame = stack.back();

      auto next = nextFunction(std::move(retVal), frame);

      if (next) {
        if ((*next)->is<ConstantRef>()) {
          retVal.setEqualConst((*next)->as<ConstantRef>());
        } else
          stack.emplace_back(
              (*next)->as<FatDynObjRef<InstrDefUse>>()->getDef());
      } else {
        retVal = std::move(stack.back().acc);
        stack.pop_back();
      }
    }
    return retVal;
  }
};
}; // namespace dyno
