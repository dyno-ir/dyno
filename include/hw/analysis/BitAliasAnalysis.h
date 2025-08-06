#pragma once

#include "dyno/Constant.h"
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
class BitAliasAnalysis {
  HWContext &ctx;

  class BitAliasAcc : public RegisterValue {
  public:
    BitAliasAcc(ConstantRef c)
        : RegisterValue{c, c.getNumBits(), 0, true, nullopt} {}
    BitAliasAcc(WireRef w)
        : RegisterValue{w, *w.getNumBits(), 0, true, nullopt} {}
    BitAliasAcc() = default;
    BitAliasAcc(const RegisterValue &val) : RegisterValue(val) {}
    BitAliasAcc(RegisterValue &&val) : RegisterValue(std::move(val)) {}

    static BitAliasAcc mostPessimistic(WireRef wire) {
      return BitAliasAcc{
          RegisterValue{wire, *wire.getNumBits(), 0, true, nullopt}};
    }
  };

  struct Frame {
    OperandRef ref;
    BitAliasAcc acc;
  };

  std::optional<OperandRef> nextFunction(BitAliasAcc &&retVal, Frame &frame) {
    auto ref = frame.ref;
    auto instr = frame.ref.instr();
    bool first = frame.ref.isDef();
    if (first) {
      frame.acc = BitAliasAcc{};
      frame.ref = *instr.other_begin();
    }

    switch (*instr.getDialectOpcode()) {
    case *HW_CONCAT: {
      if (first)
        break;
      retVal.appendTop(frame.acc);
      frame.acc = std::move(retVal);
      frame.ref += 1;
      break;
    }

    case *HW_SPLICE: {
      if (first)
        break;
      auto addr = frame.ref[1].as<HWValue>();
      auto len = frame.ref[2].as<ConstantRef>();
      if (!addr.is<ConstantRef>()) {
        frame.acc = BitAliasAcc::mostPessimistic(instr.def(0)->as<WireRef>());
        return std::nullopt;
      }
      retVal = retVal.getRange(addr.as<ConstantRef>().getExactVal(),
                               len.getExactVal());
      retVal.appendTop(frame.acc);
      frame.acc = std::move(retVal);
      frame.ref += 3;
      break;
    }

    case *OP_TRUNC: {
      if (first)
        break;
      auto len = *instr.def(0)->as<WireRef>().getNumBits();
      frame.acc = retVal.getRange(0, len);
      return std::nullopt;
    }

    case *OP_ZEXT: {
      if (first)
        break;
      auto outLen = *instr.def(0)->as<WireRef>().getNumBits();
      auto inLen = *instr.other(0)->as<WireRef>().getNumBits();
      assert(outLen >= inLen);
      retVal.appendTop(ctx.constBuild().zero(outLen - inLen).get(), 0,
                       outLen - inLen);
      frame.acc = std::move(retVal);
      return std::nullopt;
    }

    case *OP_ANYEXT: {
      if (first)
        break;
      auto outLen = *instr.def(0)->as<WireRef>().getNumBits();
      auto inLen = *instr.other(0)->as<WireRef>().getNumBits();
      assert(outLen >= inLen);
      retVal.appendTop(ctx.constBuild().undef(outLen - inLen).get(), 0,
                       outLen - inLen);
      frame.acc = std::move(retVal);
      return std::nullopt;
    }

    case *HW_REPEAT: {
      if (first)
        break;
      auto outLen = *instr.def(0)->as<WireRef>().getNumBits();
      auto inLen = *instr.other(0)->as<WireRef>().getNumBits();
      assert(outLen % inLen == 0);
      auto cnt = outLen / inLen;
      frame.acc = retVal;
      for (uint i = 0; i < cnt - 1; i++)
        frame.acc.appendTop(retVal);
      return std::nullopt;
    }

    case *OP_SEXT: {
      if (first)
        break;
      auto outLen = *instr.def(0)->as<WireRef>().getNumBits();
      auto inLen = *instr.other(0)->as<WireRef>().getNumBits();
      assert(outLen >= inLen);
      frame.acc = std::move(retVal);
      auto signBit = frame.acc.getRange(inLen - 1, 1);
      for (uint i = 0; i < (outLen - inLen); i++)
        frame.acc.appendTop(signBit);
      return std::nullopt;
    }

    case *HW_INSERT: {
      if (first)
        break;
      if (frame.ref == instr.other(0)) {
        frame.acc = std::move(retVal);
        ++frame.ref;
      } else {
        auto addr = frame.ref[1].as<HWValue>();
        auto len = frame.ref[2].as<ConstantRef>();
        if (!addr.is<ConstantRef>()) {
          frame.acc = BitAliasAcc::mostPessimistic(instr.def(0)->as<WireRef>());
          return std::nullopt;
        }
        frame.acc.overwriteNoMaterialize(
            retVal, 0, addr.as<ConstantRef>().getExactVal(), len.getExactVal());
        return std::nullopt;
      }
      break;
    }

    default: {
      frame.acc = ref->as<WireRef>();
      return std::nullopt;
    }
    }
    return frame.ref == instr.end() ? std::nullopt
                                    : std::make_optional(frame.ref);
  }

  SmallVec<Frame, 16> stack;
  BitAliasAcc retVal;

public:
  BitAliasAnalysis(HWContext &ctx) : ctx(ctx) {}
  auto getReprAliases(WireRef wire) {
    stack.emplace_back(wire.getDef());

    uint maxLevel = 0;

    while (!stack.empty()) {
      maxLevel = std::max(maxLevel, stack.size());
      auto &frame = stack.back();

      auto next = nextFunction(std::move(retVal), frame);

      if (next) {
        if ((*next)->is<ConstantRef>()) {
          retVal = (*next)->as<ConstantRef>();
        } else
          stack.emplace_back(
              (*next)->as<FatDynObjRef<InstrDefUse>>()->getDef());
      } else {
        retVal = std::move(stack.back().acc);
        stack.pop_back();
      }
    }
    auto change = retVal.defragmentValues(ctx);
    change |= maxLevel > 2;
    if (retVal.frags.size() == 1) {
      assert(retVal.frags.front().len == *wire.getNumBits());
      change &= retVal.frags.front().ref != wire;
    }
    return std::make_pair(RegisterValue{retVal}, change);
  }
};

}; // namespace dyno
