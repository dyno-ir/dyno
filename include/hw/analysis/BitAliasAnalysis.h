#pragma once

#include "dyno/AnalysisCache.h"
#include "dyno/Constant.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "dyno/Opcode.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Wire.h"
#include "hw/analysis/RegisterValue.h"
#include "op/IDs.h"
#include "support/Debug.h"
#include <optional>
namespace dyno {
class BitAliasAnalysis {
  HWContext &ctx;
  bool change;

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
    bool nested = stack.size() >= 3;

    switch (*instr.getDialectOpcode()) {
    case *HW_CONCAT: {
      if (first)
        break;
      retVal.appendTop(frame.acc);
      frame.acc = std::move(retVal);
      frame.ref += 1;
      change |= nested;
      break;
    }

    case *HW_SPLICE: {
      auto asSplice = instr.as<SpliceIRef>();
      if (asSplice.getNumTerms() != 0 && !asSplice.terms().all([](auto term) {
            return term.getIdx().template is<ConstantRef>();
          })) {

        frame.acc = BitAliasAcc::mostPessimistic(instr.def(0)->as<WireRef>());
        return std::nullopt;
      }

      if (first)
        break;
      auto addr = asSplice.getBase();
      auto len = *asSplice.out()->as<WireRef>().getNumBits();
      assert(retVal.frags.empty() || retVal.frags.front().dstAddr == 0);

      // support unsimplified constant addresses here to avoid explosion
      // in instcombine for long insert/splice sequences. (todo: better fix?)
      asSplice.terms().for_each([&](auto term) {
        addr += term.getIdx().template as<ConstantRef>().getExactVal();
      });

      retVal = retVal.getRange(addr, len);
      retVal.appendTop(frame.acc);
      frame.acc = std::move(retVal);
      change |= nested;
      return std::nullopt;
      break;
    }

    case *OP_TRUNC: {
      if (first)
        break;
      auto len = *instr.def(0)->as<WireRef>().getNumBits();
      frame.acc = retVal.getRange(0, len);
      change |= nested;
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
      change |= nested;
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
      change |= nested;
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
      if (cnt == 0) {
        frame.acc = ConstantRef::zeroBitZero();
        change = true;
        return std::nullopt;
      }
      for (uint i = 0; i < cnt - 1; i++)
        frame.acc.appendTop(retVal);
      change |= nested;
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
      change |= nested;
      return std::nullopt;
    }

    case *HW_INSERT: {
      InsertIRef asInsert = instr.as<InsertIRef>();
      if (first) {
        if (asInsert.getNumTerms() != 0 && !asInsert.terms().all([](auto term) {
              return term.getIdx().template is<ConstantRef>();
            })) {
          frame.acc = BitAliasAcc::mostPessimistic(instr.def(0)->as<WireRef>());
          return std::nullopt;
        }
        break;
      }
      if (frame.ref == instr.other(0)) {
        frame.acc = std::move(retVal);
        ++frame.ref;
      } else {
        auto addr = asInsert.getBase();
        auto len = asInsert.getLen();

        // support unsimplified constant addresses here to avoid explosion
        // in instcombine for long insert/splice sequences. (todo: better fix?)
        asInsert.terms().for_each([&](auto term) {
          addr += term.getIdx().template as<ConstantRef>().getExactVal();
        });

        frame.acc.overwriteNoMaterialize(retVal, 0, addr, len);
        // constant inserts can be lowered to splice, so also mark change if
        // first entry.
        change |= stack.size() == 1;
        change |= nested;
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
  AnalysisCache<ObjRef<Wire>, BitAliasAcc> cache;

public:
  BitAliasAnalysis(HWContext &ctx) : ctx(ctx) {}
  auto getReprAliases(WireRef rootWire) {
    stack.emplace_back(rootWire.getDef());

    uint maxLevel = 0;
    retVal = BitAliasAcc{};
    change = false;

    while (!stack.empty()) {
      maxLevel = std::max(maxLevel, stack.size());
      auto &frame = stack.back();

      if (auto asWire = frame.ref.instr().def(0)->dyn_as<WireRef>()) {
        if (auto val = cache.find(asWire)) {
          retVal = *val;
          stack.pop_back();
          continue;
        }
      }

      auto next = nextFunction(std::move(retVal), frame);

      if (next) {
        if ((*next)->is<ConstantRef>()) {
          retVal = (*next)->as<ConstantRef>();
        } else
          stack.emplace_back(
              (*next)->as<FatDynObjRef<InstrDefUse>>()->getDef());
      } else {
        retVal = std::move(stack.back().acc);
        assert(frame.ref.instr().getNumDefs() == 1);
        if (auto asWire = frame.ref.instr().def(0)->dyn_as<WireRef>()) {
          cache.insert(asWire, retVal);
        }
        stack.pop_back();
      }
    }
    change |= retVal.defragmentValues(ctx);
    if (retVal.frags.size() == 1) {
      assert(retVal.frags.front().len == *rootWire.getNumBits());
      change &= retVal.frags.front().ref != rootWire;
    }
    return std::make_pair(RegisterValue{retVal}, change);
  }

  void clearCache() { cache.clearAll(); }
};

}; // namespace dyno
