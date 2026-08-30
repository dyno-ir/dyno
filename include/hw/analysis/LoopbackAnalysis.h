#pragma once

#include "dyno/Constant.h"
#include "dyno/Context.h"
#include "dyno/IDImpl.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Wire.h"
#include "hw/analysis/RegisterValue.h"
#include "op/IDs.h"
#include "support/DenseMap.h"
#include "support/DynBitSet.h"
#include "support/RTTI.h"
#include "support/Ranges.h"
#include "support/SmallVec.h"
#include <limits>
namespace dyno {

class LoopbackFrag {
  constexpr static ObjRef<Wire> invalid{
      ObjID{std::numeric_limits<ObjID::num_t>::max() - 1}};

public:
  uint32_t dstAddr;
  uint32_t len;
  SmallVec<ObjRef<Wire>, 4> enable{invalid};
  DynBitSet<SmallVec<uint64_t, 1>> polarity;

  LoopbackFrag() = default;
  LoopbackFrag(uint32_t dstAddr, uint32_t len) : dstAddr(dstAddr), len(len) {
    this->polarity.push_back(1);
  }
  LoopbackFrag(uint32_t dstAddr, uint32_t len, ObjRef<Wire> en, bool polarity)
      : dstAddr(dstAddr), len(len) {

    this->enable[0] = en;
    this->polarity.push_back(polarity);
    if (!en) {
      enable.pop_back();
      this->polarity.pop_back();
    }
  }
  LoopbackFrag(uint32_t dstAddr, uint32_t len, const LoopbackFrag &frag)
      : dstAddr(dstAddr), len(len), enable(frag.enable) {
    this->polarity.push_back(1);
  }

  void push_back(ObjRef<Wire> en, bool polarity) {
    if (!*this)
      return; // nothing to do
    enable.push_back(en);
    this->polarity.push_back(polarity);
  }
  uint32_t size() const { return bool(*this) ? enable.size() : 0; }
  auto begin() { return zip_iterator{enable.begin(), polarity.begin()}; }
  auto end() { return zip_iterator{enable.end(), polarity.end()}; }

  explicit operator bool() const {
    return enable.size() != 1 || enable[0] != invalid;
  }

  bool overwrites(LoopbackFrag &other) { return false; }
  bool fuses(LoopbackFrag &other) { return false; }
  bool intersects(LoopbackFrag &other) { return true; }

  LoopbackFrag intersect(LoopbackFrag &o) {
    if (*this)
      return *this;
    return o;
  }
  bool abstractEquals(LoopbackFrag &) const { return false; }
};

class LoopbackPartition : public GenericPartitions<LoopbackFrag> {
public:
  using GenericPartitions::GenericPartitions;
  LoopbackPartition(GenericPartitions &&o) : GenericPartitions(std::move(o)) {}
  LoopbackPartition(const GenericPartitions &o) : GenericPartitions(o) {}

  bool addConditionToLoopbackFrags(HWValue val, bool polarity) {
    if (val.is<ConstantRef>() && val.as<ConstantRef>().valueEquals(0))
      return false;
    if (val.is<WireRef>())
      for (auto &frag : frags) {
        if (!frag)
          continue;
        frag.push_back(val.as<WireRef>(), polarity);
      }
    return true;
  }

  bool hasAnyLoopback() {
    return Range{frags}.any([](auto frag) { return !!frag; });
  }
};

class LoopbackAnalysis {
  Context &ctx;
  struct Frame {
    HWValue value;
    uint32_t idx = 0;
    LoopbackPartition acc = {};
  };
  LoopbackPartition retVal;
  SmallVec<Frame, 64> stack;
  uint32_t addr = 0;

#define FRAME_RET(...)                                                         \
  {                                                                            \
    retVal = {*val.getNumBits(), __VA_ARGS__};                                 \
    stack.pop_back();                                                          \
    continue;                                                                  \
  }

#define FRAME_CALL(...)                                                        \
  {                                                                            \
    stack.emplace_back(__VA_ARGS__);                                           \
    continue;                                                                  \
  }

#define FRAME_RET_ACC                                                          \
  {                                                                            \
    retVal = std::move(acc);                                                   \
    stack.pop_back();                                                          \
    break;                                                                     \
  }

public:
  static constexpr const char *debugName = "LoopbackPartitionAnalysis";
  static constexpr uint32_t debugID = 130; // todo: analysis debug ID assignment

  LoopbackPartition get(HWValue value, HWValue loopback) {
    stack.emplace_back(value, 0);
    SmallDenseMap<ObjRef<Wire>, LoopbackPartition> map;
    retVal = {};

    while (!stack.empty()) {
      auto &[val, idx, acc] = stack.back();
      addr = 0;

      if (auto asConst = val.dyn_as<ConstantRef>()) {
        // use loopback=nullref as wildcard for any constant (used for finding
        // reset values).
        if (loopback == nullref && !asConst.getIs4S())
          // return always-on loopback
          FRAME_RET(nullref, true);

        if (auto loopbackConst = loopback.dyn_as<ConstantRef>()) {
          BigInt out;
          BigInt::rangeSelectOp4S(out, loopbackConst, addr,
                                  asConst.getNumBits());

          if (out == asConst) // return always-on loopback
            FRAME_RET(nullref, true);
        }
        FRAME_RET();
      }

      auto wire = val.as<WireRef>();
      auto instr = wire.getDefI();

      DYNO_DBG({
        // print last return value first.
        auto dumpVal = [](LoopbackPartition &val) {
          for (auto f : val.frags) {
            std::print(dbgs(), "  [{}+:{}]: ", f.dstAddr, f.len);
            for (auto [en, p] : Range{f.enable}.zip(f.polarity)) {
              std::print(dbgs(), "{}w{}, ", p ? "!" : "0", en.getObjID().num);
            }
            std::print(dbgs(), "\n");
          }
        };
        dumpVal(retVal);

        dumpInstr(instr, ctx, true, false);
        std::print(dbgs(), "acc:\n");
        dumpVal(stack.back().acc);
        std::print(dbgs(), "rv:\n");
      });

      if (auto it = map.find(wire); it != map.end()) {
        retVal = it.val();
        stack.pop_back();
        continue;
      }

      switch (*instr.getDialectOpcode()) {
      case *HW_SPLICE: {
        auto asSplice = instr.as<SpliceIRef>();
        if (idx == 0) {
          if (!asSplice.isConstantOffs())
            FRAME_RET();
          idx++;
          addr += asSplice.getBase();
          FRAME_CALL(asSplice.in()->as<HWValue>());
        }
        addr -= asSplice.getBase();
        retVal = retVal.getRange(asSplice.getBase(), asSplice.getLen());
        stack.pop_back();
        break;
      }
      case *OP_TRUNC: {
        if (idx == 0) {
          idx++;
          FRAME_CALL(instr.other(0)->as<HWValue>());
        }

        retVal = retVal.getRange(0, *instr.def()->as<HWValue>().getNumBits());
        stack.pop_back();
        break;
      }

      case *OP_ANYEXT:
      case *OP_SEXT:
      case *OP_ZEXT: {
        if (idx == 0) {
          idx++;
          FRAME_CALL(instr.other(0)->as<HWValue>());
        }

        auto patBits = *instr.def()->as<WireRef>().getNumBits() -
                       *instr.other(0)->as<HWValue>().getNumBits();

        retVal.append(patBits);
        stack.pop_back();
        break;
      }

      case *HW_CONCAT: {
        if (idx != 0) {
          acc.append(retVal);
        }

        if (idx == instr.getNumOthers()) {
          retVal = std::move(acc);
          stack.pop_back();
          addr -= *instr.def()->as<WireRef>().getNumBits();
          break;
        }
        auto nextVal =
            instr.other(instr.getNumOthers() - idx - 1)->as<HWValue>();
        ++idx;
        addr += *nextVal.getNumBits();
        FRAME_CALL(nextVal)
      }

      case *HW_INSERT: {
        auto asInsert = instr.as<InsertIRef>();
        if (idx == 0) {
          if (!asInsert.isConstantOffs())
            FRAME_RET();
          idx++;
          FRAME_CALL(asInsert.val()->as<HWValue>());
        }
        if (idx == 1) {
          ++idx;
          acc = std::move(retVal);
          FRAME_CALL(asInsert.in()->as<HWValue>());
        }

        assert(acc.getLen() == asInsert.val()->as<HWValue>().getNumBits());
        retVal.write(acc, 0, asInsert.getBase(), acc.getLen());
        stack.pop_back();
        break;
      }

      case *HW_ONEHOT_MUX: {
        if (idx == 0) // reset acc unconditionally
          acc = {*val.getNumBits()};
        if (idx != 0 && retVal.hasAnyLoopback()) {
          // add condition to loopback frags
          auto cond = instr.other((idx - 1) * 2)->as<HWValue>();
          if (auto asConst = cond.dyn_as<ConstantRef>()) {

            // give up if undef select
            if (asConst.allBitsUndef())
              FRAME_RET();

            // select is zero i.e. unreachable
            if (asConst.valueEquals(0)) {
              // return if last
              if (idx == instr.getNumOthers() / 2) {
                retVal = std::move(acc);
                stack.pop_back();
                break;
              }
              // else skip, move on to next
              ++idx;
              continue;
            }
          }
          retVal.addConditionToLoopbackFrags(cond, true);
          acc.write(retVal, 0, 0, acc.getLen());
        }

        if (idx == instr.getNumOthers() / 2)
          FRAME_RET_ACC;

        auto nextVal = instr.other(2 * idx + 1)->as<HWValue>();
        ++idx;
        FRAME_CALL(nextVal)
      }

        // AND/OR are important for single bit MUXs, gets very tricky however.
        // We can't enable this currently as boolean opt isn't powerful enough
        // to actually prune the loopback or reset path even once we extract the
        // control signal. Thus no guaranteed forward progress.

        // case *OP_AND:
        // case *OP_OR: {
        //   if (loopback != nullref) {
        //     if (idx == 0) // reset acc unconditionally
        //       acc = {*val.getNumBits()};

        //     // check last, include in acc
        //     if (idx != 0 && retVal.hasAnyLoopback()) {
        //       unsigned i = idx - 1;
        //       for (unsigned j = 0; j < instr.getNumOthers(); j++) {
        //         if (i == j)
        //           continue;
        //         retVal.addConditionToLoopbackFrags(
        //             isBooleanValue(instr.other(j)->as<HWValue>()),
        //             instr.isOpc(OP_AND));
        //       }
        //       acc.write(retVal, 0, 0, acc.getLen());
        //     }

        //     // find candidate
        //     unsigned i = idx;
        //     for (; i < instr.getNumOthers(); i++) {
        //       // check that all others are boolean-compatible
        //       for (unsigned j = 0; j < instr.getNumOthers(); j++) {
        //         if (i == j)
        //           continue;
        //         auto val = isBooleanValue(instr.other(j)->as<HWValue>());
        //         if (!val || val.is<ConstantRef>())
        //           goto next;
        //       }
        //       goto found;
        //     next:
        //     }
        //     FRAME_RET_ACC;
        //   found:

        //     idx = i + 1;
        //     FRAME_CALL(instr.other(i)->as<HWValue>())

        //   } else {
        //     // if we're looking for constants, check for short circuit
        //     auto it = instr.others().find_if([](OperandRef op) {
        //       auto val = isBooleanValue(op->template as<HWValue>());
        //       return val && val.template is<WireRef>();
        //     });
        //     if (it == instr.other_end())
        //       FRAME_RET();
        //     FRAME_RET(it->as<WireRef>(), instr.isOpc(OP_OR));
        //   }
        // }

      default: {
        if (wire == loopback) // return always-on loopback
          FRAME_RET(nullref, true);
        FRAME_RET();
      }
      }

      map.insert(wire, retVal);
    }

    return retVal;
  }

  static HWValue isBooleanValue(HWValue val) {
    if (val.getNumBits() == 1)
      return val;
    if (auto asConst = val.dyn_as<ConstantRef>()) {
      if (asConst.valueEqualsS(0) || asConst.valueEqualsS(-1))
        return asConst;
    } else if (auto asWire = val.dyn_as<WireRef>()) {
      if (asWire.getDefI().isOpc(HW_REPEAT, OP_SEXT) &&
          asWire.getDefI().other(0)->as<HWValue>().getNumBits() == 1) {
        return asWire.getDefI().other(0)->as<HWValue>();
      }
    }
    return nullref;
  }

#undef FRAME_CALL
#undef FRAME_RET

public:
  explicit LoopbackAnalysis(Context &ctx) : ctx(ctx) {}
};
}; // namespace dyno
