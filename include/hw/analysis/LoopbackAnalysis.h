#pragma once

#include "dyno/Context.h"
#include "dyno/IDImpl.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Wire.h"
#include "hw/analysis/RegisterValue.h"
#include "op/IDs.h"
#include "support/DenseMap.h"
#include "support/RTTI.h"
#include "support/SmallVec.h"
#include <limits>
namespace dyno {

class LoopbackFrag {
  constexpr static ObjRef<Wire> invalid{
      ObjID{std::numeric_limits<ObjID::num_t>::max() - 1}};

public:
  uint32_t dstAddr;
  uint32_t len;
  std::array<ObjRef<Wire>, 4> enable{invalid, nullref, nullref, nullref};

  LoopbackFrag() = default;
  LoopbackFrag(uint32_t dstAddr, uint32_t len) : dstAddr(dstAddr), len(len) {}
  LoopbackFrag(uint32_t dstAddr, uint32_t len, ObjRef<Wire> en)
      : dstAddr(dstAddr), len(len) {
    enable[0] = en;
  }
  LoopbackFrag(uint32_t dstAddr, uint32_t len, const LoopbackFrag &frag)
      : dstAddr(dstAddr), len(len), enable(frag.enable) {}

  bool push_back(ObjRef<Wire> en) {
    if (!*this)
      return true; // nothing to do
    for (unsigned i = 0; i < enable.size(); i++) {
      if (enable[i])
        continue;
      enable[i] = en;
      return true;
    }
    return false;
  }
  uint32_t size() const {
    if (!*this)
      return 0;
    for (unsigned i = 0; i < enable.size(); i++) {
      if (!enable[i])
        return i;
    }
    return enable.size();
  }
  auto begin() { return enable.begin(); }
  auto end() { return enable.begin() + size(); }

  explicit operator bool() const { return enable[0] != invalid; }

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

using LoopbackPartition = GenericPartitions<LoopbackFrag>;

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

#define FRAME_RET(x)                                                           \
  {                                                                            \
    retVal = {*val.getNumBits(), x};                                           \
    stack.pop_back();                                                          \
    continue;                                                                  \
  }

#define FRAME_CALL(...)                                                        \
  {                                                                            \
    stack.emplace_back(__VA_ARGS__);                                           \
    continue;                                                                  \
  }

public:
  LoopbackPartition get(HWValue value, HWValue loopback) {
    stack.emplace_back(value, 0);
    SmallDenseMap<ObjRef<Wire>, LoopbackPartition> map;

    while (!stack.empty()) {
      auto &[val, idx, acc] = stack.back();
      addr = 0;

      if (auto asConst = val.dyn_as<ConstantRef>()) {
        if (auto loopbackConst = loopback.dyn_as<ConstantRef>()) {
          BigInt out;
          BigInt::rangeSelectOp4S(out, loopbackConst, addr,
                                  asConst.getNumBits());

          if (out == asConst) // return always-on loopback
            FRAME_RET(nullref);
        }
        FRAME_RET();
      }

      auto wire = val.as<WireRef>();
      auto instr = wire.getDefI();

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
        if (idx != 0) {
          // add condition to loopback frags
          for (auto &frag : retVal.frags) {
            if (!frag)
              continue;
            frag.push_back(instr.other((idx - 1) * 2)->as<WireRef>());
          }

          if (idx == 1)
            acc = std::move(retVal);
          else
            acc.write(retVal, 0, 0, acc.getLen());
        }

        if (idx == instr.getNumOthers() / 2) {
          retVal = std::move(acc);
          stack.pop_back();
          break;
        }
        auto nextVal = instr.other(2 * idx + 1)->as<HWValue>();
        ++idx;
        FRAME_CALL(nextVal)
      }

      default: {
        if (wire == loopback)
          FRAME_RET(nullref);
        FRAME_RET();
      }
      }

      map.insert(wire, retVal);
    }

    return retVal;
  }

public:
  explicit LoopbackAnalysis(Context &ctx) : ctx(ctx) {}
};
}; // namespace dyno
