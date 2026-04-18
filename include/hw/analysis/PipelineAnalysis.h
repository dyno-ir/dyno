#pragma once
#include "dyno/Context.h"
#include "hw/FlipFlop.h"
#include "hw/HWValue.h"
#include "hw/LoadStore.h"
#include "hw/Register.h"
#include "hw/Wire.h"
#include "support/Bits.h"
#include <optional>

namespace dyno {
class PipelineAnalysis {
public:
  struct WireSlice {
    ObjRef<Wire> wire;
    uint32_t addr;
    uint32_t len;
  };
  struct Stage {
    uint32_t depth;
    // if clk/rst/clken is the same for neighboring flip flops, this points to
    // the latter one (first one seen) with depth >= 2
    FlipFlopIRef ff;
  };
  struct Pipeline {
    uint32_t totalDelay;
    WireSlice slice;
    SmallVec<Stage, 4> stages;
  };

  static Pipeline getPipeline(HWValue value) {
    Pipeline p{0, {nullref, 0, 0}, {}};
    p.slice.len = *value.getNumBits();
    HWValue curr = value;

    while (curr.is<WireRef>()) {
      auto wire = curr.as<WireRef>();
      auto instr = wire.getDefI();

      switch (*instr.getDialectOpcode()) {
      case *HW_FLIP_FLOP:
      case *HW_FLIP_FLOP_SRST: {
        auto ff = instr.as<FlipFlopIRef>();
        p.totalDelay++;
        if (!p.stages.empty() && isSameFlopType(p.stages.back().ff, ff))
          p.stages.back().depth++;
        else
          p.stages.push_back({1, ff});

        curr = ff.d();
        break;
      }
      case *HW_LOAD: {
        auto load = instr.as<LoadIRef>();
        if (!load.isConstantOffs())
          break;
        uint32_t ldBase = load.getBase();
        uint32_t ldLen = load.getLen();
        auto reg = load.reg().iref();
        auto store = reg.getSingleStore();
        if (!store || !store.isOpc(HW_STORE))
          break;
        auto stRef = store.as<StoreIRef>();
        if (!stRef.isConstantOffs())
          break;
        uint32_t stBase = stRef.getBase();
        uint32_t stLen = stRef.getLen();
        if (ldBase < stBase || ldBase + ldLen > stBase + stLen)
          break;
        p.slice.addr += ldBase - stBase;
        p.slice.len = std::min(p.slice.len, ldLen);
        curr = stRef.value();
        break;
      }
      case *OP_TRUNC: {
        curr = instr.other(0)->as<HWValue>();
        break;
      }
      case *OP_ZEXT: {
        curr = instr.other(0)->as<HWValue>();
        p.slice.len = *curr.getNumBits();
        break;
      }
      case *HW_SPLICE: {
        auto splice = instr.as<SpliceIRef>();
        if (!splice.isConstantOffs())
          break;
        curr = splice.in()->as<HWValue>();
        p.slice.addr += splice.getBase();
        p.slice.len = std::min(p.slice.len, splice.getLen());
        break;
      }
      default:
        break;
      }
    }

    if (!curr.is<WireRef>())
      return p;

    p.slice.wire = curr.as<WireRef>();
    return p;
  }

  // in the future we can add a retiming plan here to so if hypothetically
  // we could get a certain number of stages
  static Pipeline getOutPipeline(WireRef value, bool coalesce = true) {
    Pipeline p{0, {nullref, 0, 0}, {}};
    p.slice.len = *value.getNumBits();
    WireRef curr = value;

    while (curr.hasSingleUse()) {
      auto use = curr.getSingleUse();
      auto instr = use->instr();

      switch (*instr.getDialectOpcode()) {
      case *HW_FLIP_FLOP:
      case *HW_FLIP_FLOP_SRST: {
        auto ff = instr.as<FlipFlopIRef>();
        if (use->getNum() != 2)
          break;
        p.totalDelay++;
        if (coalesce && !p.stages.empty() &&
            isSameFlopType(p.stages.back().ff, ff))
          p.stages.back().depth++;
        else
          p.stages.push_back({1, ff});

        curr = ff.q();
        continue;
      }
      case *HW_ASSUME: {
        curr = instr.def()->as<WireRef>();
        continue;
      }
      // todo: remap
      default:
        break;
      }
      break;
    }
    p.slice.wire = curr.as<WireRef>();
    return p;
  }

  static bool isSameFlopType(FlipFlopIRef lhs, FlipFlopIRef rhs) {
    if (lhs.clkRaw() != rhs.clkRaw() || lhs.clkEnRaw() != rhs.clkEnRaw() ||
        lhs.numRsts() != rhs.numRsts())
      return false;
    for (unsigned i = 0; i < lhs.numRsts(); ++i) {
      if (lhs.rstRaw(i) != rhs.rstRaw(i) || lhs.rstVal(i) != rhs.rstVal(i))
        return false;
    }
    return true;
  }
  static bool isImplementableWithFlop(FlipFlopIRef act, FlipFlopIRef mod) {
    if (act.hasClkEn() && !mod.hasClkEn())
      return false;
    if (act.numRsts() > mod.numRsts())
      return false;
    for (unsigned i = 0; i < act.numRsts(); i++) {
      // non-constant reset values may occur as parameters (especially on model
      // flops)
      switch ((act.rstVal(i).is<ConstantRef>() << 1) |
              mod.rstVal(i).is<ConstantRef>()) {
      case 0b00:
        continue;
      case 0b01:
        return false;
      case 0b10:
        continue;
      case 0b11:
        if (act.rstVal(i).as<ConstantRef>() != mod.rstVal(i).as<ConstantRef>())
          return false;
        continue;
      }
    }
    return true;
  }

  // model pipeline dictates #stages, actual pipeline dictates rst/en
  static bool isImplementableWithPipe(
      const Pipeline &act, const Pipeline &mod,
      SmallDenseMap<ObjRef<Wire>, ObjRef<Wire>, 8> &assignments) {
    auto actIt = act.stages.begin();
    auto modIt = mod.stages.begin();

    uint32_t actSub = 0;
    uint32_t modSub = 0;

    auto checkAssign = [&assignments](WireRef act, WireRef mod) -> bool {
      auto [found, it] = assignments.findOrInsert(act, mod);
      if (found) {
        return it.val() == mod;
      }
      return true;
    };

    while (actIt != act.stages.end() && modIt != mod.stages.end()) {
      auto max = std::max(actIt->depth - actSub, modIt->depth - modSub);

      if (!isImplementableWithFlop(actIt->ff, modIt->ff))
        return false;

      if (modIt->ff.hasClkEn()) {
        if (!checkAssign(!actIt->ff.hasClkEn()
                             ? nullref
                             : actIt->ff.clkEnRaw().as<WireRef>(),
                         modIt->ff.clkEnRaw().as<WireRef>()))
          return false;
      }
      for (unsigned i = 0; i < modIt->ff.numRsts(); i++) {
        if (!checkAssign(i >= actIt->ff.numRsts()
                             ? nullref
                             : actIt->ff.rstRaw(i).as<WireRef>(),
                         modIt->ff.rstRaw(i).as<WireRef>()))
          return false;
      }

      actSub += max;
      if (actSub == actIt->depth) {
        actSub = 0;
        ++actIt;
      }
      modSub += max;
      if (modSub == modIt->depth) {
        modSub = 0;
        ++modIt;
      }
    }

    return modIt == mod.stages.end();
  }

  static bool isImplementableWithPipe(const Pipeline &act,
                                      const Pipeline &mod) {
    SmallDenseMap<ObjRef<Wire>, ObjRef<Wire>, 8> assignments;
    return isImplementableWithPipe(act, mod, assignments);
  }
};

}; // namespace dyno
