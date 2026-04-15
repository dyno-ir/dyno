#pragma once

#include "dyno/Opcode.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Register.h"
#include "hw/Wire.h"
#include <optional>

namespace dyno {

// wire/variable conversion and mapping utilities
class WireVariable {
public:
  static RegisterIRef checkIsReg(HWValue val) {
    if (!val.is<WireRef>())
      return nullref;
    auto wire = val.as<WireRef>();
    auto instr = wire.getDefI();

    if (instr.isOpc(HW_LOAD)) {
      auto load = instr.as<LoadIRef>();
      if (!load.isFullReg())
        return nullref;
      auto reg = load.reg().iref();
      return reg;
    }

    if (auto use = wire.getSingleUse()) {
      instr = use->instr();
      if (instr.isOpc(HW_STORE)) {
        auto store = instr.as<StoreIRef>();
        if (!store.isFullReg())
          return nullref;
        auto reg = store.reg().iref();
        return reg;
      }
    }
    return nullref;
  }

  struct RegSlice {
    RegisterIRef reg;
    uint32_t addr;
    uint32_t len;
  };
  static std::optional<RegSlice> checkIsInputRegRemap(HWValue val) {
    uint32_t addr = 0;
    uint32_t len = *val.getNumBits();
    while (1) {
      if (!val.is<WireRef>())
        return std::nullopt;
      auto wire = val.as<WireRef>();
      auto instr = wire.getDefI();

      switch (*instr.getDialectOpcode()) {
      case *HW_LOAD: {
        auto asLoad = instr.as<LoadIRef>();
        return {{asLoad.reg().iref(), addr, len}};
      }
      case *OP_TRUNC: {
        val = instr.other(0)->as<HWValue>();
        break;
      }
      case *OP_ZEXT: {
        val = instr.other(0)->as<HWValue>();
        len = *val.getNumBits();
        break;
      }
      case *HW_SPLICE: {
        auto asSplice = instr.as<SpliceIRef>();
        if (!asSplice.isConstantOffs())
          return std::nullopt;
        val = asSplice.in()->as<HWValue>();
        addr += asSplice.getBase();
        len = std::min(len, asSplice.getLen());
        break;
      }
      default: {
        return std::nullopt;
      }
      }
    }
  }

  // look thru trunc and zext
  static std::optional<RegSlice> checkIsInputLookthru(HWValue val) {
    auto rv = checkIsInputRegRemap(val);
    if (rv && rv->reg.isOpc(HW_INPUT_REGISTER_DEF, HW_INOUT_REGISTER_DEF,
                            HW_REF_REGISTER_DEF))
      return rv;
    return std::nullopt;
  }

  static RegisterIRef checkIsPort(HWValue val, IsDialectOpcode auto... opc) {
    auto rv = checkIsReg(val);
    if (!rv || !rv.isOpc(opc...))
      return nullref;
    return rv;
  }

  struct EnableSignal {
    WireRef wire;
    uint32_t addr;
    bool polarity;
  };
  static bool
  connectEnSignal(EnableSignal signal,
                  CallableRef<void(LoadIRef, EnableSignal)> connect) {
    SmallVec<EnableSignal, 8> stack{signal};
    while (!stack.empty()) {
      EnableSignal cur = stack.pop_back_val();
      auto instr = cur.wire.getDefI();
      switch (*instr.getDialectOpcode()) {
      case *HW_LOAD: {
        auto asLoad = instr.as<LoadIRef>();
        if (!asLoad.isConstantOffs())
          return false;
        connect(asLoad, EnableSignal{cur.wire, cur.addr + asLoad.getBase(),
                                     cur.polarity});
        break;
      }
      case *OP_OR:
      case *OP_AND: {
        if (cur.polarity == instr.isOpc(OP_OR))
          return false;
        for (auto op : instr.others().as<HWValue>()) {
          assert(op.is<WireRef>());
          stack.emplace_back(op.as<WireRef>(), cur.addr, cur.polarity);
        }
        break;
      }
      case *OP_NOT: {
        stack.emplace_back(instr.other(0).as<WireRef>(), cur.addr,
                           !cur.polarity);
        break;
      }
      case *HW_SPLICE: {
        auto asSplice = instr.as<SpliceIRef>();
        if (!asSplice.isConstantOffs())
          return false;
        stack.emplace_back(asSplice.in()->as<WireRef>(),
                           cur.addr + asSplice.getBase(), cur.polarity);
        break;
      }
      case *OP_TRUNC: {
        stack.emplace_back(instr.other(0).as<WireRef>(), cur.addr,
                           cur.polarity);
        break;
      }
      default:
        return false;
      }
    }
    return true;
  }
};

}; // namespace dyno
