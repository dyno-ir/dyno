#pragma once

#include "dyno/Opcode.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Register.h"
#include "hw/Wire.h"

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

  // look thru trunc and zext
  static RegisterIRef checkIsInputLookthru(HWValue val) {
    while (val.is<WireRef>() &&
           val.as<WireRef>().getDefI().isOpc(OP_TRUNC, OP_ZEXT)) {
      val = val.as<WireRef>().getDefI().other(0)->as<HWValue>();
    }

    return checkIsPort(val, HW_INPUT_REGISTER_DEF, HW_REF_REGISTER_DEF,
                       HW_INOUT_REGISTER_DEF);
  }

  static RegisterIRef checkIsPort(HWValue val, IsDialectOpcode auto... opc) {
    auto rv = checkIsReg(val);
    if (!rv || !rv.isOpc(opc...))
      return nullref;
    return rv;
  }
};

}; // namespace dyno
