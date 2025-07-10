#include "hw/HWPrinter.h"
#include "support/Debug.h"

namespace dyno {

void dumpCtx(HWContext &ctx) { HWPrinter{dbgs()}.printCtx(ctx); }

void dumpInstr(InstrRef instr) { HWPrinter{dbgs()}.printInstr(instr); }

void dumpObj(FatDynObjRef<> obj) {
  if (auto asInstr = obj.dyn_as<InstrRef>())
    return dumpInstr(asInstr);
  HWPrinter{dbgs()}.printDef(obj);
  if (!obj.isCustom())
    dbgs() << "[" << obj.getObjID() << "]";
};

}; // namespace dyno
