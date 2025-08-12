#include "hw/HWPrinter.h"
#include "hw/HWContext.h"
#include "support/Debug.h"
#include <memory>

namespace dyno {

static HWPrinter print{dbgs()};

void dumpCtx(HWContext &ctx) { print.printCtx(ctx); }
void dumpInstr(InstrRef instr) { print.printInstr(instr); }
void dumpInstr(InstrRef instr, HWContext &ctx) { print.printInstr(instr, ctx); }

void dumpObj(FatDynObjRef<> obj) {
  if (auto asInstr = obj.dyn_as<InstrRef>())
    return dumpInstr(asInstr);
  HWPrinter{dbgs()}.printDef(obj);
  if (!obj.isCustom())
    dbgs() << "[" << obj.getObjID() << "]";
};

}; // namespace dyno
