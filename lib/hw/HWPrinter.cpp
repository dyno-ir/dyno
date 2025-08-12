#include "hw/HWPrinter.h"
#include "hw/HWContext.h"
#include "support/Debug.h"
#include <memory>

namespace dyno {

static HWPrinter print{dbgs()};

void dumpCtx(HWContext &ctx) {
  print.reset();
  print.printCtx(ctx);
}
void dumpInstr(InstrRef instr) {
  print.reset();
  print.printInstr(instr);
}
void dumpInstr(InstrRef instr, HWContext &ctx) {
  print.reset();
  print.printInstr(instr, ctx);
}

void dumpObj(FatDynObjRef<> obj) {
  print.reset();
  if (auto asInstr = obj.dyn_as<InstrRef>())
    return dumpInstr(asInstr);
  HWPrinter{dbgs()}.printDef(obj);
  if (!obj.isCustom())
    dbgs() << "[" << obj.getObjID() << "]";
};

}; // namespace dyno
