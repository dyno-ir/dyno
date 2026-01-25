#include "hw/HWPrinter.h"
#include "dyno/Context.h"
#include "dyno/Obj.h"
#include "hw/HWContext.h"
#include "support/Debug.h"

namespace dyno {

static HWPrinter print{dbgs()};

__attribute__((used)) void dumpCtx(Context &ctx) {
  print.reset();
  print.printCtx(ctx);
}
__attribute__((used)) void dumpInstr(InstrRef instr) {
  print.reset();
  print.printInstr(instr);
}
__attribute__((used)) void dumpInstr(InstrRef instr, Context &ctx) {
  print.reset();
  print.printInstr(instr, ctx);
}

__attribute__((used)) void dumpDeps(InstrRef instr) {
  print.reset();
  print.printDeps(instr);
}
__attribute__((used)) void dumpDeps(InstrRef instr, uint maxDepth) {
  print.reset();
  print.printDeps(instr, maxDepth);
}
__attribute__((used)) void dumpDeps(InstrRef instr, Context &ctx) {
  print.reset();
  print.printDeps(instr, ctx);
}
__attribute__((used)) void dumpDeps(InstrRef instr, Context &ctx,
                                    uint maxDepth) {
  print.reset();
  print.printDeps(instr, ctx, maxDepth);
}
__attribute__((used)) void dumpObj(FatDynObjRef<> obj) {
  print.reset();
  if (auto asInstr = obj.dyn_as<InstrRef>())
    return dumpInstr(asInstr);
  HWPrinter{dbgs()}.printDef(obj);
  if (!obj.isCustom())
    dbgs() << "[" << obj.getObjID() << "]";
};

}; // namespace dyno
