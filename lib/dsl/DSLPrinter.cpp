#include "dsl/DSLPrinter.h"
#include "support/Debug.h"

namespace dyno {

DSLPrinter::DSLPrinter(Context &ctx, std::ostream &os)
    : ContextPrinterWrapper(ctx, os) {
  sourceLocInfo = &ctx.getCtx<CoreDialectContext>().instrSourceLocInfo;
}

__attribute__((used)) void dumpInstr(DSLInstrRef instr, Context &ctx) {
  DSLPrinter(ctx, dbgs()).printInstr(instr);
}

__attribute__((used)) void dumpObj(FatDynObjRef<> obj, Context &ctx) {
  if (auto instr = obj.dyn_as<InstrRef>())
    return dumpInstr(DSLInstrRef(instr), ctx);
  DSLPrinter(ctx, dbgs()).printDef(obj);
  if (!obj.isCustom())
    dbgs() << '[' << obj.getObjID() << ']';
}

} // namespace dyno
