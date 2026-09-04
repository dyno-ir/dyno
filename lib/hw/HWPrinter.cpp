#include "hw/HWPrinter.h"
#include "dyno/Context.h"
#include "dyno/Obj.h"
#include "hw/HWContext.h"
#include "support/Debug.h"
#include <fstream>

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
__attribute__((used)) void dumpInstr(InstrRef instr, Context &ctx,
                                     bool trailingNewline) {
  print.reset();
  print.printInstr(instr, ctx, trailingNewline);
}
__attribute__((used)) void dumpInstr(InstrRef instr, Context &ctx,
                                     bool trailingNewline, bool expandBlocks) {
  print.reset();
  print.printInstr(instr, ctx, trailingNewline, expandBlocks);
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

__attribute__((used)) void dumpDeps(FatDynObjRef<> ref, Context &ctx) {
  if (!Operand::isDefUseOperand(ref)) {
    dumpObj(ref);
    return;
  }
  print.reset();
  print.printDeps(ref.as<FatDynObjRef<InstrDefUse>>()->getSingleDef()->instr(),
                  ctx);
}

__attribute__((used)) void dumpInstrByID(uint32_t id, Context &ctx) {
  dumpInstr(ctx.getStore<Instr>().resolve(ObjRef<Instr>{ObjID{id}}), ctx);
}
__attribute__((used)) void dumpDepsByID(uint32_t id, Context &ctx,
                                        uint maxDepth) {
  dumpDeps(ctx.getStore<Instr>().resolve(ObjRef<Instr>{ObjID{id}}), ctx,
           maxDepth);
}
__attribute__((used)) void dumpRegByID(uint32_t id, Context &ctx) {
  dumpInstr(
      ctx.getStore<Register>().resolve(ObjRef<Register>{ObjID{id}}).iref(),
      ctx);
}

__attribute__((used)) void dumpObj(FatDynObjRef<> obj) {
  print.reset();
  if (auto asInstr = obj.dyn_as<InstrRef>())
    return dumpInstr(asInstr);
  HWPrinter{dbgs()}.printDef(obj);
  if (!obj.isCustom())
    dbgs() << "[" << obj.getObjID() << "]";
};

__attribute__((used)) void dumpBlock(BlockRef block, Context &ctx) {
  print.reset();
  auto tok = print.bindCtx(ctx);
  print.printBlock(block);
}

__attribute__((used)) void dumpCtxToFile(Context &ctx, const char *path) {
  std::ofstream str(path);
  HWCtxPrinter{ctx, str}.printCtx();
};

}; // namespace dyno
