#pragma once
#include "aig/AIG.h"
#include "aig/AIGPrinter.h"
#include "aig/IDs.h"
#include "aig/PrintParse.h"
#include "dyno/IDs.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Obj.h"
#include "dyno/Opcode.h"
#include "dynomite/IDs.h"
#include "hw/DebugInfo.h"
#include "hw/HWAbstraction.h"
#include "dyno/Context.h"
#include "hw/HWContext.h"
#include "hw/IDs.h"
#include "hw/PrintParse.h"
#include "hw/Process.h"
#include "meta/PrintParse.h"
#include "op/IDs.h"
#include "op/PrintParse.h"
#include "support/ArrayRef.h"
#include "support/TempBind.h"
#include <fstream>
#include <ostream>
namespace dyno {

void dumpCtx(Context &ctx);
void dumpInstr(InstrRef instr);
void dumpInstr(InstrRef instr, Context &ctx);
void dumpDeps(InstrRef instr);
void dumpDeps(InstrRef instr, Context &ctx);
void dumpDeps(InstrRef instr, Context &ctx, unsigned depth);
void dumpObj(FatDynObjRef<> obj);

class HWPrinter : public PrinterWrapper<CoreDialectPrinter, MetaDialectPrinter,
                                        OpDialectPrinter, HWDialectPrinter,
                                        AIGDialectPrinter> {
public:
  HWPrinter(std::ostream &str) : PrinterWrapper(str) {
    setDefaultDialects({DialectID{DIALECT_CORE}, DialectID{DIALECT_OP},
                        DialectID{DIALECT_HW}});
  }

  auto &regNames() { return std::get<HWDialectPrinter>(printers).regNames; }

  [[nodiscard]] auto bindCtx(Context &ctx) {
    return std::pair(sourceLocInfo.bind(&ctx.getCtx<CoreDialectContext>().instrSourceLocInfo),
                     regNames().bind(&ctx.getCtx<HWDialectContext>().regNameInfo));
  }

  void printCtx(Context &ctx) {
    auto tok = bindCtx(ctx);
    for (auto mod : ctx.getStore<Module>()) {
      if (mod.iref().isOpc(HW_STDCELL_DEF))
        continue;
      printInstr(mod.iref());
    }
  }

  using PrinterWrapper::printInstr;
  void printInstr(InstrRef instr, Context &ctx) {
    auto tok = bindCtx(ctx);
    printInstr(instr);
  }

  void printDeps(InstrRef instr, unsigned maxDepth = -1) {
    if (maxDepth)
      for (auto use : instr.others()) {
        if (!Operand::isDefUseOperand(*use))
          continue;
        printDeps(use->as<FatDynObjRef<InstrDefUse>>()->getSingleDef()->instr(),
                  maxDepth - 1);
      }
    printInstr(instr);
  }
  void printDeps(InstrRef instr, Context &ctx, unsigned maxDepth = -1) {
    if (maxDepth)
      for (auto use : instr.others()) {
        if (!Operand::isDefUseOperand(*use))
          continue;
        printDeps(use->as<FatDynObjRef<InstrDefUse>>()->getSingleDef()->instr(),
                  ctx, maxDepth - 1);
      }
    printInstr(instr, ctx);
  }
};

}; // namespace dyno
