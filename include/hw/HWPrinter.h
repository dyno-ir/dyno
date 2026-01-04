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

void dumpCtx(HWContext &ctx);
void dumpInstr(InstrRef instr);
void dumpInstr(InstrRef instr, HWContext &ctx);
void dumpDeps(InstrRef instr);
void dumpDeps(InstrRef instr, HWContext &ctx);
void dumpDeps(InstrRef instr, HWContext &ctx, unsigned depth);
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

  [[nodiscard]] auto bindCtx(HWContext &ctx) {
    return std::pair(sourceLocInfo.bind(&ctx.sourceLocInfo),
                     regNames().bind(&ctx.regNameInfo));
  }

  void printCtx(HWContext &ctx) {
    auto tok = bindCtx(ctx);
    for (auto mod : ctx.getModules()) {
      if (mod.iref().isOpc(HW_STDCELL_DEF))
        continue;
      printInstr(mod.iref());
    }
  }

  using PrinterWrapper::printInstr;
  void printInstr(InstrRef instr, HWContext &ctx) {
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
  void printDeps(InstrRef instr, HWContext &ctx, unsigned maxDepth = -1) {
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
