#pragma once
#include "aig/AIG.h"
#include "aig/AIGPrinter.h"
#include "aig/IDs.h"
#include "aig/PrintParse.h"
#include "dyno/Context.h"
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

void dumpCtx(Context &ctx);
void dumpInstr(InstrRef instr);
void dumpInstr(InstrRef instr, Context &ctx);
void dumpInstr(InstrRef instr, Context &ctx, bool trailingNewline);
void dumpInstr(InstrRef instr, Context &ctx, bool trailingNewline,
               bool expandBlocks);
void dumpDeps(InstrRef instr);
void dumpDeps(InstrRef instr, Context &ctx);
void dumpDeps(InstrRef instr, Context &ctx, unsigned depth);
void dumpObj(FatDynObjRef<> obj);

template <typename Derived> class HWPrinterImpl {
  auto &self() { return *reinterpret_cast<Derived *>(this); }

public:
  auto &regNames() {
    return self().printers.template get<HWDialectPrinter>().regNames;
  }

  void printDeps(InstrRef instr, unsigned maxDepth = -1) {
    if (maxDepth)
      for (auto use : instr.others()) {
        if (!Operand::isDefUseOperand(*use))
          continue;
        printDeps(use->as<FatDynObjRef<InstrDefUse>>()->getSingleDef()->instr(),
                  maxDepth - 1);
      }
    self().printInstr(instr);
  }
};

class HWPrinter : public HWPrinterImpl<HWPrinter>,
                  public PrinterWrapper<CoreDialectPrinter, MetaDialectPrinter,
                                        OpDialectPrinter, HWDialectPrinter,
                                        AIGDialectPrinter> {
  friend class HWPrinterImpl<HWPrinter>;

public:
  HWPrinter(std::ostream &str) : PrinterWrapper(str) {
    setDefaultDialects({DialectID{DIALECT_CORE}, DialectID{DIALECT_OP},
                        DialectID{DIALECT_HW}});
  }
  HWPrinter() : PrinterWrapper(OStreamWrapper{}) {
    setDefaultDialects({DialectID{DIALECT_CORE}, DialectID{DIALECT_OP},
                        DialectID{DIALECT_HW}});
  }

  [[nodiscard]] auto bindCtx(Context &ctx) {
    return std::pair(
        sourceLocInfo.bind(
            &ctx.getCtx<CoreDialectContext>().instrSourceLocInfo),
        regNames().bind(&ctx.getCtx<HWDialectContext>().regNameInfo));
  }

  void printCtx(Context &ctx, bool printStdCells = false) {
    auto tok = bindCtx(ctx);
    for (auto mod : ctx.getStore<Module>()) {
      if (mod.iref().isOpc(HW_STDCELL_DEF) && !printStdCells)
        continue;
      PrinterBase::printInstr(mod.iref());
    }
  }
  using PrinterWrapper::printInstr;
  void printInstr(InstrRef instr, Context &ctx, bool trailingNewline = true,
                  bool expandBlocks = true) {
    auto tok = bindCtx(ctx);
    PrinterBase::printInstr(instr, trailingNewline, expandBlocks);
  }
  using HWPrinterImpl::printDeps;
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

// Smaller/preferred version of HWPrinter when context is known and always the
// same. Uses context's dialect infos.
class HWCtxPrinter
    : public HWPrinterImpl<HWCtxPrinter>,
      public ContextPrinterWrapper<CoreDialectPrinter, MetaDialectPrinter,
                                   OpDialectPrinter, HWDialectPrinter,
                                   AIGDialectPrinter> {
  friend class HWPrinterImpl<HWCtxPrinter>;

public:
  HWCtxPrinter(Context &ctx, std::ostream &str)
      : ContextPrinterWrapper(ctx, str) {
    setDefaultDialects({DIALECT_CORE, DIALECT_OP, DIALECT_HW});
  }
  HWCtxPrinter(Context &ctx) : ContextPrinterWrapper(ctx, OStreamWrapper{}) {
    setDefaultDialects({DIALECT_CORE, DIALECT_OP, DIALECT_HW});
  }
};
}; // namespace dyno
