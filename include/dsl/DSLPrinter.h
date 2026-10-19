#pragma once

#include "dsl/DSLInstr.h"
#include "dsl/PrintParse.h"
#include "dyno/InstrPrinter.h"
#include "meta/PrintParse.h"
#include "op/PrintParse.h"

namespace dyno {

class DSLPrinter
    : public ContextPrinterWrapper<CoreDialectPrinter, OpDialectPrinter,
                                   DSLDialectPrinter, MetaDialectPrinter> {
public:
  DSLPrinter(Context &ctx, std::ostream &os);
};

void dumpInstr(DSLInstrRef instr, Context &ctx);
void dumpObj(FatDynObjRef<> obj, Context &ctx);

} // namespace dyno
