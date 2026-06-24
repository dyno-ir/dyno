#pragma once

#include "dyno/InstrPrinter.h"

namespace dyno {

class DSLDialectPrinter {
public:
  static constexpr DialectID dialect{DIALECT_DSL};
  PrinterBase *base;
  DSLDialectPrinter(PrinterBase *base) : base(base) {
    base->interfaces.registerVal<PrinterBase::type::print_fn>(
        dialect,
        CallableRef{this, BindMethod<&DSLDialectPrinter::printType>::fv});
  }

  bool printType(FatDynObjRef<> ref, bool def) { return false; }
};
} // namespace dyno
