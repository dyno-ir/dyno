#pragma once

#include <dyno/Instr.h>
#include <dyno/Interface.h>
#include <dyno/ObjInfo.h>
#include <iostream>
#include <unordered_map>

namespace dyno {

class RefPrinter {
public:
  std::ostream &str;

  Interface<DialectInfo> dialectI;
  Interface<TyInfo> tyI;

  std::unordered_map<DynObjRef, size_t> introduced;

  RefPrinter(std::ostream &str, Interface<DialectInfo> dialectI,
             Interface<TyInfo> tyI)
      : str(str), dialectI(dialectI), tyI(tyI) {}

  void printAlways(DynObjRef ref) {
    str << "&" << dialectI[ref].name << '.' << tyI[ref].name << '('
        << ref.getObjID() << ")";
    printCustom(ref);
  }

  void printCustom(DynObjRef ref) {
    if (!ref.isCustom())
      return;
    str << '(' << ref.getCustom() << ')';
  }

  void print(DynObjRef ref) {
    DynObjRef noCustom = ref;
    noCustom.clearCustom();
    auto it = introduced.find(noCustom);
    if (it != introduced.end()) {
      str << '%' << it->second;
      printCustom(ref);
      return;
    }
    printAlways(ref);
  }

  void introduce(DynObjRef ref) {
    DynObjRef noCustom = ref;
    noCustom.clearCustom();
    auto [it, succ] = introduced.try_emplace(noCustom, introduced.size());
    str << '%' << it->second << " = ";
  }

  void introduceRef(DynObjRef ref) {
    introduce(ref);
    printAlways(ref);
    str << "\n";
  }

  void reset() { introduced.clear(); }
};

class InstrPrinter {
  RefPrinter &refPrinter;

public:
  std::ostream &str;

  Interface<OpcodeInfo> opcodeI;

  InstrPrinter(RefPrinter &refPrinter, Interface<OpcodeInfo> opcodeI)
      : refPrinter(refPrinter), str(refPrinter.str), opcodeI(opcodeI) {}

  void print(InstrRef instr) {
    refPrinter.introduce(instr);
    str << refPrinter.dialectI[instr.getDialect()]->name << "."
        << opcodeI[instr].name << " | ";
    for (auto op : instr.defs()) {
      refPrinter.print(op.getRef());
      str << ' ';
    }
    str << '|';
    for (auto op : instr.others()) {
      str << ' ';
      refPrinter.print(op.getRef());
    }
    str << '\n';
  }
};

} // namespace dyno
