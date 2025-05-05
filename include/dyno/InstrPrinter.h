#pragma once

#include "dyno/CFG.h"
#include "dyno/Obj.h"
#include <dyno/Instr.h>
#include <dyno/Interface.h>
#include <initializer_list>
#include <iostream>
#include <limits>
#include <ostream>
#include <unordered_map>

namespace dyno {

class FieldPrinter {
  std::vector<bool> isDefault =
      std::vector<bool>(std::numeric_limits<DialectID::num_t>::max() + 1ULL);

public:
  Interface<DialectInfo> dialectI;
  Interface<TyInfo> tyI;
  Interface<OpcodeInfo> opcodeI;

  std::ostream &str;

  FieldPrinter(std::ostream &str, Interface<DialectInfo> dialectI,
               Interface<TyInfo> tyI, Interface<OpcodeInfo> opcodeI)
      : dialectI(dialectI), tyI(tyI), opcodeI(opcodeI), str(str) {}

  void printType(DynObjRef ref) {
    if (!isDefault[ref.getDialectID()]) {
      str << dialectI[ref].name << ".";
    }
    str << tyI[ref].name;
  }

  void printOpcode(InstrRef ref) {
    if (!isDefault[ref.getDialect()]) {
      str << dialectI[ref.getDialect()]->name << ".";
    }
    str << opcodeI[ref].name;
  }

  void setDefaultDialects(std::initializer_list<DialectID> dialects) {
    for (auto dial : dialects)
      isDefault[dial] = 1;
  }
};

class RefPrinter {
public:
  FieldPrinter &fieldPrinter;
  std::ostream &str;

  std::unordered_map<DynObjRef, size_t> introduced;
  RefPrinter(FieldPrinter &fieldPrinter)
      : fieldPrinter(fieldPrinter), str(fieldPrinter.str) {}

  void printRef(FatDynObjRef<> ref) {
    if (fieldPrinter.tyI[ref].print) {
      fieldPrinter.tyI[ref].print(str, ref, false);
      return;
    }
    fieldPrinter.printType(ref);
    str << '[' << ref.getObjID() << "]";
    printCustom(ref);
  }

  void printConstruct(FatDynObjRef<> ref) {
    if (fieldPrinter.tyI[ref].print) {
      fieldPrinter.tyI[ref].print(str, ref, true);
      return;
    }
    fieldPrinter.printType(ref);
    // str << '(' << ref.getObjID() << ")";
    // printCustom(ref);
  }

  void printCustom(FatDynObjRef<> ref) {
    if (!ref.isCustom())
      return;
    str << '(' << ref.getCustom() << ')';
  }

  void print(FatDynObjRef<> ref) {
    DynObjRef noCustom = ref;
    noCustom.clearCustom();
    auto it = introduced.find(noCustom);
    if (it != introduced.end()) {
      str << '%' << it->second;
      printCustom(ref);
      return;
    }
    printRef(ref);
  }

  void introduce(FatDynObjRef<> ref) {
    DynObjRef noCustom = ref;
    noCustom.clearCustom();
    auto [it, succ] = introduced.try_emplace(noCustom, introduced.size());
    str << '%' << it->second << ":";
  }

  void introduceRef(FatDynObjRef<> ref) {
    introduce(ref);
    printConstruct(ref);
  }

  void reset() { introduced.clear(); }
};

class Printer {
  RefPrinter &refPrinter;
  FieldPrinter &fieldPrinter;
  int indent = 0;

  void addIndent() { indent++; }
  void removeIndent() { indent--; }
  void printIndent() {
    for (int i = 0; i < indent; i++)
      str << "  ";
  }
  void printNewLineIndent() {
    str << '\n';
    for (int i = 0; i < indent; i++)
      str << "  ";
  }

public:
  std::ostream &str;

  Printer(RefPrinter &refPrinter)
      : refPrinter(refPrinter), fieldPrinter(refPrinter.fieldPrinter),
        str(refPrinter.str) {}

  void printBlock(BlockRef block) {
    str << "{\n";
    addIndent();
    for (auto it : block) {
      printIndent();
      printInstr(it);
    }
    removeIndent();
    if (!block.empty())
      printIndent();
    str << "}";
  }

  void printInstr(InstrRef instr) {
    fieldPrinter.printOpcode(instr);
    str << ' ';

    for (size_t i = 0; i < instr.getNumOperands(); i++) {
      auto ref = instr.operand(i)->fat();
      if (i < instr.getNumDefs()) {
        refPrinter.introduceRef(ref);
      } else {
        refPrinter.print(ref);
      }

      if (i != instr.getNumOperands() - 1)
        str << ", ";
    }

    bool first = 1;
    for (size_t i = 0; i < instr.getNumDefs(); i++) {
      if (auto asBlock = instr.def(i)->dyn_as<BlockRef>()) {
        if (first) {
          first = 0;
          str << ' ';
        }
        printBlock(asBlock);
      }
    }

    str << '\n';
  }
};

} // namespace dyno
