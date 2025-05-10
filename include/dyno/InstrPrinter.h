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

class IndentPrinter {
public:
  std::ostream &str;

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

  IndentPrinter(std::ostream &str) : str(str) {}
};

class Printer {
  IndentPrinter indentPrint;
  std::unordered_map<DynObjRef, size_t> introduced;

  std::vector<bool> isDefault =
      std::vector<bool>(std::numeric_limits<DialectID::num_t>::max() + 1ULL);

public:
  Interface<DialectInfo> dialectI;
  Interface<TyInfo> tyI;
  Interface<OpcodeInfo> opcodeI;

  std::ostream &str;

  Printer(std::ostream &str, Interface<DialectInfo> dialectI,
          Interface<TyInfo> tyI, Interface<OpcodeInfo> opcodeI)
      : indentPrint(str), dialectI(dialectI), tyI(tyI), opcodeI(opcodeI),
        str(str) {}

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

  void printUse(FatDynObjRef<> ref) {
    if (tyI[ref].print) {
      tyI[ref].print(str, ref, false);
      return;
    }
    printType(ref);
    str << '[' << ref.getObjID() << "]";
    printCustom(ref);
  }

  void printDef(FatDynObjRef<> ref) {
    if (tyI[ref].print) {
      tyI[ref].print(str, ref, true);
      return;
    }
    printType(ref);
  }

  void printCustom(FatDynObjRef<> ref) {
    if (!ref.isCustom())
      return;
    str << '(' << ref.getCustom() << ')';
  }

  void printRefOrUse(FatDynObjRef<> ref) {
    DynObjRef noCustom = ref;
    noCustom.clearCustom();
    auto it = introduced.find(noCustom);
    if (it != introduced.end()) {
      str << '%' << it->second;
      printCustom(ref);
      return;
    }
    printUse(ref);
  }

  void introduce(FatDynObjRef<> ref) {
    DynObjRef noCustom = ref;
    noCustom.clearCustom();
    auto [it, succ] = introduced.try_emplace(noCustom, introduced.size());
    str << '%' << it->second << ":";
  }

  void introduceAndPrintDef(FatDynObjRef<> ref) {
    introduce(ref);
    printDef(ref);
  }

  void reset() { introduced.clear(); }

  void printBlock(BlockRef block) {
    str << "{\n";
    indentPrint.addIndent();
    for (auto it : block) {
      indentPrint.printIndent();
      printInstr(it);
    }
    indentPrint.removeIndent();
    if (!block.empty())
      indentPrint.printIndent();
    str << "}";
  }

  void printInstr(InstrRef instr) {
    printOpcode(instr);
    str << ' ';

    for (size_t i = 0; i < instr.getNumOperands(); i++) {
      auto ref = instr.operand(i)->fat();
      if (i < instr.getNumDefs()) {
        introduceAndPrintDef(ref);
      } else {
        printRefOrUse(ref);
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
