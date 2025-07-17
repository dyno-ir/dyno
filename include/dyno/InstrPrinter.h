#pragma once

#include "dyno/CFG.h"
#include "dyno/Constant.h"
#include "dyno/DialectInfo.h"
#include "dyno/IDs.h"
#include "dyno/Obj.h"
#include "hw/DebugInfo.h"
#include "support/DenseMap.h"
#include "support/RTTI.h"
#include "support/Utility.h"
#include <dyno/Instr.h>
#include <dyno/Interface.h>
#include <initializer_list>
#include <iostream>
#include <ostream>

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

class PrinterBase {

  struct IntroducedName : public RTTIUtilMixin<IntroducedName> {
    enum Type : uint8_t { NUMERIC, STRING };
    Type type;

    std::string str() const {
      switch (type) {
      case NUMERIC:
        return std::to_string(this->storage.numeric);
      case STRING:
        return this->storage.string;
      }
      dyno_unreachable("unknown type");
    }

    IntroducedName() = default;

    IntroducedName(uint32_t numeric)
        : type(NUMERIC), storage{.numeric = numeric} {}
    IntroducedName(const char *string)
        : type(STRING), storage{.string = string} {}

  protected:
    union {
      uint32_t numeric;
      const char *string;
    } storage;
  };

  DenseMap<DynObjRef, IntroducedName> introduced;
  uint32_t numericNameCnt = 0;

  std::vector<bool> isDefault = std::vector<bool>(NUM_DIALECTS);

protected:
  IndentPrinter indentPrint;
  SourceLocInfo<Instr> *sourceLocInfo = nullptr;

public:
  Interface<DialectInfo> dialectI;
  Interface<TyInfo> tyI;
  Interface<OpcodeInfo> opcodeI;

protected:
  struct type {
    using print_fn = bool (PrinterBase::*)(FatDynObjRef<> ref, bool def);
  };
  struct opc {
    using print_fn = bool (PrinterBase::*)(FatDynObjRef<> ref, bool def);
  };
  using name_fn = const char *(PrinterBase::*)(FatDynObjRef<> ref);
  Interfaces<NUM_DIALECTS, type::print_fn, opc::print_fn, name_fn> interfaces;

public:
  std::ostream &str;

  PrinterBase(std::ostream &str, Interface<DialectInfo> dialectI,
              Interface<TyInfo> tyI, Interface<OpcodeInfo> opcodeI)
      : indentPrint(str), dialectI(dialectI), tyI(tyI), opcodeI(opcodeI),
        str(str) {}

  void printTypeDefault(DynObjRef ref) {
    if (!isDefault[ref.getDialectID()]) {
      str << dialectI[ref].name << ".";
    }
    str << tyI[ref].name;
  }

  void printOpcodeDefault(InstrRef ref) {
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
    if (auto func = interfaces.getVal<type::print_fn>(ref.getDialectID())) {
      if ((this->*func)(ref, false))
        return;
    }
    printTypeDefault(ref);
    // str << '[' << ref.getObjID() << "]";
    // printCustom(ref);
  }

  void printDef(FatDynObjRef<> ref) {
    if (auto func = interfaces.getVal<type::print_fn>(ref.getDialectID())) {
      if ((this->*func)(ref, true))
        return;
    }
    printTypeDefault(ref);
  }

  void printCustom(FatDynObjRef<> ref) {
    if (!ref.isCustom())
      return;
    str << '(' << ref.getCustom() << ')';
  }

  std::pair<bool, IntroducedName &> introduceNameFor(FatDynObjRef<> ref) {
    DynObjRef noCustom = ref;
    noCustom.clearCustom();
    auto [found, it] = introduced.findOrInsert(noCustom, [&] -> IntroducedName {
      if (auto func = interfaces.getVal<name_fn>(ref.getDialectID())) {
        if (const char *name = (this->*func)(ref))
          return IntroducedName{name};
      }
      return IntroducedName{numericNameCnt++};
    });
    return {found, it.val()};
  }


  void printRefOrUse(FatDynObjRef<> ref) {
    if (ref.getObjID() == ObjID::invalid() && !ref.getCustom()) {
      str << "nullref";
      return;
    }

    auto [found, name] = introduceNameFor(ref);

    if (Operand::isDefUseOperand(ref)) {
      str << '%' << name.str();
      if (!found) {
        str << ":?";
        printUse(ref);
      }
      return;
    }

    DynObjRef noCustom = ref;
    noCustom.clearCustom();
    auto it = introduced.find(noCustom);
    if (it) {
      str << '%' << it.val().str();
    } else {
      printUse(ref);
    }
  }

  void introduce(FatDynObjRef<> ref) {
    auto [found, name] = introduceNameFor(ref);
    str << '%' << name.str() << ":";
  }

  void introduceAndPrintDef(FatDynObjRef<> ref) {
    introduce(ref);
    printDef(ref);
  }

  void reset() { introduced.clear(); }

  void printBlock(BlockRef block) {
    if (block.empty()) {
      str << "{}";
      return;
    }
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

  void tryPrintSrcLoc(ObjRef<Instr> instr) {
    if (!sourceLocInfo)
      return;
    bool any = false;
    for (auto [i, loc] :
         Range{sourceLocInfo->getSourceLocs(instr)}.enumerate()) {
      any = true;
      if (i == 0)
        str << "// ";
      else
        str << ", ";
      if (loc.beginLine == loc.endLine) {
        std::print(str, "{}:{}:{}-{}", loc.fileName, loc.beginLine,
                   loc.beginCol, loc.endCol);
      } else {
        std::print(str, "{}:{}.{}-{}.{}", loc.fileName, loc.beginLine,
                   loc.beginCol, loc.endLine, loc.endCol);
      }
    }
    if (any) {
      str << "\n";
      indentPrint.printIndent();
    }
  }

  void printInstr(InstrRef instr) {
    tryPrintSrcLoc(instr);
    printOpcodeDefault(instr);
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

class Printer : public PrinterBase {
protected:
  using PrinterBase::opc;
  using PrinterBase::type;

public:
  Printer(std::ostream &str, Interface<DialectInfo> dialectI,
          Interface<TyInfo> tyI, Interface<OpcodeInfo> opcodeI)
      : PrinterBase(str, dialectI, tyI, opcodeI) {
    interfaces.registerVal<type::print_fn>(
        DIALECT_CORE, static_cast<type::print_fn>(&Printer::printTypeCore));
  }

  bool printTypeCore(FatDynObjRef<> ref, bool def) {
    switch (ref.getTyID()) {
    case CORE_CONSTANT.type: {
      str << '#' << ref.as<ConstantRef>();
      return true;
    }
    default:
      return false;
    }
  }
};

} // namespace dyno
