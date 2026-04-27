#pragma once

#include "DialectInfo.h"
#include "dyno/CFG.h"
#include "dyno/Constant.h"
#include "dyno/DialectInfo.h"
#include "dyno/IDs.h"
#include "dyno/Obj.h"
#include "dyno/Symbol.h"
#include "hw/DebugInfo.h"
#include "hw/HWContext.h"
#include "support/CallableRef.h"
#include "support/DenseMap.h"
#include "support/RTTI.h"
#include "support/TempBind.h"
#include "support/Tuple.h"
#include "support/Utility.h"
#include <array>
#include <dyno/Instr.h>
#include <dyno/Interface.h>
#include <initializer_list>
#include <iostream>
#include <ostream>

namespace dyno {

struct OStreamWrapper {
  std::ostream *os = nullptr;

  template <typename T> OStreamWrapper &operator<<(const T &t) {
    *os << t;
    return *this;
  }

  operator std::ostream &() { return *os; }
  OStreamWrapper() = default;
  OStreamWrapper(std::ostream &os) : os(&os) {};
};

class IndentPrinter {
public:
  OStreamWrapper str;

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

  IndentPrinter(OStreamWrapper str) : str(str) {}
  IndentPrinter() : str() {}
};

class PrinterBase {
public:
  struct IntroducedName : public RTTIUtilMixin<IntroducedName> {
    enum Type : uint8_t { NUMERIC, STRING };
    Type type;

    std::string str() const {
      switch (type) {
      case NUMERIC:
        return std::format("{}{}", this->storage.numeric.prefix.data(),
                           this->storage.numeric.num);
      case STRING:
        return this->storage.string;
      }
      dyno_unreachable("unknown type");
    }

    IntroducedName() = default;

    IntroducedName(uint32_t numeric, std::array<char, 4> prefix)
        : type(NUMERIC), storage{.numeric = {{prefix}, numeric}} {}
    IntroducedName(uint32_t numeric)
        : type(NUMERIC),
          storage{.numeric = {{'\0', '\0', '\0', '\0'}, numeric}} {}
    IntroducedName(const char *string)
        : type(STRING), storage{.string = string} {}

  protected:
    union {
      struct {
        std::array<char, 4> prefix;
        uint32_t num;
      } numeric;
      const char *string;
    } storage;
  };

private:
  DenseMap<DynObjRef, IntroducedName> introduced;
  uint32_t numericNameCnt = 0;

  UnsizedBitSet<SmallVec<uint64_t, 2>> isDefault = (MAX_NUM_DIALECTS);

protected:
  TempBindPtr<SourceLocInfo<Instr>> sourceLocInfo;

public:
  IndentPrinter indentPrint;
  Interface<DialectInfo> dialectI;
  Interface<TyInfo> tyI;
  Interface<OpcodeInfo> opcodeI;
  // may be unset, printers must be able to handle both.
  // can print with more fidelity when set though.
  Context *ctx = nullptr;

public:
  struct type {
    using print_fn = CallableRef<bool(FatDynObjRef<>, bool)>;
  };
  struct opc {
    using print_fn = CallableRef<bool(FatDynObjRef<>, bool)>;
  };
  using name_fn = CallableRef<std::optional<IntroducedName>(FatDynObjRef<>)>;
  Interfaces<NUM_DIALECTS, type::print_fn, opc::print_fn, name_fn> interfaces;

  OStreamWrapper str;

  PrinterBase(OStreamWrapper str, Interface<DialectInfo> dialectI,
              Interface<TyInfo> tyI, Interface<OpcodeInfo> opcodeI)
      : indentPrint(str), dialectI(dialectI), tyI(tyI), opcodeI(opcodeI),
        str(str) {}
  PrinterBase(Interface<DialectInfo> dialectI, Interface<TyInfo> tyI,
              Interface<OpcodeInfo> opcodeI)
      : indentPrint(), dialectI(dialectI), tyI(tyI), opcodeI(opcodeI), str() {}

  void printTypeDefault(DynObjRef ref) {
    if (!isDefault[ref.getDialectID()]) {
      str << dialectI[ref].name << ".";
    }
    str << tyI[ref].name;
  }
  void printTypeDefault(DialectType type) {
    if (!isDefault[type.getDialectID()]) {
      str << dialectI[type.getDialectID()]->name << ".";
    }
    str << tyI[type.getDialectID()][type.getTypeID()].name;
  }

  void printOpcodeDefault(InstrRef ref) {
    if (!isDefault[ref.getDialect()]) {
      str << dialectI[ref.getDialect()]->name << ".";
    }
    str << opcodeI[ref].name;
  }
  void printOpcodeDefault(DialectOpcode opc) {
    if (!isDefault[opc.getDialectID()]) {
      str << dialectI[opc.getDialectID()]->name << ".";
    }
    str << opcodeI[opc.getDialectID()][opc.getOpcodeID()].name;
  }

  void setDefaultDialects(std::initializer_list<DialectID> dialects) {
    for (auto dial : dialects)
      isDefault[dial] = 1;
  }

  void printUse(FatDynObjRef<> ref) {
    if (auto func = interfaces.getVal<type::print_fn>(ref.getDialectID())) {
      if (func(ref, false))
        return;
    }
    printTypeDefault(ref);
    // str << '[' << ref.getObjID() << "]";
    // printCustom(ref);
  }

  void printDef(FatDynObjRef<> ref) {
    if (auto func = interfaces.getVal<type::print_fn>(ref.getDialectID())) {
      if (func(ref, true))
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
        if (auto nm = func(ref))
          return *nm;
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

    if (Operand::isDefUseOperand(ref)) {
      auto [found, name] = introduceNameFor(ref);
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

  bool introduce(FatDynObjRef<> ref) {
    auto [found, name] = introduceNameFor(ref);
    str << '%' << name.str() << ":";
    return found;
  }

  void introduceAndPrintDef(FatDynObjRef<> ref) {
    bool found = false;
    if (1 || !Operand::isDefUseOperand(ref) ||
        ref.as<FatDynObjRef<InstrDefUse>>()->getNumUses() != 0) {
      found = introduce(ref);
    } else {
      str << ":";
    }
    if (!found)
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
        str << "  [";
      else
        str << ", ";
      if (loc.beginLine == loc.endLine) {
        std::print(str, "\"{}:{}:{}-{}\"", loc.fileName, loc.beginLine,
                   loc.beginCol, loc.endCol);
      } else {
        std::print(str, "\"{}:{}.{}-{}.{}\"", loc.fileName, loc.beginLine,
                   loc.beginCol, loc.endLine, loc.endCol);
      }
    }
    if (any)
      str << "]";
  }

  void printInstr(InstrRef instr, bool trailingNewline = true,
                  bool expandBlocks = true) {
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
        if (expandBlocks)
          printBlock(asBlock);
      }
    }

    tryPrintSrcLoc(instr);

    if (trailingNewline)
      str << '\n';
  }

  std::string toString(InstrRef instr) {
    auto backup = str;
    std::stringstream stringstr;
    indentPrint.str = stringstr;
    str = stringstr;
    printInstr(instr, false);
    indentPrint.str = backup;
    str = backup;
    return std::move(stringstr).str();
  }
  std::string toString(DialectOpcode opc) {
    auto backup = str;
    std::stringstream stringstr;
    indentPrint.str = stringstr;
    str = stringstr;
    printOpcodeDefault(opc);
    indentPrint.str = backup;
    str = backup;
    return std::move(stringstr).str();
  }
};

class Printer : public PrinterBase {
protected:
  using PrinterBase::opc;
  using PrinterBase::type;

public:
  Printer(OStreamWrapper str, Interface<DialectInfo> dialectI,
          Interface<TyInfo> tyI, Interface<OpcodeInfo> opcodeI)
      : PrinterBase(str, dialectI, tyI, opcodeI) {}
};

// with context, uses context's info (including dynamic)
template <typename... Printers>
class ContextPrinterWrapper : public PrinterBase {

protected:
  Tuple<Printers...> printers;

public:
  ContextPrinterWrapper(Context &ctx, OStreamWrapper str)
      : PrinterBase(
            str,
            Interface<DialectInfo>{ctx.getDialectInfos().dialectInfoArr.data()},
            Interface<TyInfo>{ctx.getDialectInfos().typeInfoArr.data()},
            Interface<OpcodeInfo>{ctx.getDialectInfos().opcodeInfoArr.data()}),
        printers{(static_cast<void>(sizeof(Printers)), this)...} {}

  ContextPrinterWrapper(Context &ctx)
      : PrinterBase(
            Interface<DialectInfo>{ctx.getDialectInfos().dialectInfoArr.data()},
            Interface<TyInfo>{ctx.getDialectInfos().typeInfoArr.data()},
            Interface<OpcodeInfo>{ctx.getDialectInfos().opcodeInfoArr.data()}),
        printers{(static_cast<void>(sizeof(Printers)), this)...} {}
};

// context-less, carries own info
template <typename... Printers>
class PrinterWrapper : private AutoDialectInfos<Printers::dialect...>,
                       public PrinterBase {

protected:
  Tuple<Printers...> printers;

public:
  PrinterWrapper(OStreamWrapper str)
      : PrinterBase(str,
                    Interface<DialectInfo>{this->infos.dialectInfoArr.data()},
                    Interface<TyInfo>{this->infos.typeInfoArr.data()},
                    Interface<OpcodeInfo>{this->infos.opcodeInfoArr.data()}),
        printers{(static_cast<void>(sizeof(Printers)), this)...} {}
};

class CoreDialectPrinter {
  PrinterBase &base;

public:
  constexpr static DialectID dialect{DIALECT_CORE};

  CoreDialectPrinter(PrinterBase *base) : base(*base) {
    base->interfaces.registerVal<PrinterBase::type::print_fn>(
        DIALECT_CORE,
        CallableRef{this, &BindMethod<&CoreDialectPrinter::printTypeCore>::fv});
    base->interfaces.registerVal<PrinterBase::name_fn>(
        DIALECT_CORE,
        CallableRef{this, &BindMethod<&CoreDialectPrinter::getNameCore>::fv});
  }

  std::optional<PrinterBase::IntroducedName> getNameCore(FatDynObjRef<> ref) {
    switch (ref.getTyID()) {
    case CORE_SYMBOL.type: {
      return PrinterBase::IntroducedName{ref.as<SymbolRef>()->name.c_str()};
    }
    default:
      return std::nullopt;
    }
  }

  bool printTypeCore(FatDynObjRef<> ref, bool def) {
    switch (ref.getTyID()) {
    case CORE_CONSTANT.type: {
      base.str << '#' << ref.as<ConstantRef>();
      return true;
    }
    case CORE_SYMBOL.type: {
      auto asSymb = ref.as<SymbolRef>();
      base.str << "symbol";
      if (!def || asSymb->type) {
        std::print(base.str, "(\"{}\"", asSymb->name);
        if (asSymb->type) {
          std::print(base.str, ", ");
          base.printTypeDefault(*asSymb->type);
        }
        std::print(base.str, ")");
      }
      return true;
    }
    default:
      return false;
    }
  }
};

} // namespace dyno
