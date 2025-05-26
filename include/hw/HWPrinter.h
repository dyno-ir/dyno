#pragma once
#include "dyno/IDs.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Obj.h"
#include "dyno/Opcode.h"
#include "hw/HWAbstraction.h"
#include "hw/IDs.h"
#include "hw/Process.h"
#include "op/IDs.h"

namespace dyno {

class HWPrinter : public Printer {
  static constexpr std::array<const DialectInfo *, NUM_DIALECTS> dialectIs{
#define HEADER
#define FOOTER
#define LAST
#define ADD_OP(x) &DialectTraits<x>::info
#include "dyno/DialectIDs.inc"
  };
  static constexpr std::array<const TyInfo *, NUM_DIALECTS> tyIs{
#define HEADER
#define FOOTER
#define LAST
#define ADD_OP(x) DialectTraits<x>::tyInfo
#include "dyno/DialectIDs.inc"
  };
  static constexpr std::array<const OpcodeInfo *, NUM_DIALECTS> opcodeIs{
#define HEADER
#define FOOTER
#define LAST
#define ADD_OP(x) DialectTraits<x>::opcInfo
#include "dyno/DialectIDs.inc"
  };

  Interface<DialectInfo> dialectI{dialectIs.data()};
  Interface<TyInfo> tyI{tyIs.data()};
  Interface<OpcodeInfo> opcI{opcodeIs.data()};

public:
  HWPrinter(std::ostream &str)
      : Printer(str, dialectIs.data(), tyIs.data(), opcodeIs.data()) {
    setDefaultDialects({DialectID{DIALECT_CORE}, DialectID{DIALECT_OP},
                        DialectID{DIALECT_HW}});
    interfaces.registerVal<type::print_fn>(
        DIALECT_HW, static_cast<type::print_fn>(&HWPrinter::printHWType));
  }

  bool printHWType(FatDynObjRef<> ref, bool def) {
    switch (ref.getTyID()) {
    case HW_WIRE.type: {
      WireRef asWire = ref.as<WireRef>();
      str << "wire";
      if (asWire->numBits)
        str << "(" << *asWire->numBits << ")";
      break;
    }
    case HW_MODULE.type: {
      ModuleRef asModule = ref.as<ModuleRef>();
      str << "module(\"" << asModule->name << "\")";
      break;
    }
    case HW_REGISTER.type: {
      RegisterRef asReg = ref.as<RegisterRef>();
      str << "register";
      if (asReg->numBits) {
        str << "(" << *asReg->numBits << ")";
      }
      break;
    }
    case HW_PROCESS.type: {
      ProcessRef asProc = ref.as<ProcessRef>();
      str << "process";
      if (!asProc->modes.empty()) {
        str << "(";
        for (const auto &mode : asProc->modes) {
          auto arr = std::array<const char *, 4>{"pos", "neg", "any", "iff"};
          str << arr.at(mode);
          if (&mode != &asProc->modes.back())
            str << ", ";
        }
        str << ")";
      }
      break;
    }
    default:
      return false;
    }
    return true;
  }

  void printCtx(HWContext &ctx) {
    for (auto instr : ctx.getInstrs()) {
      if (InstrRef{instr}.isOpc(HW_MODULE_INSTR))
        printInstr(InstrRef{instr});
    }
  }
};

inline void dumpCtx(HWContext &ctx) { HWPrinter{std::cerr}.printCtx(ctx); }

inline void dumpInstr(InstrRef instr) {
  HWPrinter{std::cerr}.printInstr(instr);
}

inline void dumpObj(FatDynObjRef<> obj) {
  if (auto asInstr = obj.dyn_as<InstrRef>())
    return dumpInstr(asInstr);
  HWPrinter{std::cerr}.printDef(obj);
}

}; // namespace dyno
