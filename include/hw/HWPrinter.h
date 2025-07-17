#pragma once
#include "aig/AIG.h"
#include "aig/AIGPrinter.h"
#include "aig/IDs.h"
#include "dyno/IDs.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Obj.h"
#include "dyno/Opcode.h"
#include "hw/DebugInfo.h"
#include "hw/HWAbstraction.h"
#include "hw/IDs.h"
#include "hw/Process.h"
#include "op/IDs.h"
#include <fstream>
#include <ostream>

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

  ValueNameInfo<Register> *regNames = nullptr;

public:
  HWPrinter(std::ostream &str)
      : Printer(str, dialectIs.data(), tyIs.data(), opcodeIs.data()) {
    setDefaultDialects({DialectID{DIALECT_CORE}, DialectID{DIALECT_OP},
                        DialectID{DIALECT_HW}});
    interfaces.registerVal<type::print_fn>(
        DIALECT_HW, static_cast<type::print_fn>(&HWPrinter::printHWType));
    interfaces.registerVal(
        DIALECT_AIG, static_cast<type::print_fn>(&HWPrinter::printAIGType));
    interfaces.registerVal(DIALECT_HW,
                           static_cast<name_fn>(&HWPrinter::getObjectName));
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
      // ProcessRef asProc = ref.as<ProcessRef>();
      str << "process";
      break;
    }
    case HW_TRIGGER.type: {
      str << "trigger";
      auto asTrigger = ref.as<TriggerRef>();
      if (asTrigger->size() != 0) {
        str << "(";
        for (size_t i = 0; i < asTrigger->size(); i++) {
          auto arr = std::array<const char *, 4>{"pos", "neg", "any", "iff"};
          str << arr[size_t(asTrigger->getMode(i))];
          if (i != asTrigger->size() - 1)
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

  const char *getObjectName(FatDynObjRef<> ref) {
    switch (ref.getTyID()) {
    case HW_MODULE.type:
      return ref.as<ModuleRef>()->name.c_str();
    case HW_REGISTER.type: {
      if (!regNames)
        return nullptr;
      auto range = regNames->getNames(ref.as<RegisterRef>());
      if (range.begin() == range.end())
        return nullptr;
      // todo: what about multiple and collisions?
      return *range.begin();
    }
    }
    return nullptr;
  }

  // todo: in AIG dialect
  bool printAIGType(FatDynObjRef<> ref, bool def) {
    switch (ref.getTyID()) {
    case AIG_AIG.type: {
      auto &asAIG = ref.as<AIGObjRef>()->aig;
      str << "aig(\n";
      {
        std::ofstream dot{"graph.dot"};
        AIGPrinter{ref.as<AIGObjRef>()}.dumpGraphviz(dot);
      }

      indentPrint.addIndent();

      for (auto obj : asAIG.gates()) {
        indentPrint.printIndent();
        auto printOperand = [&](AIGNodeTRef node) {
          if (node.invert())
            str << "!";
          if (node.isSpecial())
            printRefOrUse(asAIG.store.fat.resolve(node).nonInverted());
          else
            str << "$" << node.idx();
        };

        printOperand(obj);
        str << " = node ";
        printOperand(obj->op[0]);
        str << ", ";
        printOperand(obj->op[1]);
        str << "\n";
      }
      indentPrint.removeIndent();
      indentPrint.printIndent();
      str << ")";
      break;
    }
    default:
      return false;
    }
    return true;
  }

  void printCtx(HWContext &ctx) {
    sourceLocInfo = &ctx.sourceLocInfo;
    regNames = &ctx.regNameInfo;
    for (auto instr : ctx.getInstrs()) {
      if (InstrRef{instr}.isOpc(HW_MODULE_INSTR))
        printInstr(InstrRef{instr});
    }
    regNames = nullptr;
    sourceLocInfo = nullptr;
  }
};

void dumpCtx(HWContext &ctx);
void dumpInstr(InstrRef instr);
void dumpObj(FatDynObjRef<> obj);

}; // namespace dyno
