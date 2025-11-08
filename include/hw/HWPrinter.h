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
#include "hw/HWContext.h"
#include "hw/IDs.h"
#include "hw/Process.h"
#include "op/IDs.h"
#include "support/ArrayRef.h"
#include "support/TempBind.h"
#include <fstream>
#include <ostream>

namespace dyno {

class HWPrinter : public Printer {
public:
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

  // maybe make interfaces work with these instead --- requires reworking
  // InterfaceTraits though.
  static constexpr std::array<ArrayRef<TyInfo>, NUM_DIALECTS> typeInfoArrays{
#define HEADER
#define FOOTER
#define LAST
#define ADD_OP(x) DialectTraits<x>::tyInfo
#include "dyno/DialectIDs.inc"
  };
  static constexpr std::array<ArrayRef<OpcodeInfo>, NUM_DIALECTS>
      opcodeInfoArrays{
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
  TempBindPtr<ValueNameInfo<Register>> regNames;

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

  std::optional<IntroducedName> getObjectName(FatDynObjRef<> ref) {
    switch (ref.getTyID()) {
    case HW_MODULE.type:
      return ref.as<ModuleRef>()->name.c_str();
    case HW_REGISTER.type: {
      if (!regNames)
        return std::nullopt;
      auto range = regNames->getNames(ref.as<RegisterRef>());
      if (range.begin() == range.end())
        return std::nullopt;
      // todo: what about multiple and collisions?
      return *range.begin();
    }
      // case HW_WIRE.type: {
      //   // todo properly (or just not)
      //   return ref.getObjID() + 10000;
      // }
    }
    return std::nullopt;
  }

  // todo: in AIG dialect
  bool printAIGType(FatDynObjRef<> ref, bool def) {
    switch (ref.getTyID()) {
    case AIG_AIG.type: {
      auto &asAIG = ref.as<AIGObjRef>()->aig;
      str << "aig(\n";
      // {
      //   std::ofstream dot{"graph.dot"};
      //   AIGPrinter{ref.as<AIGObjRef>()}.dumpGraphviz(dot);
      // }

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
    case AIG_FAT_NODE.type: {
      FatAIGNodeRef node = ref.as<FatAIGNodeRef>();
      auto printOperand = [&](AIGNodeTRef node) {
        if (node.getObjID() == ObjID::invalid()) {
          str << "null";
          return;
        }
        if (node.invert())
          str << "!";
        str << "$" << node.idx();
      };

      str << "fat_node(";
      printOperand(node.as<AIGNodeTRef>());
      str << ", ";
      printOperand(node->node.op[0]);
      str << ", ";
      printOperand(node->node.op[1]);
      str << ")";
      break;
    }
    default:
      return false;
    }
    return true;
  }

  void printCtx(HWContext &ctx) {
    auto locTok = sourceLocInfo.bind(&ctx.sourceLocInfo);
    auto regTok = regNames.bind(&ctx.regNameInfo);
    for (auto mod : ctx.getModules()) {
      if (mod.iref().isOpc(HW_STDCELL_DEF))
        continue;
      printInstr(mod.iref());
    }
  }

  using Printer::printInstr;
  void printInstr(InstrRef instr, HWContext &ctx) {
    auto locTok = sourceLocInfo.bind(&ctx.sourceLocInfo);
    auto regTok = regNames.bind(&ctx.regNameInfo);
    printInstr(instr);
  }

  void printDeps(InstrRef instr, unsigned maxDepth = -1) {
    if (maxDepth)
      for (auto use : instr.others()) {
        if (!Operand::isDefUseOperand(*use))
          continue;
        printDeps(use->as<FatDynObjRef<InstrDefUse>>()->getSingleDef()->instr(),
                  maxDepth - 1);
      }
    printInstr(instr);
  }
  void printDeps(InstrRef instr, HWContext &ctx, unsigned maxDepth = -1) {
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

void dumpCtx(HWContext &ctx);
void dumpInstr(InstrRef instr);
void dumpInstr(InstrRef instr, HWContext &ctx);
void dumpDeps(InstrRef instr);
void dumpDeps(InstrRef instr, HWContext &ctx);
void dumpDeps(InstrRef instr, HWContext &ctx, unsigned depth);
void dumpObj(FatDynObjRef<> obj);

}; // namespace dyno
