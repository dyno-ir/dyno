#pragma once

#include "aig/AIG.h"
#include "aig/IDs.h"
#include "dyno/DialectInfo.h"
#include "dyno/IDImpl.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Obj.h"
#include "dyno/Parser.h"
namespace dyno {

class AIGDialectPrinter {
public:
  static constexpr DialectID dialect{DIALECT_AIG};
  PrinterBase *base;

  AIGDialectPrinter(PrinterBase *base) : base(base) {
    base->interfaces.registerVal<PrinterBase::type::print_fn>(
        DIALECT_AIG,
        MemberRef{this, BindMethod<&AIGDialectPrinter::printAIGType>::fv});
  }

  bool printAIGType(FatDynObjRef<> ref, bool def) {
    auto &str = base->str;

    switch (ref.getTyID()) {
    case AIG_AIG.type: {
      auto &asAIG = ref.as<AIGObjRef>()->aig;
      str << "aig(\n";

      base->indentPrint.addIndent();

      for (auto obj : asAIG.gates()) {
        base->indentPrint.printIndent();
        auto printOperand = [&](AIGNodeTRef node) {
          if (node.invert())
            str << "!";
          if (node.isSpecial())
            base->printRefOrUse(asAIG[node].nonInverted().as<FatAIGNodeRef>());
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
      base->indentPrint.removeIndent();
      base->indentPrint.printIndent();
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
};

class AIGDialectParser {
  ParserBase *base;

public:
  static constexpr DialectID dialect{DIALECT_AIG};
  explicit AIGDialectParser(ParserBase *base) : base(base) {}

  FatDynObjRef<> parseAIG(DialectType type, ArrayRef<char> name) {
    (void)base;
    return nullref;
  }
};

}; // namespace dyno
