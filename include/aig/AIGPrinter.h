#pragma once

#include "aig/AIG.h"
#include <ostream>
namespace dyno {

class AIGPrinter {
  AIGObjRef aigObj;

public:
  void dumpGraphviz(std::ostream &os) {
    AIG &aig = aigObj->aig;

    SmallVec<AIGNodeRef, 4> inputs;
    SmallVec<AIGNodeRef, 4> outputs;

    for (auto use : aigObj->defUse.uses()) {
      auto instr = use.instr();
      if (instr.isOpc(AIG_INPUT)) {
        for (auto def : instr.defs())
          inputs.emplace_back(def->as<FatAIGNodeRef>().as<AIGNodeRef>());
      }
      if (instr.isOpc(AIG_OUTPUT)) {
        for (auto def : instr.defs().drop_front())
          outputs.emplace_back(def->as<FatAIGNodeRef>().as<AIGNodeRef>());
      }
    }

    std::print(os, "digraph aig {{");

    for (auto [idx, node] : Range{aig.store.thin}.enumerate()) {
      for (auto &op : node->op)
        std::print(os, "{} -> {}{}\n", (unsigned)op.idx(), idx,
                   op.invert() ? "[style=dotted]" : "");
    }

    for (auto [idx, out] : Range{outputs}.enumerate()) {
      std::print(os, "{} -> output_{}{}\n", (unsigned)out->op[0].idx(), idx,
                 out->op[0].invert() ? "[style=dotted]" : "");
    }

    std::print(os, "}}");
  }

public:
  explicit AIGPrinter(AIGObjRef aig) : aigObj(aig) {}
};

}; // namespace dyno
