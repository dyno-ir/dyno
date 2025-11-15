#pragma once

#include "aig/AIG.h"
#include "support/Debug.h"
#include <format>
#include <functional>
#include <ostream>

namespace dyno {

class AIGDotPrinter {
  std::ostream &os;

  std::string formatName(AIGNodeTRef node) {
    if (!node)
      return "null";
    if (node.isSpecial())
      return std::format("fat{}", node.idx().get() - AIGObjID::FAT_ID_START);
    return std::format("{}", node.idx().get());
  }

  void dumpEdge(AIGNodeTRef a, AIGNodeTRef b) {
    std::print(os, "{} -> {}{}\n", formatName(a), formatName(b),
               a.invert() ? "[style=dotted]" : "");
  }

public:
  void print(
      AIG &aig,
      std::function<void(std::ostream &os, AIGNodeTRef)> labelFunc = nullptr) {
    os << "digraph aig {\n";
    os << "rankdir=LR\n";
    os << "node [shape=box]\n";
    for (auto node : aig.gates()) {
      os << formatName(node);
      if (labelFunc) {
        os << " [label=\"";
        labelFunc(os, node);
        os << "\"]";
      }
      os << "\n";
    }
    os << "node [shape=doublecircle]\n";
    // for (auto node : aig.outputs) {
    //   dumpEdge(node.as<AIGNodeRef>()[0], node.as<AIGNodeTRef>());
    //   os << formatName(node.as<AIGNodeTRef>()) << " [label=\""; os <<
    //   "\"]\n";
    // }
    for (auto node : aig.gates()) {
      for (auto op : node)
        dumpEdge(op, node);
    }
    for (auto node : aig.outputs) {
      dumpEdge(node.as<AIGNodeRef>()[0], node.as<AIGNodeTRef>());
    }
    os << "}\n";
  }

public:
  explicit AIGDotPrinter(std::ostream &os) : os(os) {}
};

}; // namespace dyno
