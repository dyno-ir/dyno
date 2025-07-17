#pragma once
#include "aig/AIG.h"
#include "hw/HWContext.h"
#include <fstream>

namespace dyno {

class BLIF_Printer {
  HWContext &ctx;
  std::ostream &os;

  std::string getName(AIGObjID idLit) const {
    if (idLit.isSpecial())
      return "f" + std::to_string(idLit.idx() - AIGObjID::FAT_ID_START);
    return "n" + std::to_string(idLit.idx());
  }

  void printAnd(AIGObjID outLit, AIGObjID in1Lit, AIGObjID in2Lit) {
    os << ".names " << getName(in1Lit) << " " << getName(in2Lit) << " "
       << getName(outLit) << "\n";
    char c1 = (in1Lit.invert()) ? '0' : '1';
    char c2 = (in2Lit.invert()) ? '0' : '1';
    os << c1 << c2 << " 1\n";
  }

  void printBuf(AIGObjID inLit, const std::string &outName) {
    std::string inName = getName(inLit);
    os << ".names " << inName << " " << outName << "\n";
    char c = inLit.invert() ? '0' : '1';
    os << c << " 1\n";
  }

public:
  // Export the AIG as BLIF; modelName used in .model directive
  void print(AIGObjRef aigObj, const std::string &modelName = "mymodel") {
    auto &aig = aigObj->aig;

    os << ".model " << modelName << "\n";

    os << ".inputs";
    for (auto in : aig.inputs) {
      os << " " << getName(in.getObjID());
    }
    os << "\n";

    os << ".outputs";
    for (size_t i = 0; i < aig.outputs.size(); ++i)
      os << " o" << i;
    os << "\n";

    for (auto gate : aig.gates()) {
      printAnd(gate.getObjID(), gate->op[0], gate->op[1]);
    }

    for (auto [i, out] : Range{aig.outputs}.enumerate()) {
      printBuf(out->node.op[0], "o" + std::to_string(i));
    }

    os << ".end\n";
  }

  BLIF_Printer(HWContext &ctx, std::ostream &os) : ctx(ctx), os(os) {}
};

class AIGERPrinter {
  HWContext &ctx;
  std::ostream &os;

  struct AIGERMeta {
    uint32_t fatIDShift;
  };

public:
  AIGERMeta print(AIGObjRef aigObj) {
    auto &aig = aigObj->aig;

    // all logic we want to export is in thin AIG nodes.
    // fat AIG nodes are used to interact with the rest of the IR
    // (inputs and outputs).
    uint32_t thinIDs = aig.store.thin.numIDs();
    uint32_t fatIDs = aig.store.fat.numIDs();

    uint32_t numInputs = aig.inputs.size();
    uint32_t numOutputs = aig.outputs.size();

    std::print(os, "aag {} {} {} {} {}\n", (thinIDs + fatIDs - 2), numInputs, 0,
               numOutputs, aig.store.thin.size());

    AIGERMeta meta;
    meta.fatIDShift = AIGObjID::FAT_ID_START - thinIDs;

    auto adjustID = [&meta](AIGNodeTRef id) -> uint32_t {
      if (id.getObjID().isSpecial())
        return (id.getObjID().idx() - meta.fatIDShift) << 1 |
               id.getObjID().invert();
      return (id.getObjID().idx()) << 1 | id.getObjID().invert();
    };

    for (auto input : aig.inputs) {
      std::print(os, "{}\n", adjustID(input.as<AIGNodeTRef>()));
    }
    for (auto output : aig.outputs) {
      std::print(os, "{}\n", adjustID(output->node.op[0]));
    }
    for (auto gate : aig.gates()) {
      std::print(os, "{} {} {}\n", adjustID(gate), adjustID(gate->op[0]),
                 adjustID(gate->op[1]));
    }

    return meta;
  }

public:
  AIGERPrinter(HWContext &ctx, std::ostream &os) : ctx(ctx), os(os) {}
};

class ABCPass {
  HWContext &ctx;

  void runOnAIG(InstrRef aigInstr) {
    auto aigRef = aigInstr.def(0)->as<AIGObjRef>();
    // auto &aig = aigRef->aig;
    // std::ofstream aigerFile{"aiger.aag"};
    // AIGERPrinter aigPrint{ctx, aigerFile};
    // aigPrint.print(aigRef);
    std::ofstream blifFile{"aig.blif"};
    BLIF_Printer print{ctx, blifFile};
    print.print(aigRef);
  }

  void runOnProc(ProcessIRef proc) {
    for (auto instr : proc.block()) {
      if (instr.isOpc(AIG_GRAPH))
        runOnAIG(instr);
    }
  }

  void runOnModule(ModuleIRef module) {
    for (auto proc : module.procs()) {
      runOnProc(proc);
    }
  }

public:
  void run() {
    for (auto mod : ctx.getModules()) {
      runOnModule(mod.iref());
    }
  }
  explicit ABCPass(HWContext &ctx) : ctx(ctx) {}
};

}; // namespace dyno
