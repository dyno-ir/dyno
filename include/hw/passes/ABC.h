#pragma once
#include "aig/AIG.h"
#include "dyno/Constant.h"
#include "dyno/DestroyMap.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Process.h"
#include "support/ErrorRecovery.h"
#include "support/Utility.h"
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

class BLIF_Parser {
  HWContext &ctx;
  std::istream &is;

  template <char Delim = ' '> class SplitIterator {
    const char *ptr;
    size_t len;

    void prime() {
      while (ptr[len] != Delim && ptr[len] != '\0')
        len++;
    }

  public:
    SplitIterator(const char *ptr) : ptr(ptr), len(0) { prime(); }

    SplitIterator &operator++() {
      if (ptr[len] == '\0')
        ptr += len;
      else {
        ptr += len + 1;
        while (*ptr == Delim)
          ++ptr;
      }
      len = 0;
      prime();

      return *this;
    }

    SplitIterator operator++(int) {
      auto tmp{*this};
      ++*this;
      return tmp;
    }

    friend bool operator==(SplitIterator lhs, SplitIterator rhs) {
      return lhs.ptr == rhs.ptr;
    }

    std::string_view operator*() const { return std::string_view{ptr, len}; }
  };

  auto split(const std::string &str) {
    auto begin = SplitIterator<' '>{str.begin().base()};
    auto end = SplitIterator<' '>{str.end().base()};
    return Range{begin, end};
  }

public:
  BLIF_Parser(HWContext &ctx, std::istream &is) : ctx(ctx), is(is) {}

  /*
    Imports techmapped BLIF. Each gate turns into a standard cell instance.
    Using instances implies we're using dyno Registers to communicate between
    instances, which gets weird at times. Alternative would be creating a new
    type of instance that uses wires and lives inside the processes. Also not
    and ideal solution though.
  */
  void parse(AIGObjRef aigObj) {
    std::string line;
    std::unordered_map<std::string, HWValue> names;
    std::unordered_map<std::string, ModuleRef> modules;

    for (auto mod : ctx.getModules()) {
      modules[mod->name] = mod;
    }

    HWInstrBuilder build{ctx};

    while (std::getline(is, line), !line.empty()) {
      while (line.ends_with('\\')) {
        std::string rem;
        std::getline(is, rem);
        line = line.substr(0, line.size() - 1) + rem;
      }

      if (line.starts_with(".inputs")) {
        for (auto [i, tok] : split(line).drop_front().enumerate()) {
          auto def = *aigObj->aig.inputs[i]->defUse.getSingleDef();
          auto defI = def.instr();
          build.setInsertPoint(defI);
          assert(defI.isOpc(AIG_INPUT));
          // def is an AIG input
          auto inputVal = defI.other(0)->as<HWValue>();
          unsigned index = def - defI.def_begin();

          // create reg with copy of the val
          names.insert(
              std::make_pair(tok, build.buildSplice(inputVal, 1, index)));
        }
      }
      if (line.starts_with(".outputs")) {
        SmallVec<HWValue, 4> outputBitArr;
        InstrRef lastDefI = nullref;

        auto makeConcat = [&]() {
          std::reverse(outputBitArr.begin(), outputBitArr.end());
          auto newVal = build.buildConcat(outputBitArr);
          assert(lastDefI.def(0)->as<WireRef>()->numBits ==
                 outputBitArr.size());
          lastDefI.def(0)->as<WireRef>().replaceAllUsesWith(newVal);
          outputBitArr.clear();
        };

        for (auto [i, tok] : split(line).drop_front().enumerate()) {
          auto def = *aigObj->aig.outputs[i]->defUse.getSingleDef();
          auto defI = def.instr();
          if (lastDefI != defI) {
            if (lastDefI)
              makeConcat();
            lastDefI = defI;
            build.setInsertPoint(defI);
          }

          assert(defI.isOpc(AIG_OUTPUT));
          auto wire = ctx.getWires().create(1);
          outputBitArr.emplace_back(wire);

          names.insert(std::make_pair(tok, wire));
        }
        makeConcat();
        assert(outputBitArr.size() == 0);
      }

      if (line.starts_with(".gate")) {
        auto aigInstr = aigObj->defUse.getSingleDef()->instr();
        build.setInsertPoint(HWInstrRef{aigInstr}.parentBlock(ctx).end());

        SmallVec<WireRef, 4> defs;
        SmallVec<WireRef, 8> uses;

        ModuleRef mod = nullref;
        Optional<unsigned> constVal = nullopt;
        for (auto [i, tok] : split(line).drop_front().enumerate()) {
          if (i == 0) {
            if (tok == "_const0_")
              constVal = 0;
            else if (tok == "_const1_")
              constVal = 1;
            else
              mod = modules.find(std::string(tok))->second;
            continue;
          }
          auto eqIdx = tok.find('=');
          if (eqIdx == std::string::npos)
            report_fatal_error("BLIF format");
          auto tokStr = std::string(tok.begin() + eqIdx + 1, tok.end());
          auto wire = names.find(tokStr);

          if (constVal) {
            assert(i == 1);
            if (wire == names.end()) {
              names.insert(
                  std::make_pair(tokStr, ConstantRef::fromBool(*constVal)));
            } else {
              if (auto asConst = wire->second.dyn_as<ConstantRef>()) {
                if (asConst != ConstantRef::fromBool(*constVal))
                  report_fatal_error("contradictory BLIF constant value");

              } else {
                wire->second.as<WireRef>().replaceAllUsesWith(
                    ConstantRef::fromBool(*constVal));
                ctx.getWires().destroy(wire->second.as<WireRef>());
                wire->second = ConstantRef::fromBool(*constVal);
              }
            }
          } else if (wire == names.end()) {
            wire =
                names.insert(std::make_pair(tokStr, ctx.getWires().create(1)))
                    .first;
          }

          if (mod) {
            if (mod->ports[i - 1].portType.is(HW_INPUT_REGISTER_DEF))
              uses.emplace_back(wire->second);
            else if (mod->ports[i - 1].portType.is(HW_OUTPUT_REGISTER_DEF))
              defs.emplace_back(wire->second);
            else
              dyno_unreachable("unexpected port type");
          }
        }
        if (mod) {
          auto ib = build.buildInstrRaw(HW_STDCELL_INSTANCE,
                                        1 + defs.size() + uses.size());
          ib.addRefs(defs).other().addRef(mod).addRefs(uses);
        }
      }
    }
  }
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
  DestroyMap<Instr> destroyMap;

  void runOnAIG(InstrRef aigInstr) {
    auto aigRef = aigInstr.def(0)->as<AIGObjRef>();
    // auto &aig = aigRef->aig;
    // std::ofstream aigerFile{"aiger.aag"};
    // AIGERPrinter aigPrint{ctx, aigerFile};
    // aigPrint.print(aigRef);
    {
      std::ofstream blifFile{"aig.blif"};
      BLIF_Printer print{ctx, blifFile};
      print.print(aigRef);
    }
    system(("yosys-abc -q \"read_blif aig.blif; read_lib -X sky130_fd_sc_hd__lpflow_inputiso1p_1 -X sky130_fd_sc_hd__lpflow_isobufsrc_1 -X sky130_fd_sc_hd__clkinv_1 -w " + config.path +
            "; strash; &get -n; &fraig -x; "
            "&put; scorr; dc2; dretime; strash; &get -n; &dch -f; &nf; &put;"
            "print_stats; write_blif "
            "mapped.blif\"")
               .c_str());

    // system("yosys-abc -q \"read_blif aig.blif; read_library "
    //        "stdcells.genlib; strash; &get -n; &fraig -x; "
    //        "&put; scorr; dc2; dretime; strash; &get -n; &dch -f; &nf; &put;"
    //        "print_stats; write_blif "
    //        "mapped_gen.blif\"");

    std::ifstream mappedFile{"mapped.blif"};
    BLIF_Parser parse{ctx, mappedFile};
    parse.parse(aigRef);
  }

  void runOnProc(ProcessIRef proc) {
    for (auto instr : proc.block()) {
      if (instr.isOpc(AIG_GRAPH))
        runOnAIG(instr);
    }

    HWInstrBuilder build{ctx};
    build.setInsertPoint(proc);
    auto ib = build.buildInstrRaw(HW_NETLIST_PROCESS_DEF, 2);
    ib.addRef(proc.proc()).addRef(proc.block());
    proc.def(0).replace(FatDynObjRef<>{nullref});
    proc.def(1).replace(FatDynObjRef<>{nullref});
    destroyMap.mark(proc);
  }

  void runOnModule(ModuleIRef module) {
    for (auto proc : module.procs()) {
      runOnProc(proc);
    }
  }

public:
  struct Config {
    std::string path;
  };
  Config config;
  void run() {
    destroyMap.clear();
    destroyMap.resize(ctx.getInstrs().numIDs());

    auto tok = destroyMap.registerCreateHook(ctx.getInstrs());
    for (auto mod : ctx.activeModules()) {
      runOnModule(mod.iref());
    }

    destroyMap.apply(ctx.getInstrs(), [&](InstrRef ref) {
      HWInstrBuilder{ctx}.destroyInstr(ref);
    });
  }
  explicit ABCPass(HWContext &ctx) : ctx(ctx) {}
};

}; // namespace dyno
