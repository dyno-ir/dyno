#pragma once

#include "dyno/Context.h"
#include "dyno/ObjMap.h"
#include "dyno/Pass.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Module.h"
#include "op/IDs.h"
#include "support/ErrorRecovery.h"
#include "support/Utility.h"
#include <print>
#include <tuple>
namespace dyno {

class void_stream final : public std::ostream {
public:
  void_stream() noexcept : std::ostream(&os_buffer_) {}

private:
  class void_stream_buffer final : public std::streambuf {
  protected:
    [[nodiscard]] auto overflow(int_type ch) noexcept -> int_type override {
      return ch;
    }
  };
  void_stream_buffer os_buffer_{};
};

class DumpVerilogPass : public Pass<DumpVerilogPass> {
  Context &ctx;
  std::ostream &os;

  void_stream voidStr;
  HWPrinter print;

#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM) FIELD(bool, dumpWiresLast, true)
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
  Config config;

  // Adapter for printer's regular IntroducedName, only overrides str()
  struct VerilogIntroducedName : public Printer::IntroducedName {
    using Printer::IntroducedName::IntroducedName;
    VerilogIntroducedName(Printer::IntroducedName base)
        : Printer::IntroducedName(base) {}
    std::string str() const {
      switch (type) {
      case NUMERIC:
        return "_r" +
               std::format("{}{}", this->storage.numeric.prefix.data(),
                           this->storage.numeric.num) +
               "_";
      case STRING:
        return this->storage.string;
      }
      dyno_unreachable("unknown type");
    }
  };

  void dumpNetlistProcess(ProcessIRef proc) {
    ObjMapVec<Wire, Optional<uint32_t>> wireMap;
    wireMap.resize(ctx.getStore<Wire>().numIDs());
    uint32_t wireIdCnt = 0;
    auto wireToID = [&](WireRef wire) {
      auto &entry = wireMap[wire];
      if (entry)
        return *entry;
      wireMap[wire] = wireIdCnt;
      return wireIdCnt++;
    };

    auto dumpWires = [&]() {
      for (auto [obj, id] : wireMap) {
        if (!id || !ctx.getStore<Wire>().exists(obj))
          continue;
        auto wire = ctx.getStore<Wire>().resolve(obj);
        std::print(os, "wire[{}:0] _w{}_;\n", *wire.getNumBits() - 1, *id);
      }
    };
    if (!config.dumpWiresLast)
      dumpWires();

    uint32_t instanceIDCnt = 0;

    for (auto instr : proc.block()) {
      switch (*instr.getDialectOpcode()) {
      case *HW_LOAD: {
        auto asLoad = instr.as<LoadIRef>();
        std::print(
            os, "assign _w{}_ = {}", wireToID(asLoad.value()),
            VerilogIntroducedName{print.introduceNameFor(asLoad.reg()).second}
                .str());
        if (!asLoad.isFullReg()) {
          assert(asLoad.isConstantOffs());
          auto addr = asLoad.getBase();
          auto len = asLoad.getLen();
          std::print(os, " [{}:{}]", addr + len - 1, addr);
        }
        std::print(os, ";\n");
        break;
      }
      case *HW_STORE: {
        auto asStore = instr.as<StoreIRef>();
        // fixme: what about constant stores?
        std::print(
            os, "assign {}",
            VerilogIntroducedName{print.introduceNameFor(asStore.reg()).second}
                .str());
        if (!asStore.isFullReg()) {
          assert(asStore.isConstantOffs());
          auto addr = asStore.getBase();
          auto len = asStore.getLen();
          std::print(os, " [{}:{}]", addr + len - 1, addr);
        }
        std::print(os, " = _w{}_;\n", wireToID(asStore.value().as<WireRef>()));
        break;
      }

      case *HW_SPLICE: {
        auto asSplice = instr.as<SpliceIRef>();
        assert(asSplice.isConstantOffs());
        std::print(os,
                   "assign _w{}_ = ", wireToID(instr.def(0)->as<WireRef>()));

        auto wire = asSplice.in()->as<WireRef>();
        auto addr = asSplice.getBase();
        auto len = asSplice.getLen();
        std::print(os, "_w{}_ [{}:{}]", wireToID(wire), addr + len - 1, addr);

        std::print(os, ";\n");
        break;
      }

      case *HW_CONCAT: {
        std::print(os, "assign _w{}_ = {{",
                   wireToID(instr.def(0)->as<WireRef>()));
        for (unsigned i = 0; i < instr.getNumOthers(); i++) {
          if (i != 0)
            std::print(os, ", ");
          auto wire = instr.other(i)->as<WireRef>();
          std::print(os, "_w{}_", wireToID(wire));
        }
        std::print(os, "}};\n");
        break;
      }

      case *HW_REPEAT: {
        std::print(os,
                   "assign _w{}_ = ", wireToID(instr.def(0)->as<WireRef>()));
        auto cnt = *instr.def(0)->as<WireRef>().getNumBits() /
                   *instr.other(0)->as<WireRef>().getNumBits();
        auto wire = instr.other(0)->as<WireRef>();
        std::print(os, "{{{}{{_w{}_}}}};\n", cnt, wireToID(wire));
        break;
      }

      case *OP_TRUNC: {
        auto def = instr.def(0)->as<WireRef>();
        std::print(os, "assign _w{}_ = ", wireToID(def));
        auto wire = instr.other(0)->as<WireRef>();
        std::print(os, "_w{}_ [{}:0];\n", wireToID(wire),
                   *def.getNumBits() - 1);
        break;
      }

      case *OP_ZEXT: {
        auto def = instr.def(0)->as<WireRef>();
        std::print(os, "assign _w{}_ = ", wireToID(def));
        auto wire = instr.other(0)->as<WireRef>();
        std::print(os, "{{{}'b0, _w{}_}};\n",
                   *def.getNumBits() - *wire.getNumBits(), wireToID(wire));
        break;
      }

      case *OP_SEXT: {
        auto def = instr.def(0)->as<WireRef>();
        std::print(os, "assign _w{}_ = ", wireToID(def));
        auto wire = instr.other(0)->as<WireRef>();
        std::print(os, "{{{{{}{{_w{}_[{}]}}}}, _w{}_}};\n",
                   *def.getNumBits() - *wire.getNumBits(),
                   *wire.getNumBits() - 1, wireToID(wire),
                   *def.getNumBits() - 1);
        break;
      }

      case *HW_STDCELL_INSTANCE: {
        auto mod = instr.other(0)->as<ModuleRef>();
        std::print(os, "{} _inst{}_ (", mod->name, instanceIDCnt++);

        // in the instance outputs (defs) come first then inputs (uses),
        // interleave them in original order again.
        OperandRef defIt = *instr.def_begin();
        OperandRef useIt = *(instr.other_begin() + 1);

        for (auto [last, port] : Range{mod->ports}.mark_back()) {
          WireRef wire;
          if (port.portType == HW_INPUT_REGISTER_DEF)
            wire = (useIt++)->as<WireRef>();
          else if (port.portType == HW_OUTPUT_REGISTER_DEF)
            wire = (defIt++)->as<WireRef>();
          else
            dyno_unreachable("invalid port on stdcell");

          auto names =
              ctx.getCtx<HWDialectContext>().regNameInfo.getNames(port.reg);
          if (names.begin() != names.end())
            std::print(os, ".{}(_w{}_)", *names.begin(), wireToID(wire));
          else
            std::print(os, "_w{}_", *names.begin(), wireToID(wire));
          if (!last)
            std::print(os, ", ");
        }

        std::print(os, ");\n");
        break;
      }

      default:
        dumpInstr(instr, ctx);
        report_fatal_error("verilog dump not implemented for instruction");
        break;
      }
    }

    if (config.dumpWiresLast)
      dumpWires();
  }

  void dumpInstrModScope(InstrRef instr) {
    switch (*instr.getDialectOpcode()) {
    case *HW_REGISTER_DEF: {
      auto asReg = instr.as<RegisterIRef>();
      std::print(os, "reg");
      if (asReg.getNumBits() != 1)
        std::print(os, " [{}:0]", *asReg.getNumBits() - 1);
      std::print(
          os, " {};\n",
          VerilogIntroducedName{print.introduceNameFor(asReg.oref()).second}
              .str());
      break;
    }
    case *HW_NETLIST_PROCESS_DEF: {
      dumpNetlistProcess(instr);
      break;
    }
    case *HW_TRIGGER_DEF:
      // printed as part of uses.
      break;
    default:
      report_fatal_error("unimplemented");
    }
  }

  void dumpModule(ModuleIRef module) {
    std::print(os, "module {}(\n", module.mod()->name);
    for (auto [last, port] : module.ports().mark_back()) {
      switch (*port.getDialectOpcode()) {
      case *HW_INPUT_REGISTER_DEF:
        std::print(os, "input");
        break;
      case *HW_OUTPUT_REGISTER_DEF:
        std::print(os, "output");
        break;
      case *HW_INOUT_REGISTER_DEF:
        std::print(os, "inout");
        break;
      case *HW_REF_REGISTER_DEF:
        std::print(os, "ref");
        break;
      }

      std::print(os, " wire");
      if (*port.getNumBits() != 1) {
        std::print(os, " [{}:0]", *port.getNumBits() - 1);
      }
      VerilogIntroducedName name = print.introduceNameFor(port.oref()).second;

      std::print(os, " {}", name.str());
      if (!last)
        os << ",";
      os << "\n";
    }
    os << ");\n";

    for (auto instr : Range{module.ports_end(), module.block().end()}) {
      dumpInstrModScope(instr);
    }

    os << "endmodule\n\n";
  }

public:
  explicit DumpVerilogPass(Context &ctx, std::ostream &os)
      : ctx(ctx), os(os), print(voidStr) {}
  void runWrapper(auto &&runFunc) {
    print.reset();
    auto tok =
        print.regNames().bind(&ctx.getCtx<HWDialectContext>().regNameInfo);
    runFunc();
  }
  void run() {
    runWrapper([&] {
      for (auto mod : ctx.getCtx<HWDialectContext>().activeModules())
        dumpModule(mod.iref());
    });
  }
  void runModule(ModuleIRef mod) {
    runWrapper([&] { dumpModule(mod); });
  }

  static constexpr auto runFuncs =
      std::make_tuple(&DumpVerilogPass::runModule, &DumpVerilogPass::run);
};

}; // namespace dyno
