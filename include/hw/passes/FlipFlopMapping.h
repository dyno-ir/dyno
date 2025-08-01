#pragma once

#include "dyno/Constant.h"
#include "dyno/DestroyMap.h"
#include "dyno/Instr.h"
#include "hw/FlipFlop.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Register.h"
#include "support/Debug.h"
#include "support/ErrorRecovery.h"
#include "support/PointerVariant.h"
#include "support/SlabAllocator.h"
#include "support/Utility.h"
#include <bit>
#include <bitset>
#include <cstdint>
namespace dyno {

class FlipFlopMappingPass {
  struct AbstractFF {
    uint8_t clkPol : 1 = 0;

    uint8_t hasClkEn : 1 = 0;
    uint8_t clkEnPol : 1 = 0;

    uint8_t hasRst : 1 = 0;
    uint8_t rstPol : 1 = 0;

    uint8_t hasSet : 1 = 0;
    uint8_t setPol : 1 = 0;

    uint8_t hasInvOut : 1 = 0;
    uint8_t hasRegularOut : 1 = 0;

    // uint8_t hasMux : 1 = 0;

    auto raw() const { return std::bit_cast<uint16_t>(*this); }
    AbstractFF() = default;
    explicit AbstractFF(uint16_t raw) {
      *this = std::bit_cast<AbstractFF>(raw);
    };
  };

  enum class FFPortType : uint8_t {
    CLK,
    D,
    Q,

    NUM_MANDATORY,

    RST = NUM_MANDATORY,
    SET,
    D2,
    DSEL,
    Q_INV,
    EN,

    TIE_0,
    TIE_1,
    UNUSED_OUT,

    NUM
  };

  struct StdCellFF {
    ObjRef<Module> module;
    SmallVec<FFPortType, 4> ports;
  };

  enum class FixupType {
    TIE_RST,
    TIE_SET,
    TIE_EN,
    INVERT_RST,
    INVERT_SET,
    INVERT_EN,
    INVERT_OUTPUT,
    INVERT_CLK,
    FAIL,
  };

  using StdCellOrFixupF = PointersIntsVariant<FixupType, StdCellFF *>;
  std::vector<StdCellOrFixupF> ffMap =
      std::vector<StdCellOrFixupF>(512, FixupType::FAIL);
  SlabAllocator<StdCellFF> stdCellFFs;

  struct FatFF {
    AbstractFF abstr;
    StdCellFF stdcell;
    std::bitset<size_t(FFPortType::NUM_MANDATORY)> covered;
  };

  HWContext &ctx;
  HWInstrBuilder build;
  DestroyMap<Instr> destroyMap;

  void runOnInstr(FlipFlopIRef instr) {
    auto bits = *instr.q()->numBits;
    AbstractFF abstr;
    build.setInsertPoint(
        (*instr.parentMod(ctx).comb_procs().begin()).block().end());
    FFWires wires;
    WireRef dWire = build.buildLoad(instr.d());
    SmallVec<HWValue, 32> qWires;
    qWires.reserve(bits);

    wires.clk = build.buildLoad(instr.clk());
    wires.en = instr.hasClkEn() ? build.buildLoad(instr.clkEn()) : nullref;

    abstr.clkPol = instr.clkPolarity();
    abstr.hasClkEn = instr.hasClkEn();
    abstr.clkEnPol = abstr.hasClkEn && instr.clkEnPolarity();

    // todo: fuse NOTs
    abstr.hasInvOut = 0;
    abstr.hasRegularOut = 1;

    SmallVec<WireRef, 2> rstWires;
    for (uint i = 0; i < instr.numRsts(); i++)
      rstWires.emplace_back(build.buildLoad(instr.rst(i)));

    for (uint i = 0; i < bits; i++) {
      wires.d = build.buildSplice(dWire, BitRange{i, 1}).as<WireRef>();
      qWires.emplace_back(wires.q = ctx.getWires().create(1));

      abstr.hasRst = 0;
      abstr.rstPol = 0;
      abstr.hasSet = 0;
      abstr.setPol = 0;
      wires.rst = nullref;
      wires.set = nullref;

      uint nRst = instr.numRsts();
      for (uint rstIdx = 0; rstIdx < nRst; rstIdx++) {
        auto val = instr.rstVal(rstIdx);
        auto bitVal = val.getBit(i);
        if (bitVal.isUnk())
          break;

        if (bitVal == FourState::S0) {
          if (abstr.hasRst)
            report_fatal_error("two async resets");
          abstr.hasRst = 1;
          abstr.rstPol = instr.rstPolarity(rstIdx);
          wires.rst = rstWires[rstIdx];
        } else {
          if (abstr.hasSet)
            report_fatal_error("two async sets");
          abstr.hasSet = 1;
          abstr.setPol = instr.rstPolarity(rstIdx);
          wires.set = rstWires[rstIdx];
        }
      }
      buildSingleFF(abstr, wires);
    }

    std::reverse(qWires.begin(), qWires.end());
    build.buildStore(instr.q(), build.buildConcat(qWires));
    destroyMap.mark(instr);
  }

  struct FFWires {
    WireRef clk;
    WireRef d;
    WireRef q;
    WireRef en;
    WireRef rst;
    WireRef set;
  };

  void buildMatchingStdCellFF(const StdCellFF &ff, FFWires wires) {
    auto ib = build.buildInstrRaw(HW_STDCELL_INSTANCE, ff.ports.size() + 1);
    // outputs
    for (auto port : ff.ports) {
      switch (port) {
      case FFPortType::Q: {
        ib.addRef(wires.q);
        break;
      }
      case FFPortType::UNUSED_OUT: {
        ib.addRef(ctx.getWires().create(1));
        break;
      }
      default:
        break;
      }
    }

    ib.other();
    ib.addRef(ctx.getModules().resolve(ff.module));

    // inputs
    for (auto port : ff.ports) {
      switch (port) {
      case FFPortType::CLK:
        ib.addRef(wires.clk);
        break;
      case FFPortType::D:
        ib.addRef(wires.d);
        break;
      case FFPortType::RST:
        assert(wires.rst);
        ib.addRef(wires.rst);
        break;
      case FFPortType::SET:
        assert(wires.set);
        ib.addRef(wires.set);
        break;
      case FFPortType::EN:
        assert(wires.en);
        ib.addRef(wires.en);
        break;
      case FFPortType::TIE_0:
        ib.addRef(ConstantRef::fromBool(0));
        break;
      case FFPortType::TIE_1:
        ib.addRef(ConstantRef::fromBool(1));
        break;
      case FFPortType::UNUSED_OUT:
      case FFPortType::Q:
        // outputs already handled
        break;
      default:
        dyno_unreachable("not implemented");
      }
    }
  }

  void buildSingleFF(AbstractFF abstr, FFWires wires) {
    auto cmd = ffMap[abstr.raw()];
    if (auto optPtr = cmd.dyn_as<StdCellFF *>()) {
      buildMatchingStdCellFF(**optPtr, wires);
    } else {
      switch (cmd.as<FixupType>()) {
      case FixupType::TIE_RST:
      case FixupType::TIE_SET:
      case FixupType::TIE_EN:
      case FixupType::INVERT_RST:
      case FixupType::INVERT_SET:
      case FixupType::INVERT_EN:
      case FixupType::INVERT_OUTPUT:
      case FixupType::INVERT_CLK:
        break;

      case FixupType::FAIL:
        report_fatal_error("unsupported flip flop type");
      }
      abort();
    }
  }

  void runOnModule(ModuleIRef mod) {
    for (auto instr : mod.block()) {
      if (instr.isOpc(HW_FLIP_FLOP))
        runOnInstr(instr.as<FlipFlopIRef>());
    }
  }

public:
  bool classifyInput(RegisterIRef port, FatFF &ff) {
    auto use = port.oref().getSingleUse();
    if (!use)
      return false;
    if (use->instr().isOpc(HW_FLIP_FLOP)) {
      auto asFF = use->instr().as<FlipFlopIRef>();

      auto useType = asFF.classifyUse(*use);
      switch (useType) {
      case FlipFlopIRef::CLOCK:
        ff.stdcell.ports.emplace_back(FFPortType::CLK);
        ff.abstr.clkPol = asFF.getPolarity(*use);
        ff.covered.set((size_t)useType);
        break;
      case FlipFlopIRef::D:
        ff.stdcell.ports.emplace_back(FFPortType::D);
        ff.covered.set((size_t)useType);
        break;
      case FlipFlopIRef::Q:
        ff.stdcell.ports.emplace_back(FFPortType::Q);
        ff.covered.set((size_t)useType);
        break;
      case FlipFlopIRef::ENABLE:
        ff.stdcell.ports.emplace_back(FFPortType::EN);
        ff.abstr.hasClkEn = 1;
        ff.abstr.clkEnPol = asFF.getPolarity(*use);
        break;
      case FlipFlopIRef::RESET_0:
      case FlipFlopIRef::RESET_1: {
        auto val = asFF.rstVal(useType == FlipFlopIRef::RESET_1 ? 1 : 0);
        if (val.valueEqualsS(0)) {
          if (ff.abstr.hasRst)
            return false; // multiple resets?
          ff.abstr.hasRst = 1;
          ff.abstr.rstPol = asFF.getPolarity(*use);
          ff.stdcell.ports.emplace_back(FFPortType::RST);
        } else if (val.valueEqualsS(-1)) {
          if (ff.abstr.hasSet)
            return false; // multiple sets?
          ff.abstr.hasSet = 1;
          ff.abstr.setPol = asFF.getPolarity(*use);
          ff.stdcell.ports.emplace_back(FFPortType::SET);
        }
        break;
      }
      default:
        return false;
      }
    }

    // todo: MUX before flip flop
    return true;
  }

  bool classifyOutput(RegisterIRef port, FatFF &ff) {
    auto use = port.oref().getSingleUse();
    if (!use)
      return false;

    // todo: detect NOT for q inv

    if (use->instr().isOpc(HW_FLIP_FLOP)) {
      auto asFF = use->instr().as<FlipFlopIRef>();
      auto type = asFF.classifyUse(*use);
      if (type == FlipFlopIRef::Q) {
        ff.abstr.hasRegularOut = 1;
        ff.stdcell.ports.emplace_back(FFPortType::Q);
      } else
        return false;
    }

    return true;
  }

  bool classifyPorts(ModuleIRef mod, FatFF &ff) {
    for (auto port : mod.ports()) {
      if (port.isOpc(HW_INPUT_REGISTER_DEF)) {
        if (!classifyInput(port, ff))
          return false;
      } else if (port.isOpc(HW_OUTPUT_REGISTER_DEF)) {
        if (!classifyOutput(port, ff))
          return false;
      } else
        return false;
    }
    return true;
  }

  void findFlipFlopStdCells() {
    uint count = 0;
    for (auto obj : ctx.getModules()) {
      auto mod = obj.iref();
      if (!mod.isOpc(HW_STDCELL_DEF))
        continue;
      auto it = mod.regs_end();
      if (it == mod.block().end())
        continue;
      if (!it.instr().isOpc(HW_FLIP_FLOP))
        continue;
      FatFF ff;
      if (!classifyPorts(mod, ff))
        continue;
      ff.stdcell.module = obj;
      auto ptr = &stdCellFFs.emplace_back(ff.stdcell);
      auto &entry = ffMap[std::bit_cast<uint16_t>(ff.abstr)];
      if (entry == FixupType::FAIL) {
        count++;
        entry = ptr;
        dumpInstr(mod);
      }
    }
    DEBUG("FlipFlopMapping", {
      std::print(dbgs(), "found {} out of {} flip flop types as std cells\n",
                 count, ffMap.size());
    });
  }

  void precomputeFixup(FixupType type, uint ffIdx) {
    AbstractFF abstr = std::bit_cast<AbstractFF>((uint16_t)ffIdx);

    auto fixupIfNone = [&]() {
      auto &entry = ffMap[AbstractFF{abstr}.raw()];
      if (entry == FixupType::FAIL)
        entry = type;
    };

    switch (type) {
    case FixupType::TIE_RST: {
      if (!abstr.hasRst)
        break;
      abstr.hasRst = 0;
      abstr.rstPol = 0;
      fixupIfNone();
      break;
    }
    case FixupType::TIE_SET: {
      if (!abstr.hasSet)
        break;
      abstr.hasSet = 0;
      abstr.setPol = 0;
      fixupIfNone();
      break;
    }
    case FixupType::TIE_EN: {
      if (!abstr.hasClkEn)
        break;
      abstr.hasClkEn = 0;
      abstr.clkEnPol = 0;
      fixupIfNone();
      break;
    }
    case FixupType::INVERT_RST: {
      if (!abstr.hasRst)
        break;
      abstr.rstPol = !abstr.rstPol;
      fixupIfNone();
      break;
    }
    case FixupType::INVERT_SET: {
      if (!abstr.hasSet)
        break;
      abstr.setPol = !abstr.setPol;
      fixupIfNone();
      break;
    }
    case FixupType::INVERT_EN: {
      if (!abstr.hasClkEn)
        break;
      abstr.clkEnPol = !abstr.clkEnPol;
      fixupIfNone();
      break;
    }

    case FixupType::INVERT_OUTPUT: {
      auto t = abstr.hasInvOut;
      abstr.hasInvOut = abstr.hasRegularOut;
      abstr.hasRegularOut = t;

      abstr.clkEnPol = !abstr.clkEnPol;
      fixupIfNone();
      break;
    }

    case FixupType::INVERT_CLK: {
      abstr.clkPol = !abstr.clkPol;
      fixupIfNone();
      break;
    }

    default:
      dyno_unreachable("");
    }
  }

  void precomputeFixups() {
    for (auto type = FixupType(0); type != FixupType::FAIL;
         type = FixupType(int(type) + 1)) {
      for (uint i = 0; i < ffMap.size(); i++) {
        if (auto type = ffMap[i].dyn_as<FixupType>(); type == FixupType::FAIL)
          continue;
        precomputeFixup(type, i);
      }
    }

    uint missing = 0;
    for (auto entry : ffMap) {
      if (auto fixup = entry.dyn_as<FixupType>();
          fixup && *fixup == FixupType::FAIL)
        missing++;
    }
    DEBUG("FlipFlopMapping", {
      std::print(dbgs(), "covered {} out of {} flip flop types with fixups\n",
                 ffMap.size() - missing, ffMap.size());
    });
  }

  void run() {
    findFlipFlopStdCells();
    precomputeFixups();

    ctx.getInstrs().createHooks.emplace_back(
        [&](InstrRef ref) { destroyMap.ensureUnmarked(ref); });

    destroyMap.resize(ctx.getInstrs().numIDs());
    for (auto mod : ctx.activeModules()) {
      if (mod.iref().isOpc(HW_MODULE_DEF))
        runOnModule(mod.iref());
    }

    ctx.getInstrs().createHooks.pop_back();

    destroyMap.apply(ctx.getInstrs(),
                     [&](InstrRef ref) { build.destroyInstr(ref); });
    destroyMap.clear();
  }
  explicit FlipFlopMappingPass(HWContext &ctx) : ctx(ctx), build(ctx) {}
};
}; // namespace dyno
