#pragma once

#include "dyno/Constant.h"
#include "dyno/Context.h"
#include "dyno/DestroyMap.h"
#include "dyno/Instr.h"
#include "dyno/MutInstr.h"
#include "dyno/Obj.h"
#include "dyno/Pass.h"
#include "hw/FlipFlop.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Register.h"
#include "hw/Wire.h"
#include "hw/analysis/WireVariable.h"
#include "hw/passes/DumpVerilog.h"
#include "support/Bits.h"
#include "support/Debug.h"
#include "support/ErrorRecovery.h"
#include "support/PointerVariant.h"
#include "support/SlabAllocator.h"
#include "support/Utility.h"
#include <bitset>
#include <cstdint>
namespace dyno {

class FlipFlopMappingPass : public Pass<FlipFlopMappingPass> {

  struct AbstractFF {
    enum Indices {
      CLK_POL,
      HAS_EN,
      EN_POL,
      HAS_RST,
      RST_POL,
      HAS_SET,
      SET_POL,
      HAS_INV_OUT,
      HAS_REGULAR_OUT
    };

  private:
    uint16_t raw = 0;

  public:
    auto clkPol() { return BitField<uint16_t, 1, CLK_POL>{raw}; }
    auto hasEn() { return BitField<uint16_t, 1, HAS_EN>{raw}; }
    auto enPol() { return BitField<uint16_t, 1, EN_POL>{raw}; }
    auto hasRst() { return BitField<uint16_t, 1, HAS_RST>{raw}; }
    auto rstPol() { return BitField<uint16_t, 1, RST_POL>{raw}; }
    auto hasSet() { return BitField<uint16_t, 1, HAS_SET>{raw}; }
    auto setPol() { return BitField<uint16_t, 1, SET_POL>{raw}; }
    auto hasInvOut() { return BitField<uint16_t, 1, HAS_INV_OUT>{raw}; }
    auto hasRegularOut() { return BitField<uint16_t, 1, HAS_REGULAR_OUT>{raw}; }

    auto getRaw() const { return raw; }

    AbstractFF() = default;
    explicit AbstractFF(uint16_t raw) : raw(raw) {};
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
    TIE0_RST,
    TIE0_SET,
    TIE0_EN,
    TIE1_RST,
    TIE1_SET,
    TIE1_EN,
    INVERT_RST,
    INVERT_SET,
    INVERT_EN,
    INVERT_OUTPUT,
    ADD_EN0_MUX,
    ADD_EN1_MUX,
    INVERT_CLK,
    FAIL,
  };

  using StdCellOrFixupF = PointersIntsVariant<FixupType, StdCellFF *>;
  Vec<StdCellOrFixupF> ffMap = Vec<StdCellOrFixupF>(512, FixupType::FAIL);
  SlabAllocator<StdCellFF> stdCellFFs;

  struct FatFF {
    AbstractFF abstr;
    StdCellFF stdcell;
    std::bitset<size_t(FFPortType::NUM_MANDATORY)> covered;
  };

  Context &ctx;
  HWInstrBuilder build, regBuild;
  DestroyMap<Instr> destroyMap;

  void runOnInstr(FlipFlopIRef instr) {
    auto bits = *instr.q()->numBits;
    AbstractFF abstr;
    FFWires wires;

    wires.qReg = regBuild.buildRegister(instr.q().getNumBits());

    HWValue dWire = instr.d();
    OperandVec<HWValue> qConcat(ctx, 1, bits);
    qConcat.emplace_back(instr.q());
    instr.q().getDef().replace(FatDynObjRef{nullref});

    wires.clk = instr.clk();
    wires.en = instr.hasClkEn() ? instr.clkEn() : nullref;

    abstr.clkPol() = instr.clkPol();
    abstr.hasEn() = instr.hasClkEn();
    abstr.enPol() = abstr.hasEn() && instr.clkEnPolarity();

    // todo: fuse NOTs
    abstr.hasInvOut() = 0;
    abstr.hasRegularOut() = 1;

    SmallVec<WireRef, 2> rstWires;
    for (unsigned i = 0; i < instr.numRsts(); i++)
      rstWires.emplace_back(instr.rst(i));

    build.setInsertPoint(instr);
    for (unsigned i = 0; i < bits; i++) {
      wires.d = build.buildSplice(dWire, 1, i);
      wires.bitIdx = i;
      qConcat.emplace_back(wires.q = ctx.getStore<Wire>().create(1));

      abstr.hasRst() = 0;
      abstr.rstPol() = 0;
      abstr.hasSet() = 0;
      abstr.setPol() = 0;

      unsigned nRst = instr.numRsts();
      for (unsigned rstIdx = 0; rstIdx < nRst; rstIdx++) {
        auto val = instr.rstVal(rstIdx).dyn_as<ConstantRef>();
        if (!val)
          report_fatal_error("ff with non-constant reset value: {}",
                             HWCtxPrinter{ctx}.toString(instr));

        auto bitVal = val.getBit(i);
        if (bitVal.isUnk())
          break;

        if (bitVal == FourState::S0) {
          if (abstr.hasRst())
            report_fatal_error("two async resets");
          abstr.hasRst() = 1;
          abstr.rstPol() = instr.rstPol(rstIdx);
          wires.rst = rstWires[rstIdx];
        } else {
          if (abstr.hasSet())
            report_fatal_error("two async sets");
          abstr.hasSet() = 1;
          abstr.setPol() = instr.rstPol(rstIdx);
          wires.set = rstWires[rstIdx];
        }
      }
      buildSingleFF(abstr, wires);
    }

    qConcat.others().do_reverse();
    auto qVal = build.buildConcat(std::move(qConcat));
    assert(qVal);
    build.buildStore(wires.qReg, qVal);

    destroyMap.mark(instr);
  }

  struct FFWires {
    HWValue clk;
    HWValue d;
    HWValue q;
    HWValue en;
    HWValue rst;
    HWValue set;

    RegisterRef qReg;
    unsigned bitIdx;
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
        ib.addRef(ctx.getStore<Wire>().create(1));
        break;
      }
      default:
        break;
      }
    }

    ib.other();
    ib.addRef(ctx.getStore<Module>().resolve(ff.module));

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
    while (1) {
      auto cmd = ffMap[abstr.getRaw()];
      if (auto optPtr = cmd.dyn_as<StdCellFF *>()) {
        buildMatchingStdCellFF(**optPtr, wires);
        return;
      } else {
        auto type = cmd.as<FixupType>();
        switch (type) {
        case FixupType::TIE0_RST:
        case FixupType::TIE1_RST:
          wires.rst = ConstantRef::fromBool(type == FixupType::TIE1_RST);
          abstr.rstPol() = (type == FixupType::TIE0_RST);
          abstr.hasRst() = 1;
          break;

        case FixupType::TIE0_SET:
        case FixupType::TIE1_SET:
          wires.rst = ConstantRef::fromBool(type == FixupType::TIE1_SET);
          abstr.setPol() = (type == FixupType::TIE0_SET);
          abstr.hasSet() = 1;
          break;

        case FixupType::TIE0_EN:
        case FixupType::TIE1_EN:
          wires.en = ConstantRef::fromBool(type == FixupType::TIE1_EN);
          abstr.enPol() = (type == FixupType::TIE0_EN);
          abstr.hasEn() = 1;
          break;

        case FixupType::INVERT_RST:
          wires.rst = build.buildNot(wires.rst);
          abstr.rstPol() = !abstr.rstPol();
          break;
        case FixupType::INVERT_SET:
          wires.set = build.buildNot(wires.set);
          abstr.setPol() = !abstr.setPol();
          break;
        case FixupType::INVERT_EN:
          wires.en = build.buildNot(wires.en);
          abstr.enPol() = !abstr.enPol();
          break;
        case FixupType::ADD_EN0_MUX: {
          // if we directly use wires.q we build a cyclic dependency, better
          // avoid that while we can.
          auto qIndirect = build.buildLoad(wires.qReg, 1, wires.bitIdx);
          wires.d = build.buildMux(wires.en, qIndirect, wires.d);
          abstr.hasEn() = 0;
          abstr.enPol() = 0;
          break;
        }
        case FixupType::ADD_EN1_MUX: {
          auto qIndirect = build.buildLoad(wires.qReg, 1, wires.bitIdx);
          wires.d = build.buildMux(wires.en, wires.d, qIndirect);
          abstr.hasEn() = 0;
          abstr.enPol() = 0;
          break;
        }
        case FixupType::INVERT_CLK:
          wires.clk = build.buildNot(wires.clk);
          abstr.clkPol() = !abstr.clkPol();
          break;

        case FixupType::INVERT_OUTPUT:
        case FixupType::FAIL:
          report_fatal_error("unsupported flip flop type");
        }
      }
    }
  }

  void runOnModule(ModuleIRef mod) {
    regBuild.setInsertPoint(mod.regs_end());
    for (auto proc : mod.procs()) {
      for (auto instr : proc.block().unordered())
        if (instr.isOpc(HW_FLIP_FLOP))
          runOnInstr(instr.as<FlipFlopIRef>());
    }
  }

public:
  bool classifyInput(RegisterIRef port, FatFF &ff) {
    auto ld = port.getSingleLoad();
    if (!ld)
      return false;
    auto wire = ld.as<LoadIRef>().value();
    auto use = wire.getSingleUse();
    if (!use)
      return false;

    bool pol = true;
    if (use->instr().isOpc(OP_NOT)) {
      wire = use->instr().def()->as<WireRef>();
      use = wire.getSingleUse();
      if (!use)
        return false;
      pol = false;
    }

    if (!use->instr().isOpc(HW_FLIP_FLOP))
      return false;
    auto asFF = use->instr().as<FlipFlopIRef>();

    auto useType = asFF.classifyUse(*use);
    switch (useType) {
    case FlipFlopIRef::CLOCK:
      ff.stdcell.ports.emplace_back(FFPortType::CLK);
      ff.abstr.clkPol() = pol;
      ff.covered.set((size_t)useType);
      return true;
    case FlipFlopIRef::D:
      if (!pol)
        return false;
      ff.stdcell.ports.emplace_back(FFPortType::D);
      ff.covered.set((size_t)useType);
      return true;
    case FlipFlopIRef::ENABLE:
      ff.stdcell.ports.emplace_back(FFPortType::EN);
      ff.abstr.hasEn() = 1;
      ff.abstr.enPol() = pol;
      return true;
    case FlipFlopIRef::RESET_0:
    case FlipFlopIRef::RESET_1: {
      auto val = asFF.rstVal(useType == FlipFlopIRef::RESET_1 ? 1 : 0);
      if (val.as<ConstantRef>().valueEqualsS(0)) {
        if (ff.abstr.hasRst())
          return false; // multiple resets?
        ff.abstr.hasRst() = 1;
        ff.abstr.rstPol() = pol;
        ff.stdcell.ports.emplace_back(FFPortType::RST);
      } else if (val.as<ConstantRef>().valueEqualsS(-1)) {
        if (ff.abstr.hasSet())
          return false; // multiple sets?
        ff.abstr.hasSet() = 1;
        ff.abstr.setPol() = pol;
        ff.stdcell.ports.emplace_back(FFPortType::SET);
      }
      return true;
    }
    default:;
    }
    return false;
  }

  bool classifyOutput(RegisterIRef port, FatFF &ff) {
    auto st = port.getSingleStore();
    if (!st)
      return false;
    auto wire = st.as<StoreIRef>().value().dyn_as<WireRef>();
    if (!wire)
      return false;
    auto def = wire.getSingleDef();
    if (!def)
      return false;
    if (!def->instr().isOpc(HW_FLIP_FLOP))
      return false;

    auto asFF = def->instr().as<FlipFlopIRef>();

    auto type = asFF.classifyUse(*def);
    if (type == FlipFlopIRef::Q) {
      ff.abstr.hasRegularOut() = 1;
      ff.stdcell.ports.emplace_back(FFPortType::Q);
      return true;
    }

    return false;
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
    unsigned count [[maybe_unused]] = 0;
    for (auto obj : ctx.getStore<Module>()) {
      auto mod = obj.iref();
      if (!mod.isOpc(HW_STDCELL_DEF))
        continue;
      for (auto proc : mod.procs())
        for (auto instr : proc.block())
          if (instr.isOpc(HW_FLIP_FLOP))
            goto found;
      continue;
    found:
      FatFF ff;
      if (!classifyPorts(mod, ff))
        continue;
      ff.stdcell.module = obj;
      auto ptr = &stdCellFFs.emplace_back(ff.stdcell);
      auto &entry = ffMap[ff.abstr.getRaw()];
      if (entry == FixupType::FAIL) {
        count++;
        entry = ptr;
      }
    }
    DYNO_DBG({
      std::print(dbgs(), "found {} out of {} flip flop types as std cells\n",
                 count, ffMap.size());
    });
  }

  void precomputeFixup(FixupType type, unsigned ffIdx) {
    AbstractFF abstr = AbstractFF{(uint16_t)ffIdx};

    auto fixupIfNone = [&]() {
      auto &entry = ffMap[abstr.getRaw()];
      if (entry == FixupType::FAIL)
        entry = type;
    };

    switch (type) {
    case FixupType::TIE0_RST:
    case FixupType::TIE1_RST: {
      if (!abstr.hasRst() || abstr.rstPol() != (type == FixupType::TIE0_RST))
        break;
      abstr.hasRst() = 0;
      abstr.rstPol() = 0;
      fixupIfNone();
      break;
    }
    case FixupType::TIE0_SET:
    case FixupType::TIE1_SET: {
      if (!abstr.hasSet() || abstr.setPol() != (type == FixupType::TIE0_SET))
        break;
      abstr.hasSet() = 0;
      abstr.setPol() = 0;
      fixupIfNone();
      break;
    }
    case FixupType::TIE0_EN:
    case FixupType::TIE1_EN: {
      if (!abstr.hasEn() || abstr.enPol() != (type == FixupType::TIE0_EN))
        break;
      abstr.hasEn() = 0;
      abstr.enPol() = 0;
      fixupIfNone();
      break;
    }
    case FixupType::INVERT_RST: {
      if (!abstr.hasRst())
        break;
      abstr.rstPol() = !abstr.rstPol();
      fixupIfNone();
      break;
    }
    case FixupType::INVERT_SET: {
      if (!abstr.hasSet())
        break;
      abstr.setPol() = !abstr.setPol();
      fixupIfNone();
      break;
    }
    case FixupType::INVERT_EN: {
      if (!abstr.hasEn())
        break;
      abstr.enPol() = !abstr.enPol();
      fixupIfNone();
      break;
    }

    case FixupType::INVERT_OUTPUT: {
      auto t = abstr.hasInvOut();
      abstr.hasInvOut() = abstr.hasRegularOut();
      abstr.hasRegularOut() = t;
      fixupIfNone();
      break;
    }

    case FixupType::ADD_EN0_MUX:
    case FixupType::ADD_EN1_MUX: {
      if (abstr.hasEn())
        break;
      abstr.hasEn() = 1;
      abstr.enPol() = (type == FixupType::ADD_EN1_MUX);
      fixupIfNone();
      break;
    }

    case FixupType::INVERT_CLK: {
      abstr.clkPol() = !abstr.clkPol();
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
      for (unsigned i = 0; i < ffMap.size(); i++) {
        if (auto type = ffMap[i].dyn_as<FixupType>(); type == FixupType::FAIL)
          continue;
        precomputeFixup(type, i);
      }
    }

    unsigned missing [[maybe_unused]] = 0;
    for (auto entry : ffMap) {
      if (auto fixup = entry.dyn_as<FixupType>();
          fixup && *fixup == FixupType::FAIL)
        missing++;
    }
    DYNO_DBG({
      std::print(dbgs(), "covered {} out of {} flip flop types with fixups\n",
                 ffMap.size() - missing, ffMap.size());
    });
  }
  void runWrapper(auto &&runFunc) {
    stdCellFFs.clear();
    findFlipFlopStdCells();
    precomputeFixups();

    ctx.getStore<Instr>().createHooks.emplace_back(
        [&](InstrRef ref) { destroyMap.ensureUnmarked(ref); });
    destroyMap.resize(ctx.getStore<Instr>().numIDs());

    runFunc();

    ctx.getStore<Instr>().createHooks.pop_back();
    destroyMap.apply(ctx.getStore<Instr>(),
                     [&](InstrRef ref) { build.destroyInstr(ref); });
    destroyMap.clear();
  }
  void run() {
    runWrapper([&] {
      for (auto mod : ctx.getCtx<HWDialectContext>().activeModules()) {
        if (mod.iref().isOpc(HW_MODULE_DEF))
          runOnModule(mod.iref());
      }
    });
  }
  void runModule(ModuleIRef mod) {
    runWrapper([&] {
      if (mod.isOpc(HW_MODULE_DEF))
        runOnModule(mod);
    });
  }
  static constexpr auto runFuncs =
      mk_tuple(&FlipFlopMappingPass::runModule, &FlipFlopMappingPass::run);

  auto make(Context &ctx) { return FlipFlopMappingPass(ctx); }
  explicit FlipFlopMappingPass(Context &ctx)
      : ctx(ctx), build(ctx), regBuild(ctx) {}
};
}; // namespace dyno
