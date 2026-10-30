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
#include "support/Format.h"
#include "support/PointerVariant.h"
#include "support/SlabAllocator.h"
#include "support/Utility.h"
#include <bitset>
#include <cstdint>
namespace dyno {

class FlipFlopMappingPass : public Pass<FlipFlopMappingPass> {
  struct AbstractFF {
    enum Indices {
      // ff/latch.
      // clk is EN input for latch, HAS_EN is ff only. CLK always required that
      // way.
      CLK_POL,
      HAS_RST,
      RST_POL,
      HAS_SET,
      SET_POL,
      HAS_INIT,
      HAS_INV_OUT,
      HAS_REGULAR_OUT,

      NUM_INDICES_LATCH,

      SET_RST_SYNC = NUM_INDICES_LATCH,
      HAS_EN,
      EN_POL,
      NUM_INDICES,
    };

  private:
    uint16_t raw = 0;

  public:
    constexpr auto clkPol() { return BitField<uint16_t, 1, CLK_POL>{raw}; }
    constexpr auto hasEn() { return BitField<uint16_t, 1, HAS_EN>{raw}; }
    constexpr auto enPol() { return BitField<uint16_t, 1, EN_POL>{raw}; }
    constexpr auto hasRst() { return BitField<uint16_t, 1, HAS_RST>{raw}; }
    constexpr auto rstPol() { return BitField<uint16_t, 1, RST_POL>{raw}; }
    constexpr auto setRstSync() {
      return BitField<uint16_t, 1, SET_RST_SYNC>{raw};
    }
    constexpr auto hasSet() { return BitField<uint16_t, 1, HAS_SET>{raw}; }
    constexpr auto setPol() { return BitField<uint16_t, 1, SET_POL>{raw}; }
    constexpr auto hasInit() { return BitField<uint16_t, 1, HAS_INIT>{raw}; }
    constexpr auto hasInvOut() {
      return BitField<uint16_t, 1, HAS_INV_OUT>{raw};
    }
    constexpr auto hasRegularOut() {
      return BitField<uint16_t, 1, HAS_REGULAR_OUT>{raw};
    }

    auto getRaw() const { return raw; }

    constexpr AbstractFF() = default;
    constexpr explicit AbstractFF(uint16_t raw) : raw(raw) {};

    constexpr static bool valid(AbstractFF ff, bool latch = false) {
      if (ff.enPol() && !ff.hasEn())
        return false;
      if (ff.rstPol() && !ff.hasRst())
        return false;
      if (ff.setPol() && !ff.hasSet())
        return false;
      if (ff.setRstSync() && !ff.hasSet() && !ff.hasRst())
        return false;
      if (!ff.hasRegularOut() && !ff.hasInvOut())
        return false;
      if (latch && (ff.setRstSync() || ff.hasEn() || ff.enPol()))
        return false;
      return true;
    }
    constexpr static uint32_t numConfigs(bool latch = false) {
      uint32_t rv = 0;
      for (uint32_t i = 0; i < (1ull << uint32_t(NUM_INDICES)); i++) {
        if (valid(AbstractFF{uint16_t(i)}, latch))
          rv++;
      }
      return rv;
    }
  };

  enum class FFPortType : uint8_t {
    CLK,
    D,
    Q,

    NUM_MANDATORY,

    RST = NUM_MANDATORY,
    SET,
    INIT,
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
    TIE0_INIT,
    INVERT_STATE,
    ADD_SYNC_RST,
    ADD_SYNC_SET,
    INVERT_RST,
    INVERT_SET,
    INVERT_EN,
    ADD_EN0_MUX,
    ADD_EN1_MUX,
    INVERT_CLK,
    FAIL,
  };

  using StdCellOrFixupF = PointersIntsVariant<FixupType, StdCellFF *>;
  Vec<StdCellOrFixupF> ffMap = Vec<StdCellOrFixupF>(
      1ull << unsigned(AbstractFF::NUM_INDICES), FixupType::FAIL);
  SlabAllocator<StdCellFF> stdCellFFs;

  struct FatFF {
    AbstractFF abstr;
    StdCellFF stdcell;
    std::bitset<size_t(FFPortType::NUM_MANDATORY)> covered;
  };

  Context &ctx;
  HWInstrBuilder build, regBuild;
  DestroyMap<Instr> destroyMap;

  struct FFWires {
    HWValue clk;
    HWValue d;
    HWValue q;
    HWValue en;
    HWValue rst;
    HWValue set;
    HWValue init;

    RegisterRef qReg;
    unsigned bitIdx;
  };

  void initSeqWires(FlipFlopIRef instr, AbstractFF &abstr, FFWires &wires) {
    wires.clk = instr.clk();
    wires.en = instr.hasClkEn() ? instr.clkEn() : nullref;

    abstr.clkPol() = instr.clkPol();
    abstr.hasEn() = instr.hasClkEn();
    abstr.enPol() = abstr.hasEn() && instr.clkEnPolarity();
    abstr.hasInvOut() = 0; // todo: fuse NOTs
    abstr.hasRegularOut() = 1;
    abstr.setRstSync() = instr.isOpc(HW_FLIP_FLOP_SRST);
  }

  void initSeqWires(LatchIRef instr, AbstractFF &abstr, FFWires &wires) {
    auto [en, enPol] = WireVariable::getSignalWithPolarity(instr.en());
    wires.clk = en;

    abstr.clkPol() = enPol;
    abstr.hasInvOut() = 0; // todo: fuse NOTs
    abstr.hasRegularOut() = 1;

    // ff only, meaningless
    abstr.setRstSync() = 0;
    abstr.hasEn() = 0;
    abstr.enPol() = 0;
  }

  template <typename RefT> void runOnSeqInstr(RefT instr) {
    auto bits = *instr.q()->numBits;
    AbstractFF abstr;
    FFWires wires;

    if (bits >= 0xFFFE) {
      std::stringstream str;
      HWCtxPrinter{ctx, str}.printInstr(instr, false);
      report_fatal_error(
          ctx, instr,
          "too many bits ({}) in ff/latch, meant to infer memory?: {}", bits,
          std::move(str).str());
    }

    wires.qReg = regBuild.buildRegister(instr.q().getNumBits());

    HWValue dWire = instr.d();
    OperandVec<HWValue> qConcat(ctx, 1, bits);
    qConcat.emplace_back(instr.q());

    initSeqWires(instr, abstr, wires);

    SmallVec<WireRef, 2> rstWires;
    for (unsigned i = 0; i < instr.numRsts(); i++)
      rstWires.emplace_back(instr.rst(i));

    build.setInsertPoint(instr);
    for (unsigned i = 0; i < bits; i++) {
      wires.d = build.buildSplice(dWire, 1, i);
      wires.bitIdx = i;
      wires.q = ctx.getStore<Wire>().create(1);

      abstr.hasRst() = 0;
      abstr.rstPol() = 0;
      abstr.hasSet() = 0;
      abstr.setPol() = 0;
      abstr.hasInit() = 0;

      unsigned nRst = instr.numRsts();
      for (unsigned rstIdx = 0; rstIdx < nRst; rstIdx++) {
        auto val = instr.rstVal(rstIdx).template dyn_as<ConstantRef>();
        if (!val)
          report_fatal_error(ctx, instr,
                             "ff/latch with non-constant reset value");

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

      if (auto init = instr.initValue()) {
        auto val = init.template dyn_as<ConstantRef>();
        if (!val)
          report_fatal_error(ctx, instr,
                             "ff/latch with non-constant init value");
        if (auto bval = val.getBit(i); !bval.isUnk()) {
          abstr.hasInit() = 1;
          wires.init = ConstantRef::fromFourState(bval);
        }
      }

      buildSingleFF(instr, abstr, wires);
      qConcat.emplace_back(wires.q);
    }

    instr.q().getDef().replace(FatDynObjRef{nullref});
    qConcat.others().do_reverse();
    auto qVal = build.buildConcat(std::move(qConcat));
    assert(qVal);
    build.buildStore(wires.qReg, qVal);

    destroyMap.mark(instr);
  }

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
      case FFPortType::INIT:
        assert(wires.init);
        ib.addRef(wires.init);
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

  void buildSingleFF(InstrRef instr, AbstractFF abstr, FFWires wires) {
    while (1) {
      auto cmd = ffMap[abstr.getRaw()];
      if (auto optPtr = cmd.dyn_as<StdCellFF *>()) {
        buildMatchingStdCellFF(**optPtr, wires);
        return;
      } else {
        auto type = cmd.as<FixupType>();
        // action is inversed vs precomputeFixup below
        switch (type) {
        case FixupType::TIE0_RST:
        case FixupType::TIE1_RST:
          // todo: tie to sync rst
          wires.rst = ConstantRef::fromBool(type == FixupType::TIE1_RST);
          abstr.rstPol() = (type == FixupType::TIE0_RST);
          abstr.hasRst() = 1;
          break;

        case FixupType::TIE0_SET:
        case FixupType::TIE1_SET:
          // todo: tie to sync set
          wires.set = ConstantRef::fromBool(type == FixupType::TIE1_SET);
          abstr.setPol() = (type == FixupType::TIE0_SET);
          abstr.hasSet() = 1;
          break;

        case FixupType::TIE0_EN:
        case FixupType::TIE1_EN:
          wires.en = ConstantRef::fromBool(type == FixupType::TIE1_EN);
          abstr.enPol() = (type == FixupType::TIE1_EN);
          abstr.hasEn() = 1;
          break;

        case FixupType::TIE0_INIT: {
          wires.init = ConstantRef::fromBool(false);
          abstr.hasInit() = 1;
          break;
        }

        case FixupType::ADD_SYNC_RST:
          // reset is active low when we get here
          wires.d = build.buildAnd(wires.rst, wires.d);
          abstr.hasRst() = 0;
          abstr.rstPol() = 0;
          abstr.setRstSync() = 0;
          if (abstr.hasEn())
            wires.en = build.buildOr(wires.en, build.buildNot(wires.rst));
          break;
        case FixupType::ADD_SYNC_SET:
          // reset is active high when we get here
          wires.d = build.buildOr(wires.set, wires.d);
          abstr.hasSet() = 0;
          abstr.setPol() = 0;
          abstr.setRstSync() = 0;
          if (abstr.hasEn())
            wires.en = build.buildOr(wires.en, wires.set);
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

        case FixupType::INVERT_STATE: {
          std::swap(wires.rst, wires.set);
          auto invQ = ctx.getStore<Wire>().create(1);
          // todo: move not into separate invert output fixup once we have
          // proper inv out support.
          build.buildInstrRaw(OP_NOT, 2).addRef(wires.q).other().addRef(invQ);
          wires.q = invQ;
          std::swap(abstr.hasRst(), abstr.hasEn());
          std::swap(abstr.rstPol(), abstr.setPol());
          // std::swap(abstr.hasRegularOut(), abstr.hasInvOut());
          break;
        }
        case FixupType::FAIL:
          report_fatal_error(ctx, instr, "unsupported {} type",
                             config.mapLatches ? "latch" : "flip flop");
        }
      }
    }
  }

  void runOnModule(ModuleIRef mod) {
    regBuild.setInsertPoint(mod.regs_end());
    for (auto proc : mod.procs()) {
      for (auto instr : proc.block().unordered()) {
        if (config.mapLatches) {
          if (instr.isOpc(HW_LATCH))
            runOnSeqInstr(instr.as<LatchIRef>());
        } else if (instr.isOpc(HW_FLIP_FLOP, HW_FLIP_FLOP_SRST)) {
          runOnSeqInstr(instr.as<FlipFlopIRef>());
        }
      }
    }
  }

public:
#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM) FIELD(bool, mapLatches, false)
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
  Config config;

  template <typename RefT> bool classifyInput(RegisterIRef port, FatFF &ff) {
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

    auto instr = use->instr();
    if (!instr.is<RefT>())
      return false;
    auto asFF = instr.as<RefT>();

    auto useType = asFF.classifyUse(*use);
    switch (useType) {
    case (std::is_same_v<RefT, FlipFlopIRef> ? int(FlipFlopIRef::CLOCK) : int(LatchIRef::ENABLE)):
      ff.stdcell.ports.emplace_back(FFPortType::CLK);
      ff.abstr.clkPol() = pol;
      ff.covered.set((size_t)useType);
      return true;

    case RefT::D:
      if (!pol)
        return false;
      ff.stdcell.ports.emplace_back(FFPortType::D);
      ff.covered.set((size_t)useType);
      return true;
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wswitch"
    case (std::is_same_v<RefT, FlipFlopIRef> ? FlipFlopIRef::ENABLE : -1): {
        if constexpr (std::is_same_v<RefT, LatchIRef>)
          unreachable();
        ff.stdcell.ports.emplace_back(FFPortType::EN);
        ff.abstr.hasEn() = 1;
        ff.abstr.enPol() = pol;
        return true;
      }
#pragma clang diagnostic pop
    case RefT::INIT_VAL:
      ff.stdcell.ports.emplace_back(FFPortType::INIT);
      ff.abstr.hasInit() = 1;
      return true;
    case RefT::RESET_0:
    case RefT::RESET_1: {
      auto val = asFF.rstVal(useType == RefT::RESET_1 ? 1 : 0);
      if (val.template as<ConstantRef>().valueEqualsS(0)) {
        if (ff.abstr.hasRst())
          return false; // multiple resets?
        ff.abstr.hasRst() = 1;
        ff.abstr.rstPol() = pol;
        if constexpr (std::is_same_v<RefT, LatchIRef>)
          ff.abstr.setRstSync() = 0;
        else
          ff.abstr.setRstSync() = use->instr().isOpc(HW_FLIP_FLOP_SRST);
        ff.stdcell.ports.emplace_back(FFPortType::RST);
      } else if (val.template as<ConstantRef>().valueEqualsS(-1)) {
        if (ff.abstr.hasSet())
          return false; // multiple sets?
        ff.abstr.hasSet() = 1;
        ff.abstr.setPol() = pol;
        if constexpr (std::is_same_v<RefT, LatchIRef>)
          ff.abstr.setRstSync() = 0;
        else
          ff.abstr.setRstSync() = use->instr().isOpc(HW_FLIP_FLOP_SRST);
        ff.stdcell.ports.emplace_back(FFPortType::SET);
      }
      return true;
    }
    default:;
    }
    return false;
  }

  template <typename RefT> bool classifyOutput(RegisterIRef port, FatFF &ff) {
    auto st = port.getSingleStore();
    if (!st)
      return false;
    auto wire = st.as<StoreIRef>().value().dyn_as<WireRef>();
    if (!wire)
      return false;
    auto def = wire.getSingleDef();
    if (!def)
      return false;
    if (!def->instr().is<RefT>())
      return false;

    auto asFF = def->instr().as<RefT>();

    auto type = asFF.classifyUse(*def);
    if (type == RefT::Q) {
      ff.abstr.hasRegularOut() = 1;
      ff.stdcell.ports.emplace_back(FFPortType::Q);
      return true;
    }

    return false;
  }

  bool classifyPorts(ModuleIRef mod, FatFF &ff) {
    auto checkPort = [&](RegisterIRef port, DialectOpcode type) {
      if (type.is(HW_INPUT_REGISTER_DEF, HW_PARAM_REGISTER_DEF))
        return config.mapLatches ? classifyInput<LatchIRef>(port, ff)
                                 : classifyInput<FlipFlopIRef>(port, ff);
      if (type.is(HW_OUTPUT_REGISTER_DEF))
        return config.mapLatches ? classifyOutput<LatchIRef>(port, ff)
                                 : classifyOutput<FlipFlopIRef>(port, ff);
      return false;
    };

    auto &cachedPorts = mod.mod()->ports;
    if (!cachedPorts.empty()) {
      for (auto &port : cachedPorts)
        if (!checkPort(port.reg.iref(), port.portType))
          return false;
      return true;
    }

    for (auto port : mod.ports())
      if (!checkPort(port, port.getDialectOpcode()))
        return false;
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
          if (config.mapLatches ? instr.isOpc(HW_LATCH)
                                : instr.isOpc(HW_FLIP_FLOP, HW_FLIP_FLOP_SRST))
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
      std::print(dbgs(), "found {} out of {} {} types as std cells\n", count,
                 AbstractFF::numConfigs(config.mapLatches),
                 config.mapLatches ? "latch" : "flip flop");
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
    case FixupType::TIE0_INIT: {
      if (!abstr.hasInit())
        break;
      abstr.hasInit() = 0;
      fixupIfNone();
      break;
    }
    case FixupType::TIE0_EN:
    case FixupType::TIE1_EN: {
      if (!abstr.hasEn() || abstr.enPol() != (type == FixupType::TIE1_EN))
        break;
      abstr.hasEn() = 0;
      abstr.enPol() = 0;
      fixupIfNone();
      break;
    }
    case FixupType::ADD_SYNC_RST: {
      if (abstr.hasRst() || (abstr.hasSet() && !abstr.setRstSync()))
        break;
      abstr.setRstSync() = config.mapLatches ? 0 : 1;
      abstr.hasRst() = 1;
      abstr.rstPol() = 0;
      fixupIfNone();
      break;
    }
    case FixupType::ADD_SYNC_SET: {
      if (abstr.hasSet() || (abstr.hasRst() && !abstr.setRstSync()))
        break;
      abstr.setRstSync() = config.mapLatches ? 0 : 1;
      abstr.hasSet() = 1;
      abstr.setPol() = 1;
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

    case FixupType::INVERT_STATE: {
      std::swap(abstr.hasSet(), abstr.hasRst());
      std::swap(abstr.setPol(), abstr.rstPol());
      // fixup apply also adds not for output currently.
      // std::swap(abstr.hasRegularOut(), abstr.hasInvOut());
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

    unsigned mapped [[maybe_unused]] = 0;
    for (auto [i, entry] : Range{ffMap}.enumerate()) {
      if (!AbstractFF::valid(AbstractFF{uint16_t(i)}, config.mapLatches))
        continue;
      if (auto fixup = entry.dyn_as<FixupType>();
          !fixup || *fixup != FixupType::FAIL)
        mapped++;
    }
    DYNO_DBG({
      std::print(dbgs(), "covered {} out of {} {} types with fixups\n", mapped,
                 AbstractFF::numConfigs(config.mapLatches),
                 config.mapLatches ? "latch" : "flip flop");
    });
  }
  void runWrapper(auto &&runFunc) {
    stdCellFFs.clear();
    for (auto &entry : ffMap)
      entry = FixupType::FAIL;
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
