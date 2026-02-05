#pragma once

#include "dyno/Constant.h"
#include "dyno/Context.h"
#include "dyno/Pass.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "support/ErrorRecovery.h"
namespace dyno {

class ConstantMappingPass : public Pass<ConstantMappingPass> {
  Context &ctx;
  HWInstrBuilder build;
  enum class ConstModType { NONE, ZERO, ONE, ZERO_ONE, ONE_ZERO };

  SmallVec<std::pair<ConstModType, ModuleIRef>, 4> constMods;

  static ConstModType checkIfModuleIsConst(ModuleIRef iref) {
    auto mod = iref.mod();
    if (mod->ports.size() > 2)
      return ConstModType::NONE;

    Optional<uint32_t> zeroIdx = nullopt;
    Optional<uint32_t> oneIdx = nullopt;

    for (auto [i, port] : Range{mod->ports}.enumerate()) {
      if (port.portType != HW_OUTPUT_REGISTER_DEF)
        return ConstModType::NONE;
      auto reg = port.reg;
      if (!reg.hasSingleUse())
        return ConstModType::NONE;
      auto use = reg.getSingleUse()->instr();
      if (!use.isOpc(HW_STORE))
        return ConstModType::NONE;
      auto store = use.as<StoreIRef>();
      if (!store.value().is<ConstantRef>())
        return ConstModType::NONE;

      if (store.value().as<ConstantRef>() == "1'b1"_bv)
        oneIdx = i;
      else if (store.value().as<ConstantRef>() == "1'b0"_bv)
        zeroIdx = i;
    }

    if (mod->ports.size() == 2 && (!zeroIdx || !oneIdx))
      return ConstModType::NONE;

    if (zeroIdx == 0 && oneIdx == 1)
      return ConstModType::ZERO_ONE;
    if (oneIdx == 0 && zeroIdx == 1)
      return ConstModType::ONE_ZERO;
    if (zeroIdx == 0)
      return ConstModType::ZERO;
    if (oneIdx == 0)
      return ConstModType::ONE;

    return ConstModType::NONE;
  }
  void findConstantModules() {
    constMods.clear();
    for (auto module : ctx.getStore<Module>()) {
      if (!module.iref().isOpc(HW_STDCELL_DEF))
        continue;
      auto res = checkIfModuleIsConst(module.iref());
      if (res == ConstModType::NONE)
        continue;
      constMods.emplace_back(res, module.iref());
    }

    // todo: sorting?
  }

  std::pair<WireRef, WireRef> getConstant01Wires() {
    WireRef zeroW = ctx.getStore<Wire>().create(1);
    WireRef oneW = ctx.getStore<Wire>().create(1);

    // if one cell outputs both use that
    auto it = Range{constMods}.find_if([](auto pair) {
      return pair.first == ConstModType::ONE_ZERO ||
             pair.first == ConstModType::ZERO_ONE;
    });
    if (it != constMods.end()) {
      auto ib = build.buildInstrRaw(HW_STDCELL_INSTANCE, 3);
      if (it->first == ConstModType::ZERO_ONE)
        ib.addRef(zeroW).addRef(oneW);
      else
        ib.addRef(oneW).addRef(zeroW);

      ib.other().addRef(it->second.mod());
      return std::pair(zeroW, oneW);
    }

    it = Range{constMods}.find_if(
        [](auto pair) { return pair.first == ConstModType::ZERO; });
    if (it == constMods.end())
      report_fatal_error("no constant zero std cell");
    build.buildInstrRaw(HW_STDCELL_INSTANCE, 2)
        .addRef(zeroW)
        .other()
        .addRef(it->second.mod());

    it = Range{constMods}.find_if(
        [](auto pair) { return pair.first == ConstModType::ONE; });
    if (it == constMods.end())
      report_fatal_error("no constant one std cell");
    build.buildInstrRaw(HW_STDCELL_INSTANCE, 2)
        .addRef(oneW)
        .other()
        .addRef(it->second.mod());
    return std::pair(zeroW, oneW);
  }

  WireRef makeConstant(ConstantRef val, WireRef zeroW, WireRef oneW) {
    if (val == "1'b0"_bv)
      return zeroW;
    if (val == "1'b1"_bv)
      return oneW;

    auto ib = build.buildInstrRaw(HW_CONCAT, 1 + val.getNumBits());
    WireRef w = ctx.getStore<Wire>().create(val.getNumBits());
    ib.addRef(w).other();

    // note: could use repeat to reduce operand count

    for (unsigned i = val.getNumBits(); i-- > 0;) {
      if (val.getBit(i) == FourState::S1)
        ib.addRef(oneW);
      else
        // unknown bits become zero (anything that we can gain from them should
        // have been done before.)
        ib.addRef(zeroW);
    }

    return w;
  }

  void runOnProcess(ProcessIRef proc) {
    build.setInsertPoint(proc.block().begin());
    auto [zeroW, oneW] = getConstant01Wires();

    for (auto instr : proc.block().unordered()) {
      // only map constants for stdcells, stores and concats
      if (!instr.isOpc(HW_STDCELL_INSTANCE, HW_STORE, HW_CONCAT))
        continue;
      for (auto use : instr.others()) {
        if (!use->is<ConstantRef>())
          continue;
        build.setInsertPoint(instr);
        auto w = makeConstant(use->as<ConstantRef>(), zeroW, oneW);
        use.replace(w);
      }
    }
  }

  void runOnModule(ModuleIRef mod) {
    for (auto proc : mod.procs())
      runOnProcess(proc);
  }

public:
  void runWrapper(auto &&runFunc) {
    findConstantModules();
    runFunc();
  }
  void run() {
    runWrapper([&] {
      for (auto mod : ctx.getCtx<HWDialectContext>().activeModules())
        runOnModule(mod.iref());
    });
  }
  void runModule(ModuleIRef mod) {
    runWrapper([&] { runOnModule(mod); });
  }
  void runProcess(ProcessIRef proc) {
    runWrapper([&] { runOnProcess(proc); });
  }

  static constexpr auto runFuncs = std::make_tuple(
      &ConstantMappingPass::runProcess, &ConstantMappingPass::runModule,
      &ConstantMappingPass::run);

  auto make(Context &ctx) { return ConstantMappingPass(ctx); }
  explicit ConstantMappingPass(Context &ctx) : ctx(ctx), build(ctx) {}
};

}; // namespace dyno
