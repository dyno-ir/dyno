#pragma once
#include "dyno/Constant.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/SensList.h"
#include "hw/Wire.h"
#include "hw/analysis/KnownBits.h"
#include "hw/analysis/MuxTree.h"
#include "hw/passes/MuxTreeOptimization.h"
#include "support/ErrorRecovery.h"

namespace dyno {

class FlipFlopInferencePass {
  HWContext &ctx;
  MuxtreeAnalysis muxTreeAnalysis;
  HWInstrBuilderStack build;
  ConstantBuilder cbuild;

  std::pair<ConstantRef, uint>
  findReset(StoreIRef store,
            ArrayRef<std::pair<RegisterRef, bool>> resetCandidates,
            MuxTree *muxTree) {
    if (!muxTree)
      return std::make_pair(nullref, 0);

    SmallVec<std::pair<SmallBoolExprCNF, uint>, 2> resetExprs;
    if (auto asConst = store.value().dyn_as<ConstantRef>())
      return std::pair(asConst, 0);

    auto &mtree = *muxTree;
    for (auto [i, cond] : Range{mtree.conditions}.enumerate()) {
      auto instr = ctx.getWires().resolve(cond.wire).getDefI();
      // not sure if this is 100% to spec in weird cases. we might want to
      // assert that reset gets set before this load (if it gets set in the same
      // process.)
      if (!instr.isOpc(HW_LOAD))
        continue;

      for (auto [j, cand] : Range{resetCandidates}.enumerate()) {
        if (cand.first != LoadIRef{instr}.reg())
          continue;

        // could reset possibly inverted?
        auto &[expr, idx] = resetExprs.emplace_back();
        expr.literals.emplace_back(
            BoolExprLiteral{uint16_t(i), !cand.second, 1});
        idx = j;
      }
    }

    for (auto &entry : mtree.entries) {
      if (!entry.output.is<ObjRef<Constant>>())
        continue;
      auto outputConst = ctx.getConstants().resolve(entry.output);
      for (auto [i, rstExpr] : Range{resetExprs}.enumerate()) {
        SmallBoolExprCNF copy = entry.expr;
        auto sat = copy.simplifyWith(rstExpr.first, mtree.conditions.size());
        if (sat.has_value() && sat.value() == true)
          return std::make_pair(outputConst, rstExpr.second);
      }
    }

    return std::make_pair(nullref, 0);
  }

  std::pair<ConstantRef, uint>
  findReset2(StoreIRef store,
             ArrayRef<std::pair<RegisterRef, bool>> resetCandidates) {
    KnownBitsAnalysis knownBits;
    for (auto [i, cand] : Range{resetCandidates}.enumerate()) {
      auto [reg, pol] = cand;
      SmallVec<std::pair<ObjRef<Wire>, BigInt>, 4> assignments;
      for (auto use : reg.uses()) {
        if (!use.instr().isOpc(HW_LOAD))
          continue;
        assignments.emplace_back(use.instr().def(0)->as<WireRef>(),
                                 BigInt::fromU64(pol, 1));
      }

      auto val = knownBits.getKnownBitsWith(store.value(), assignments);

      if (!val.getIs4S())
        return std::make_pair(ctx.constBuild().val(val).get(), i);
    }
    return std::make_pair(nullref, 0);
  }

  bool isQLoad(RegisterIRef q, HWValue value) {
    if (!value.is<WireRef>())
      return false;
    auto wire = value.as<WireRef>();
    auto instr = wire.getDefI();
    if (!instr.isOpc(HW_LOAD))
      return false;
    auto load = instr.as<LoadIRef>();
    if (load.reg() != q.oref() || !load.isFullReg())
      return false;
    return true;
  }

  Optional<uint> findClockEnable(RegisterIRef q, StoreIRef store,
                                 MuxTree *muxTree) {
    if (!muxTree)
      return nullopt;
    if (isQLoad(q, store.value().as<HWValue>()))
      return 0;
    if (store.value().as<WireRef>().getNumUses() != 1)
      return nullopt;
    for (auto [i, entry] : Range{muxTree->entries}.enumerate()) {
      auto ref = ctx.resolveObj(entry.output);
      if (isQLoad(q, ref.as<HWValue>()))
        return i;
    }
    return nullopt;
  }

  std::optional<MuxTree> getMuxTree(StoreIRef store) {
    std::optional<MuxTree> muxTree = std::nullopt;
    if (auto asConst = store.value().dyn_as<ConstantRef>())
      return std::nullopt;
    auto wire = store.value().as<WireRef>();
    auto instr = wire.getDefI();
    if (!instr.isOpc(HW_MUX))
      return std::nullopt;
    auto mtree = muxTreeAnalysis.analyzeMuxTree(
        store.value().as<WireRef>().getDefI(), false, false);
    if (!mtree)
      return std::nullopt;
    muxTreeAnalysis.simplifyConditions(&*mtree);
    muxTreeAnalysis.printMuxTree(ctx, &*mtree);
    muxTreeAnalysis.dedupeMuxTreeOutputs(&*mtree);
    muxTreeAnalysis.printMuxTree(ctx, &*mtree);
    return mtree;
  }

  bool runOnReg(ModuleIRef module, RegisterIRef reg) {
    // Find single store. Multiple stores is the domain of SSA construct, so
    // return.
    StoreIRef storeI = nullref;
    for (auto use : reg.oref().uses()) {
      if (use.instr().isOpc(HW_STORE_DEFER)) {
        if (storeI)
          return false;
        storeI = use.instr();
      }
    }
    if (!storeI)
      return false;

    build.setInsertPoint(module.regs_end());

    // assume ranges lowered
    assert(storeI.isFullReg() && "range not lowered?");
    assert(storeI.hasTrigger() && "no trigger?");
    auto trigger = storeI.trigger().oref();
    if (trigger->size() > 3)
      report_fatal_error("too many sensitivities on flip flop");
    bool hasReset = trigger->size() != 1;
    bool hasIFF =
        trigger->size() == 3 && (trigger->getMode(1) == SensMode::IFF ||
                                 trigger->getMode(1) == SensMode::IFFN);
    if (!hasIFF && trigger->size() == 3)
      report_fatal_error("too many sensitivities!");

    std::pair<RegisterRef, bool> clkReg;
    std::pair<RegisterRef, bool> rstReg;

    HWValue dValue = storeI.value().as<HWValue>();
    ConstantRef resetValue;

    auto get = [&](uint i) -> std::pair<RegisterRef, bool> {
      return {trigger.iref().other(i)->as<RegisterRef>(),
              trigger->getMode(i) == SensMode::POSEDGE};
    };

    auto muxTree = getMuxTree(storeI);
    if (!muxTree) {
      DEBUG("FlipFlopInference", {
        dbgs() << "could not get mux tree, skipping enable inference for: ";
        dumpInstr(reg, ctx);
        dbgs() << "store value:\n";
        dumpInstr(storeI, ctx);
      })
    }
    auto *muxTreePtr = muxTree ? &*muxTree : nullptr;

    HWValue undef = cbuild.undef(*dValue.getNumBits()).get();

    RegisterRef clkEn = nullref;
    if (auto clkEnIdx =
            findClockEnable(storeI.reg().iref(), storeI, muxTreePtr)) {
      clkEn = build.buildRegister(1);
      build.pushInsertPoint(storeI);
      auto val = MuxTreeOptimizationPass::getExprVal(
          build, muxTreePtr, muxTreePtr->entries[*clkEnIdx].expr);
      build.buildStore(clkEn, val);
      dValue = build.buildMux(val, undef, dValue);
      build.popInsertPoint();
    }

    if (hasReset) {
      SmallVec<std::pair<RegisterRef, bool>, 2> resetCandidates;

      if (hasIFF) {
        resetCandidates = {get(2)};
      } else {
        resetCandidates = {get(0), get(1)};
      }
      uint resetIndex = 0;
      std::tie(resetValue, resetIndex) = findReset2(storeI, resetCandidates);
      if (!resetValue)
        report_fatal_error("reset sensitivity but no reset value found");
      rstReg = resetCandidates[resetIndex];
      clkReg = hasIFF ? resetCandidates[0] : (resetCandidates[1 - resetIndex]);

      build.pushInsertPoint(storeI);
      dValue = build.buildMux(build.buildLoad(rstReg.first),
                              rstReg.second ? undef : dValue,
                              !rstReg.second ? undef : dValue);
      build.popInsertPoint();
    } else
      clkReg = get(0);

    auto dReg = build.buildRegister(reg.getNumBits());

    // ignore clk en for now.
    auto ib = build.buildInstrRaw(HW_FLIP_FLOP, 4 + !!clkEn * 2 + hasReset * 3);
    ib.other()
        .addRef(clkReg.first)
        .addRef(ConstantRef::fromBool(clkReg.second))
        .addRef(dReg)
        .addRef(reg.oref());
    if (clkEn)
      ib.addRef(clkEn).addRef(ConstantRef::fromBool(0));
    if (hasReset)
      ib.addRef(rstReg.first)
          .addRef(ConstantRef::fromBool(rstReg.second))
          .addRef(resetValue);

    build.setInsertPoint(storeI);
    build.buildStore(dReg, dValue);
    build.destroyInstr(storeI);

    return true;
  }

  void runOnModule(ModuleIRef module) {
    SmallVec<RegisterIRef, 16> regs;
    for (auto reg : module.regs()) {
      regs.emplace_back(reg);
    }
    for (auto reg : regs)
      runOnReg(module, reg);
  }

public:
  void run() {
    for (auto mod : ctx.activeModules()) {
      runOnModule(mod.iref());
    }
  }

  explicit FlipFlopInferencePass(HWContext &ctx)
      : ctx(ctx), build(ctx), cbuild(ctx.getConstants()) {}
}; // namespace dyno
}; // namespace dyno
