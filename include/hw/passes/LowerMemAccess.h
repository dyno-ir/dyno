#pragma once

#include "dyno/Context.h"
#include "dyno/Pass.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/LoadStore.h"
#include "hw/Module.h"
#include "support/ErrorRecovery.h"
namespace dyno {

class LowerMemAccessPass : public Pass<LowerMemAccessPass> {
  Context &ctx;
  HWInstrBuilder rbuild;
  HWInstrBuilder build;
  SmallVec<ObjRef<Instr>, 32> destroyList;

  HWValue runOnStore(MemStoreIRef st, HWValue memFlat) {
    auto dl = st.port()->delay;
    assert(dl > 0 && "todo: comb store port"); // need to detect globally and
                                               // make latch memory (final
                                               // memFlat store is not deferred)

    WireRef en = st.en();
    // we can't store a pointer currently, if we add that make this store a
    // single ptr rather than all indexes.
    SmallVec<ObjRef<Wire>, 4> addrIdxs(st.terms().transform(
        [](size_t, auto t) { return t.getIdx().template as<WireRef>(); }));
    WireRef value = st.value();

    struct DelayStage {
      ObjRef<Register> en;
      ObjRef<Register> value;
      SmallVec<ObjRef<Register>, 2> addrIdxs;
    };
    SmallVec<DelayStage, 4> stages;
    stages.reserve(dl - 1);

    // delay line
    for (uint32_t i = 1; i < dl; i++) {
      auto &stage = stages.emplace_back(
          DelayStage{.en = rbuild.buildRegister(1),
                     .value = rbuild.buildRegister(st.value().getNumBits()),
                     .addrIdxs = IntRange{st.terms().size()}.transform(
                         [&](...) { return rbuild.buildRegister(32); })});
      build.buildStore(ctx.resolve(stage.en), en, true, st.trigger().iref());
      en = build.buildLoad(ctx.resolve(stage.en));
      build.buildStore(ctx.resolve(stage.value), value, true,
                       st.trigger().iref());
      value = build.buildLoad(ctx.resolve(stage.value));
      for (auto [reg, wire] :
           Range{stage.addrIdxs}.resolve(ctx).zip(addrIdxs)) {
        build.buildStore(reg, ctx.resolve(wire), true, st.trigger().iref());
        wire = build.buildLoad(reg);
      }
    }

    auto inserted = build.buildInsert(
        memFlat, value, st.base(),
        st.terms().transform([&](size_t i, auto old) -> AddressGenTerm {
          return {ctx.resolve(addrIdxs[i]), old.getFact(), old.getMax()};
        }));
    if (!st.hasEn())
      return inserted;
    return build.buildMux(en, inserted, memFlat);
  }

  HWValue runOnLoad(MemLoadIRef ld, HWValue memFlat) {
    auto dl = ld.port()->delay;

    // todo: forwards
    HWValue val =
        build.buildSplice(memFlat, ld.getLen(), ld.base(), ld.terms());

    // delay line
    for (uint32_t i = 0; i < dl; i++) {
      auto reg = rbuild.buildRegister(val.getNumBits());
      val = build.buildStore(reg, val, true, ld.trigger().iref());
    }

    return val;
  }

  void runOnRegister(RegisterIRef reg) {
    if (reg.memStores().empty() && reg.memLoads().empty())
      return;
    assert(reg.loads().empty() && reg.stores().empty());

    HWValue memFlat = nullref;
    TriggerRef trig = nullref;

    for (auto [front, access] :
         reg.oref()
             .uses()
             .transform([](size_t, auto use) { return use.instr(); })
             .mark_front()) {
      build.setInsertPoint(access);
      if (front)
        memFlat = build.buildLoad(reg.oref());

      switch (*access.getDialectOpcode()) {
      case *HW_MEM_STORE: {
        memFlat = runOnStore(access.as<MemStoreIRef>(), memFlat);
        auto t = access.as<MemStoreIRef>().trigger();
        if (t) {
          if (trig && trig != t)
            report_fatal_error("expected same trigger");
          trig = t;
        }
        destroyList.emplace_back(access);
      } break;
      case *HW_MEM_LOAD: {
        auto ldVal = runOnLoad(access.as<MemLoadIRef>(), memFlat);
        auto t = access.as<MemLoadIRef>().trigger();
        if (t) {
          if (trig && trig != t)
            report_fatal_error("expected same trigger");
          trig = t;
        }
        destroyList.emplace_back(access);
        access.as<MemLoadIRef>().value().replaceAllUsesWith(ldVal);
      } break;
      }
    }

    build.buildStore(reg.oref(), memFlat, true, trig.iref());
    for (auto e : Range{destroyList}.resolve(ctx))
      build.destroyInstr(e);
    destroyList.clear();
  }

  void runOnModule(ModuleIRef mod) {
    build.setInsertPoint(mod.regs_end());
    for (auto reg : mod.regs()) {
      runOnRegister(reg);
    }
  }

public:
  void runWrapper(auto &&runFunc) { runFunc(); }

  void runModule(ModuleIRef mod) {
    runWrapper([&] { runOnModule(mod); });
  }

  void run() {
    runWrapper([&] {
      for (auto mod : ctx.getCtx<HWDialectContext>().activeModules())
        runOnModule(mod.iref());
    });
  }

  static constexpr auto runFuncs =
      mk_tuple(&LowerMemAccessPass::run, &LowerMemAccessPass::runModule);

  explicit LowerMemAccessPass(Context &ctx)
      : ctx(ctx), rbuild(ctx), build(ctx) {}
  auto make(Context &ctx) { return LowerMemAccessPass{ctx}; }
};
}; // namespace dyno
