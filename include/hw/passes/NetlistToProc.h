#pragma once

#include "dyno/CFG.h"
#include "dyno/Context.h"
#include "dyno/Obj.h"
#include "dyno/Pass.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/IDs.h"
#include "hw/Module.h"

namespace dyno {

// remove wire forward references by introducing loopback registers
class NetlistToProcPass : public Pass<NetlistToProcPass> {
  Context &ctx;
  HWInstrBuilder regBuild;

  void runOnProcess(ProcessIRef proc) {
    ObjMapVec<Wire, bool> seen;
    seen.resize(ctx.getStore<Wire>().numIDs());
    SmallDenseMap<ObjRef<Wire>, ObjRef<Register>, 64> map;
    HWInstrBuilder build{ctx};

    for (auto instr : proc.block()) {
      for (auto use : instr.others()) {
        if (!use->is<WireRef>())
          continue;
        auto wire = use->as<WireRef>();
        if (seen[wire])
          continue;

        auto reg =
            map.findOrInsert(
                   wire,
                   [&]() { return regBuild.buildRegister(*wire.getNumBits()); })
                .second.val();

        build.setInsertPoint(instr);
        use.replace(build.buildLoad(ctx.resolve(reg)));
      }

      for (auto def : instr.defs())
        if (auto asWire = def->dyn_as<WireRef>()) {
          seen[asWire] = 1;

          if (auto it = map.find(asWire); it != map.end()) {
            build.setInsertPoint(
                BlockRef_iterator<true>{ctx.getCFG()[instr]}.succ());
            build.buildStore(ctx.resolve(it.val()), asWire);
            map.erase(it);
          }
        }
    }

    build.changeProcessType(HW_COMB_PROCESS_DEF, proc);
  }

  void runOnModule(ModuleIRef mod) {
    regBuild.setInsertPoint(mod.regs_end());
    for (auto proc : mod.procs().earlyincr()) {
      runOnProcess(proc);
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
      mk_tuple(&NetlistToProcPass::run, &NetlistToProcPass::runModule);

  explicit NetlistToProcPass(Context &ctx) : ctx(ctx), regBuild(ctx) {}
  auto make(Context &ctx) { return NetlistToProcPass{ctx}; }
};
}; // namespace dyno
