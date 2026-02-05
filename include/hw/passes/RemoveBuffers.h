#pragma once

#include "dyno/Context.h"
#include "dyno/DestroyMap.h"
#include "dyno/HierBlockIterator.h"
#include "dyno/Pass.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Module.h"
#include "support/Debug.h"
namespace dyno {

class RemoveBuffersPass : public Pass<RemoveBuffersPass> {
  Context &ctx;
  ObjMapVec<Module, bool> isBuffer;
  DestroyMap<Instr> destroyMap;

  static bool checkIfModuleIsBuf(ModuleIRef iref) {
    auto mod = iref.mod();
    if (iref.block().size() != 3)
      return false;
    if (mod->ports.size() != 2)
      return false;
    auto it = iref.ports_end();
    if (!it.instr().isOpc(HW_COMB_PROCESS_DEF))
      return false;
    auto proc = it.instr().as<ProcessIRef>();
    if (proc.block().size() != 2)
      return false;
    auto blockIt = proc.block().begin();
    if (blockIt == proc.block().end() || !blockIt.instr().isOpc(HW_LOAD))
      return false;
    auto load = blockIt.instr().as<LoadIRef>();
    ++blockIt;
    if (blockIt == proc.block().end() || !blockIt.instr().isOpc(HW_STORE))
      return false;
    auto store = blockIt.instr().as<StoreIRef>();
    ++blockIt;
    if (blockIt != proc.block().end())
      return false;
    if (load.value() != store.value())
      return false;
    if (load.reg() == store.reg())
      return false;
    if (!load.isFullReg() || !store.isFullReg())
      return false;
    if (store.hasTrigger())
      return false;
    return true;
  }

public:
  auto make(Context &ctx) { return RemoveBuffersPass(ctx); }
  explicit RemoveBuffersPass(Context &ctx) : ctx(ctx) {}

  void runOnModule(ModuleIRef mod) {
    for (auto instr : HierBlockRange{mod.block()}) {
      if (!instr.isOpc(HW_STDCELL_INSTANCE))
        continue;
      auto mod = instr.other(0)->as<ModuleRef>();
      if (!isBuffer[mod])
        continue;
      instr.def(0)->as<WireRef>().replaceAllUsesWith(
          instr.other(1)->as<WireRef>());
      destroyMap.mark(instr);
    }
  }

  void findBuffers() {
    isBuffer.clear();
    isBuffer.resize(ctx.getStore<Module>().numIDs());
    for (auto mod : ctx.getStore<Module>()) {
      bool buf = checkIfModuleIsBuf(mod.iref());
      isBuffer[mod] = buf;
      if (buf) {
        DYNO_DBG("RemoveBuffersPass", {
          dbgs() << "identified buffer: ";
          dumpInstr(mod.iref(), ctx);
        });
      }
    }
  }

  void runWrapper(auto &&runFunc) {
    findBuffers();
    destroyMap.clear();
    destroyMap.resize(ctx.getStore<Instr>().numIDs());

    runFunc();

    destroyMap.apply(ctx.getStore<Instr>(), [&](InstrRef ref) {
      HWInstrBuilder{ctx}.destroyInstr(ref);
    });
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

  static constexpr auto runFuncs =
      std::make_tuple(&RemoveBuffersPass::runModule, &RemoveBuffersPass::run);
};

}; // namespace dyno
