#pragma once

#include "dyno/CFG.h"
#include "dyno/Context.h"
#include "dyno/DestroyMap.h"
#include "dyno/ObjMap.h"
#include "dyno/Pass.h"
#include "hw/DeepCopy.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/IDs.h"
#include "hw/Register.h"
#include "support/Debug.h"
namespace dyno {
class ModuleInlinePass : public Pass<ModuleInlinePass> {
  Context &ctx;
  DeepCopier copier;
  ObjMapVec<Module, bool> isTopModule;
  SmallVec<HWInstrRef, 32> worklist;

  void inlineInstance(HWInstrRef instance) {
    // copy module over (converting ports to regs)
    // replace port regs with module regs and delete
    ModuleIRef parentMod = instance.parentMod(ctx);
    auto modToInline = instance.operand(0)->as<ModuleRef>().iref();

    unsigned portIndex = 0;
    auto inlineHook = [&](DeepCopier *self, InstrRef src,
                          BlockRef_iterator<true> dstIt) {
      if (src.isOpc(HW_INPUT_REGISTER_DEF, HW_OUTPUT_REGISTER_DEF,
                    HW_INOUT_REGISTER_DEF, HW_REF_REGISTER_DEF)) {
        auto portReg = instance.operand(1 + portIndex++)->as<RegisterRef>();
        self->oldToNewMap.insert(src.def(0)->fat(), portReg);
        return true;
      }
      if (src.isOpc(HW_REGISTER_DEF)) {
        HWInstrBuilder build{ctx};
        build.setInsertPoint(parentMod.regs_end());
        auto reg =
            build.buildRegister(src.as<RegisterIRef>().oref().getNumBits());
        self->oldToNewMap.insert(src.def(0)->fat(), reg);
        return true;
      }
      if (src.isOpc(HW_INSTANCE)) {
        auto instr = copier.copyInstr(src, dstIt);
        worklist.emplace_back(instr);
        return true;
      }

      return false;
    };

    // todo: move
    copier.deepCopyInstrs(modToInline.block().begin(), instance.iter(ctx),
                          inlineHook);
    HWInstrBuilder{ctx}.destroyInstr(instance);
  }

  DestroyMap<Module> destroyMap;
  void deleteRec(ModuleIRef ref) {
    if (!ctx.getStore<Instr>().exists(ref))
      return;
    for (auto use : ref.mod()->defUse.uses()) {
      deleteRec(HWInstrRef{use.instr()}.parentMod(ctx));
    }
    DYNO_DBG("ModuleInline", {
      dbgs() << "deleting module: \"";
      dbgs() << ref.mod()->name;
      dbgs() << "\"\n";
    })
    destroyMap.mark(ref.mod());
  }

public:
  void run() {
    destroyMap.clear();
    destroyMap.resize(ctx.getStore<Module>().numIDs());
    worklist.clear();
    isTopModule.clear();
    isTopModule.resize(ctx.getStore<Module>().numIDs());

    for (auto mod : ctx.getCtx<HWDialectContext>().activeModules()) {
      if (mod.getNumUses() == 0) {
        isTopModule[mod] = 1;
      } else {
        for (auto use : mod->defUse.uses()) {
          auto modUsed = HWInstrRef{use.instr()}.parentMod(ctx);
          if (isTopModule[modUsed.mod()]) {
            worklist.emplace_back(use.instr());
          }
        }
      }
    }
    while (!worklist.empty()) {
      auto inst = worklist.pop_back_val();
      inlineInstance(inst);
    }

    for (auto mod : ctx.getCtx<HWDialectContext>().activeModules()) {
      if (!isTopModule[mod]) {
        deleteRec(mod.iref());
      }
    }
    HWInstrBuilder build{ctx};
    destroyMap.apply(ctx.getStore<Module>(),
                     [&](ModuleRef mod) { build.destroyInstr(mod.iref()); });
  }

  static constexpr auto runFuncs = std::make_tuple(&ModuleInlinePass::run);

public:
  auto make(Context &ctx) { return ModuleInlinePass(ctx); }
  explicit ModuleInlinePass(Context &ctx) : ctx(ctx), copier(ctx) {}
};
}; // namespace dyno
