#pragma once

#include "dyno/CFG.h"
#include "dyno/ObjMap.h"
#include "hw/DeepCopy.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/IDs.h"
#include "hw/Register.h"
namespace dyno {
class ModuleInlinePass {
  HWContext &ctx;
  DeepCopier copier;

  ObjMapVec<Module, bool> isSubModule;
  auto markSubModules() {
    isSubModule.resize(ctx.getModules().numIDs());
    for (auto mod : ctx.getModules()) {
      if (mod->defUse.getNumUses() != 0)
        isSubModule[mod] = true;
    }

    SmallVec<ModuleIRef, 4> rv;
    for (auto [obj, hasInst] : isSubModule) {
      if (hasInst)
        continue;
      rv.emplace_back(ctx.getModules().resolve(obj).iref());
    }
    return rv;
  }

  void inlineInstance(HWInstrRef instance) {
    // copy module over (converting ports to regs)
    // replace port regs with module regs and delete
    ModuleIRef parentMod = instance.parentMod(ctx);
    auto modToInline = instance.operand(0)->as<ModuleRef>().iref();

    uint portIndex = 0;
    auto inlineHook = [&](DeepCopier *self, InstrRef src,
                          BlockRef_iterator<true> dstIt) {
      if (src.isOpc(HW_INPUT_REGISTER_INSTR, HW_OUTPUT_REGISTER_INSTR,
                    HW_INOUT_REGISTER_INSTR, HW_REF_REGISTER_INSTR)) {
        auto portReg = instance.operand(1 + portIndex++)->as<RegisterRef>();
        self->oldToNewMap.insert(src.def(0)->fat(), portReg);
        return true;
      }
      if (src.isOpc(HW_REGISTER_INSTR)) {
        HWInstrBuilder build{ctx};
        build.setInsertPoint(parentMod.regs_end());
        auto reg = build.buildRegister(src.as<RegisterIRef>().oref().getNumBits());
        self->oldToNewMap.insert(src.def(0)->fat(), reg);
        return true;
      }

      return false;
    };

    // todo: move
    copier.deepCopyInstrs(modToInline.block().begin(), instance.iter(ctx),
                          inlineHook);
    HWInstrBuilder{ctx}.destroyInstr(instance);
  }

  void deleteRec(ModuleIRef ref) {
    if (!ctx.getInstrs().exists(ref))
      return;
    for (auto use : ref.mod()->defUse.uses()) {
      deleteRec(HWInstrRef{use.instr()}.parentMod(ctx));
    }
    HWInstrBuilder{ctx}.destroyInstr(ref);
  }

public:
  void run() {
    markSubModules();
    SmallVec<HWInstrRef, 32> worklist;
    for (auto mod : ctx.getModules()) {
      for (auto use : mod->defUse.uses()) {
        auto modUsed = HWInstrRef{use.instr()}.parentMod(ctx);
        if (!isSubModule[modUsed.mod()]) {
          worklist.emplace_back(use.instr());
        }
      }
    }
    while (!worklist.empty()) {
      auto inst = worklist.pop_back_val();
      inlineInstance(inst);
    }

    for (auto mod : ctx.getModules()) {
      if (isSubModule[mod])
        deleteRec(mod.iref());
    }
  }

public:
  ModuleInlinePass(HWContext &ctx) : ctx(ctx), copier(ctx) {}
};
}; // namespace dyno
