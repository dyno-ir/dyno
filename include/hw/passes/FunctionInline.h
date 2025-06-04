#pragma once
#include "dyno/CFG.h"
#include "dyno/CustomInstr.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/DeepCopy.h"
#include "hw/HWAbstraction.h"
#include "hw/HWInstr.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/Register.h"
#include "op/Function.h"
#include "op/IDs.h"
#include "support/Debug.h"
#include "support/DenseMap.h"
#include "support/ErrorRecovery.h"

namespace dyno {

using TaggedCallRef = CustomInstrRef<CallInstrRef, uint64_t>;

class FunctionInlinePass {
  HWContext &ctx;
  DeepCopier copier;

  void runOnModule(ModuleIRef mod) {

    SmallVec<CallInstrRef, 8> worklist;
    for (FunctionIRef instr : mod.funcs()) {
      for (auto call : instr.func().uses()) {
        auto parent = HWInstrRef{call.instr()}.parent(ctx);
        if (!parent.is<FunctionIRef>())
          worklist.emplace_back(call.instr());
        TaggedCallRef{call.instr()}.get() = 0;
      }
    }

    SmallVec<FunctionIRef, 8> callStack;

    while (!worklist.empty()) {
      auto callInstr = worklist.pop_back_val();
      auto dstIter = BlockRef_iterator<true>{HWInstrRef{callInstr}.iter(ctx)};

      if (auto parent = HWInstrRef{callInstr}.parent(ctx)) {
        if (auto asFunc = parent.dyn_as<FunctionIRef>()) {
          continue;
        }
      }

      auto funcInstr = callInstr.func().iref();

      DEBUG(dbgs() << "\n\n\ninlining\n"; dumpInstr(callInstr);
            dbgs() << "context:"; dumpCtx(ctx);)

      SmallVec<RegisterRef, 2> returnRegs{callInstr.getNumRetvals()};
      SmallVec<RegisterRef, 4> paramRegs{callInstr.getNumParams()};

      HWInstrBuilderStack build{ctx};
      build.setInsertPoint(mod.regs_end());

      // create regs for param/retval (SSA construction will get rid of these
      // usually.)
      for (size_t i = 0; i < paramRegs.size(); i++) {
        auto param = callInstr.params().begin()[i];
        if (auto asReg = param->dyn_as<RegisterRef>())
          paramRegs[i] = asReg;
        else {
          // params may also be SSA values in which case we have to create
          // a register on the fly
          auto asValue = param->as<HWValue>();
          paramRegs[i] = build.buildRegister(asValue.getNumBits());
          build.pushInsertPoint(dstIter);
          build.buildStore(paramRegs[i], asValue);
          build.popInsertPoint();
        }
      }
      for (size_t i = 0; i < returnRegs.size(); i++)
        returnRegs[i] = build.buildRegister(
            callInstr.retvals().begin()[i]->as<WireRef>()->numBits);

      for (auto [i, def] : callInstr.defs().enumerate()) {
        auto loadInstr = InstrRef{ctx.getInstrs().create(2, HW_LOAD)};
        InstrBuilder build{loadInstr};
        build.addRef(def->fat()).other().addRef(returnRegs[i]);
        dstIter.succ().insertPrev(loadInstr);
        def.replace(FatDynObjRef<>{nullref});
      }
      bool calledAny = false;
      unsigned paramIdx = 0;
      copier.deepCopyInstrs(
          funcInstr.getBlock().begin(), dstIter,
          [&](DeepCopier *self, InstrRef src, BlockRef_iterator<true> dstIt) {
            if (src.isOpc(OP_PARAM)) {
              self->oldToNewMap.insert(src.def(0)->fat(), paramRegs[paramIdx]);
              paramIdx++;
              return true;
            }
            if (src.isOpc(OP_RETURN)) {
              assert(
                  BlockRef_iterator<true>{HWInstrRef{src}.iter(ctx)}.succ() ==
                      funcInstr.getBlock().end() &&
                  "expected return to be last instruction");
              HWInstrBuilder build{self->ctx, dstIt};
              for (size_t i = 0; i < src.getNumOperands(); i++)
                build.buildStore(
                    returnRegs[i],
                    self->oldToNewMap.find(src.operand(i)->fat()).val());
              return true;
            }
            if (src.isOpc(OP_CALL)) {
              auto newCall = copier.copyInstr(src, dstIt, copier.emptyCallback);
              worklist.emplace_back(newCall);
              if (!calledAny) {
                // mark to pop this entry when done.
                TaggedCallRef{newCall}.get() =
                    1 + TaggedCallRef{callInstr}.get();
                TaggedCallRef{callInstr}.get() = 0;
              }
              calledAny = true;
              return true;
            }
            return false;
          });

      if (calledAny) {
        if (std::find(callStack.begin(), callStack.end(), funcInstr) !=
            callStack.end()) {
          report_fatal_error("mutual recursion");
        }
        callStack.emplace_back(funcInstr);
        assert(TaggedCallRef{callInstr}.get() == 0);
      } else {
        for (uint64_t i = 0; i < TaggedCallRef{callInstr}.get(); i++) {
          callStack.pop_back();
        }
      }

      build.destroyInstr(callInstr);
    }
  }

public:
  void run() {
    for (auto mod : Range{ctx.getModules()}.as<ModuleRef>()) {
      runOnModule(mod.iref());
    }
  }
  explicit FunctionInlinePass(HWContext &ctx) : ctx(ctx), copier(ctx) {}
};
}; // namespace dyno
