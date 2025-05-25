#pragma once
#include "dyno/CFG.h"
#include "dyno/CustomInstr.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/DeepCopy.h"
#include "hw/HWAbstraction.h"
#include "hw/IDs.h"
#include "hw/Register.h"
#include "op/Function.h"
#include "op/IDs.h"
#include "support/DenseMap.h"

namespace dyno {

using CustFuncRef = CustomInstrRef<FunctionIRef, uint64_t>;

class FunctionInlinePass {
  HWContext &ctx;
  DeepCopier copier;

  void runOnModule(ModuleIRef mod) {

    SmallVec<FunctionIRef, 8> worklist;
    for (FunctionIRef instr : mod.funcs()) {
      worklist.emplace_back(instr);
      CustFuncRef{instr}.emplace(0);
    }

    while (!worklist.empty()) {
      FunctionIRef instr = worklist.pop_back_val();

      for (auto useIt = instr.func().use_begin();
           useIt != instr.func().use_end(); useIt++) {
        auto &use = *useIt;
        assert(use.instr().isOpc(OP_CALL) && "function used in unknown instr");
        auto callInstr = CallInstrRef{use.instr()};
        auto dstIter = BlockRef_iterator<true>{HWInstrRef{callInstr}.iter(ctx)};

        if (auto parent = HWInstrRef{callInstr}.parent(ctx)) {
          if (auto asFunc = parent.dyn_as<FunctionIRef>()) {
            continue;
          }
        }

        // mark as visited
        CustFuncRef{instr}.emplace(1);

        // std::cerr << "\n\n\ninlining\n";
        // dumpInstr(instr);
        // std::cerr << "context:";
        // dumpCtx(ctx);

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
            // params may also be wires in which case we have to create
            // a register on the fly
            auto asWire = param->as<WireRef>();
            paramRegs[i] = build.buildRegister(asWire->numBits);
            build.pushInsertPoint(dstIter);
            build.buildStore(paramRegs[i], asWire);
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
        }

        unsigned paramIdx = 0;
        copier.deepCopyInstrs(
            instr.getBlock().begin(), dstIter,
            [&](DeepCopier *self, InstrRef src, BlockRef_iterator<true> dstIt) {
              if (src.isOpc(OP_PARAM)) {
                self->oldToNewMap.insert(src.def(0)->fat(),
                                         paramRegs[paramIdx]);
                paramIdx++;
                return true;
              }
              if (src.isOpc(OP_RETURN)) {
                assert(
                    BlockRef_iterator<true>{HWInstrRef{src}.iter(ctx)}.succ() ==
                        instr.getBlock().end() &&
                    "expected return to be last instruction");
                HWInstrBuilder build{self->ctx, dstIt};
                for (size_t i = 0; i < src.getNumOperands(); i++)
                  build.buildStore(
                      returnRegs[i],
                      self->oldToNewMap.find(src.operand(i)->fat()).val());
                return true;
              }
              if (src.isOpc(OP_CALL)) {
                auto otherFunc = src.other(0)->as<FunctionRef>().iref();
                assert(CustFuncRef{otherFunc}.get() == 0 &&
                       "mutually recursive");
                worklist.emplace_back(otherFunc);
                return false;
              }
              return false;
            });

        build.destroyInstr(callInstr);
        --useIt;
      }
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
