#pragma once

#include "dyno/Constant.h"
#include "hw/AutoDebugInfo.h"
#include "hw/DeepCopy.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/Wire.h"
#include "hw/analysis/SCFTraversal.h"
#include "op/IDs.h"
#include "op/StructuredControlFlow.h"
#include "support/ErrorRecovery.h"
namespace dyno {

class LinearizeControlFlowPass {
  HWContext &ctx;
  DeepCopier copier;
  HWInstrBuilder build;
  SmallVec<InstrRef, 32> worklist;
  AutoCopyDebugInfoStack autoDebugInfo;

  // if (auto asConst = cond->dyn_as<ConstantRef>();
  //     asConst && asConst.valueEquals(0)) {

  //   auto copyHook = [&](DeepCopier* self, InstrRef old,
  //   BlockRef_iterator<true> it) {
  //     if (old.isOpc(OP_UNYIELD)) {
  //       for (auto def : old.defs()) {
  //         def->as<WireRef>().replaceAllUsesWith()
  //       }
  //       return true;
  //     }
  //     return false;
  //   };
  //   copier.deepCopyInstrs(condBlock.begin(), HWInstrRef{loop}.iter(ctx));

  //   return true;
  // }

  bool copyAndPushYieldVals(DeepCopier *self, InstrRef old,
                            BlockRef_iterator<true> insert,
                            SmallVecImpl<HWValue> &yields) {

    // todo: properly lower earlier or here
    if (old.isOpc(OP_ASSERT, HW_ASSERT_DEFER, HW_PRINT, HW_PRINT_DEFER))
      return true;

    assert(!old.isOpc(HW_STORE, HW_STORE_DEFER) &&
           "branch with side effects can't be linearized");
    if (old.isOpc(OP_YIELD) && self->blockDepth == 0) {
      auto range = Range{old}.transform([self](size_t, OperandRef ref) {
        auto it = self->oldToNewMap.find(ref->fat());
        return it ? it.val() : ref->fat();
      });
      yields.push_back_range(range);
      return true;
    }
    return false;
  }

  bool copyAndLinkLoopYields(DeepCopier *self, InstrRef old,
                             BlockRef_iterator<true> insert,
                             ConstantRef iterVal,
                             SmallVecImpl<HWValue> &yields) {
    if (old.isOpc(OP_UNYIELD) && self->blockDepth == 0) {
      for (auto [i, def] : Range{old.defs()}.enumerate()) {
        // first unyield def is iterator.
        self->oldToNewMap.insert(def->fat(),
                                 (iterVal && i == 0) ? iterVal : yields[i - 1]);
      }
      return true;
    }
    if (old.isOpc(OP_YIELD) && self->blockDepth == 0) {
      yields.clear();
      auto range = Range{old}.transform([self](size_t, OperandRef ref) {
        auto it = self->oldToNewMap.find(ref->fat());
        return it ? it.val() : ref->fat();
      });
      yields.push_back_range(range);
      return true;
    }
    return false;
  }

  void linearizeIf(IfInstrRef instr) {
    SmallVec<HWValue, 8> yields;

    auto copyHook = [&](DeepCopier *self, InstrRef old,
                        BlockRef_iterator<true> insert) {
      return copyAndPushYieldVals(self, old, insert, yields);
    };

    auto insertIter = BlockRef_iterator<true>{HWInstrRef{instr}.iter(ctx)};
    auto endIter = insertIter.succ();

    copier.deepCopyInstrs(instr.getTrueBlock().begin(), insertIter, copyHook);

    insertIter = endIter.pred();

    copier.deepCopyInstrs(instr.getFalseBlock().begin(), insertIter, copyHook);
    build.setInsertPoint(endIter);

    auto token = autoDebugInfo.addWithToken(instr);

    assert(yields.size() % 2 == 0 && "invalid number of yield values");
    for (size_t i = 0; i < yields.size() / 2; i++) {
      auto trueV = yields[i];
      auto falseV = yields[(yields.size() / 2) + i];

      auto ibuild = build.buildInstrRaw(HW_MUX, 4);
      ibuild.addRef(instr.getYieldValue(i)->as<WireRef>()).other();
      ibuild.addRef(instr.getCondValue()->as<HWValue>());
      ibuild.addRef(trueV);
      ibuild.addRef(falseV);

      instr.getYieldValue(i).replace(FatDynObjRef<>{nullref});
    }

    build.destroyInstr(instr);
  }

  void linearizeSwitch(SwitchInstrRef instr) {
    SmallVec<HWValue, 32> yields;
    auto copyHook = [&](DeepCopier *self, InstrRef old,
                        BlockRef_iterator<true> insert) {
      return copyAndPushYieldVals(self, old, insert, yields);
    };

    Optional<uint32_t> defaultIdx = nullopt;
    for (auto [i, yieldInstr] :
         Range{instr.block()}.as<CaseInstrRef>().enumerate()) {
      if (yieldInstr.isOpc(OP_CASE_DEFAULT)) {
        defaultIdx = i;
        break;
      }
    }

    auto insertIter = BlockRef_iterator<true>{HWInstrRef{instr}.iter(ctx)};
    auto endIter = insertIter.succ();
    for (auto yieldInstr : Range{instr.block()}.as<CaseInstrRef>()) {
      copier.deepCopyInstrs(yieldInstr.block().begin(), insertIter, copyHook);
      insertIter = endIter.pred();
    }
    uint numCaseBlocks = instr.block().size();
    assert(yields.size() % numCaseBlocks == 0 &&
           "invalid number of yield values");

    uint numYieldValues = instr.getNumYieldValues();

    build.setInsertPoint(endIter);

    auto token = autoDebugInfo.addWithToken(instr);
    assert(numYieldValues == 0 || defaultIdx);

    for (auto [j, caseInstr] :
         Range{instr.block()}.as<CaseInstrRef>().enumerate()) {
      if (j == defaultIdx)
        continue;

      // auto pred = BigInt::ICMP_CEQ;
      auto pred = BigInt::ICMP_EQ; // fixme
      if (caseInstr.isOpc(HW_CASE_Z))
        pred = BigInt::ICMP_CZEQ;
      else if (caseInstr.isOpc(HW_CASE_X))
        pred = BigInt::ICMP_CXEQ;

      auto orIB = build.buildInstrRaw(OP_OR, 1 + caseInstr.getNumOthers());
      auto selWire = ctx.getWires().create(1);
      orIB.addRef(selWire).other();
      assert(caseInstr.getNumOthers() != 0);
      build.setInsertPoint(orIB.instr());
      for (auto cond : caseInstr.others()) {
        orIB.addRef(build.buildICmp(instr.cond()->as<HWValue>(),
                                    cond->as<HWValue>(), pred));
      }
      build.setInsertPoint(endIter);

      for (uint i = 0; i < instr.getNumYieldValues(); i++) {
        HWValue iter =
            j == 0 ? yields[*defaultIdx * numYieldValues + i] : yields[i];
        HWValue newVal = yields[j * numYieldValues + i];
        yields[i] = build.buildMux(selWire, newVal, iter);
      }
    }

    for (auto [i, yieldVal] : instr.yieldValues().enumerate()) {
      yieldVal->as<WireRef>().replaceAllUsesWith(yields[i]);
    }

    build.destroyInstr(instr);
  }

  void linearizeFor(ForInstrRef forLoop) {

    bool illformed = !forLoop.getUpper()->is<ConstantRef>() ||
                     !forLoop.getLower()->is<ConstantRef>() ||
                     !forLoop.getStep()->is<ConstantRef>();
    if (illformed)
      report_fatal_error("ill-formed for loop");

    BigInt diff = forLoop.getUpper()->as<ConstantRef>() -
                  forLoop.getLower()->as<ConstantRef>();
    BigInt step = forLoop.getStep()->as<ConstantRef>();
    if (diff.getIs4S() || step.getIs4S())
      report_fatal_error("loop with undefined bounds");
    auto [div, mod] = BigInt::udivmodOp4S(diff, step);
    if (!mod.valueEquals(0))
      report_fatal_error("loop never terminates (diff not divisible by step)");
    if (!div.getLimitedVal())
      report_fatal_error("too many loop iterations");

    SmallVec<HWValue, 16> yieldValues(forLoop.getNumYieldValues());
    for (size_t i = 0; i < forLoop.getNumYieldValues(); i++)
      yieldValues[i] = forLoop.inputValues().begin()[i]->as<HWValue>();

    auto insertIter = BlockRef_iterator<true>{HWInstrRef{forLoop}.iter(ctx)};
    auto endIter = insertIter.succ();

    auto cbuild = ctx.constBuild();

    auto token = autoDebugInfo.addWithToken(forLoop);

    cbuild.val(forLoop.getLower()->as<ConstantRef>());
    for (uint64_t i = 0; i < *div.getLimitedVal(); i++) {
      copier.deepCopyInstrs(
          forLoop.getBlock().begin(), insertIter,
          [&](DeepCopier *self, InstrRef old, BlockRef_iterator<true> insert) {
            return copyAndLinkLoopYields(self, old, insert, cbuild,
                                         yieldValues);
          });

      cbuild.add(step);
      insertIter = endIter.pred();
    }

    for (size_t i = 0; i < forLoop.getNumYieldValues(); i++) {
      auto yieldOp = forLoop.yieldValues().begin()[i];
      yieldOp->as<WireRef>().replaceAllUsesWith(yieldValues[i]);
    }
    build.destroyInstr(forLoop);
  }

  void runOnProcess(ProcessIRef proc) {
    auto list = getSCFInstrsPreorder(proc.block());
    for (auto instr : Range{list}.reverse()) {
      switch (*instr.getDialectOpcode()) {
      default:
        continue;
      case *OP_IF:
        linearizeIf(instr);
        break;
      case *OP_SWITCH:
        linearizeSwitch(instr.as<SwitchInstrRef>());
        break;
      case *OP_FOR:
        linearizeFor(instr.as<ForInstrRef>());
        break;
      }
    }
  }

  void runOnModule(ModuleIRef module) {
    for (auto proc : module.procs())
      runOnProcess(proc);
  }

public:
  void run() {
    for (auto module : ctx.activeModules()) {
      runOnModule(module.iref());
    }
  }
  explicit LinearizeControlFlowPass(HWContext &ctx)
      : ctx(ctx), copier(ctx), build(ctx), autoDebugInfo(ctx) {}
};

}; // namespace dyno
