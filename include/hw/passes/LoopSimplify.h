#pragma once

#include "dyno/CFG.h"
#include "dyno/Constant.h"
#include "dyno/Instr.h"
#include "hw/AutoDebugInfo.h"
#include "hw/DeepCopy.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/Wire.h"
#include "hw/analysis/SCFTraversal.h"
#include "op/IDs.h"
#include "op/StructuredControlFlow.h"
#include "support/Debug.h"
#include "support/Utility.h"
#include <variant>
namespace dyno {

class LoopSimplifer {
  HWContext &ctx;
  AutoCopyDebugInfoStack autoDebugInfo;
  HWInstrBuilder build;
  // inspect yields.
  struct YieldVal {
    HWValue init;
    unsigned blockID;
    struct Passthru {};
    struct Permute {
      unsigned idx;
    };
    struct Increment {
      HWValue step;
    };
    struct Invariant {
      HWValue value;
    };
    struct Complex {};
    std::variant<Passthru, Permute, Increment, Invariant, Complex> variant;

    template <typename T> void set(unsigned blockID, T t) {
      if (!std::get_if<Passthru>(&variant)) {
        variant = Complex{};
        return;
      }
      variant = t;
      this->blockID = blockID;
    }
    template <> void set<Passthru>(unsigned, Passthru) {}
  };

  static auto getYield(BlockRef block) {
    assert(!block.empty());
    auto yield = block.end().pred().instr();
    assert(yield.isOpc(OP_YIELD));
    return yield;
  };

  auto getUnyield(BlockRef block) {
    assert(!block.empty());
    auto unyield = block.begin().instr();
    assert(unyield.isOpc(OP_UNYIELD));
    return unyield;
  };

  struct ForLoopCondition {
    unsigned iterYieldIdx;
    HWValue bound;
    DialectOpcode comparison;
  };
  std::optional<ForLoopCondition>
  analyzeCondition(BlockRef condBlock, ArrayRef<YieldVal> yieldVals) {
    auto condYield = getYield(condBlock);
    assert(condYield.getNumOperands() == yieldVals.size() + 1);
    auto cond = condYield.operand(0);
    auto asWire = cond->dyn_as<WireRef>();
    if (!asWire)
      return std::nullopt;
    assert(*asWire.getNumBits() == 1);
    auto compInstr = asWire.getDefI();
    if (!compInstr.isOpc(OP_ICMP_NE, OP_ICMP_SLT, OP_ICMP_ULT, OP_ICMP_SLE,
                         OP_ICMP_ULE, OP_ICMP_SGT, OP_ICMP_UGT, OP_ICMP_SGE,
                         OP_ICMP_UGE))
      return std::nullopt;

    unsigned yieldIdx;
    HWValue bound = nullref;
    unsigned boundIdx = 0;
    WireRef iter = nullref;

    for (unsigned i = 0; i < 2; i++) {
      auto op = compInstr.other(i);
      if (auto asWire = op->dyn_as<WireRef>();
          asWire &&
          HWInstrRef{asWire.getDefI()}.parentBlock(ctx) == condBlock &&
          asWire.getDefI().isOpc(OP_UNYIELD)) {
        yieldIdx = asWire.getDef() - *asWire.getDefI().begin();
        if (std::get_if<YieldVal::Increment>(&yieldVals[yieldIdx].variant)) {
          iter = asWire;
          continue;
        }
        // we allow a passthru yield val as the bound. everything else is not
        // strictly invariant, thus not allowed.
        if (!std::get_if<YieldVal::Passthru>(&yieldVals[yieldIdx].variant))
          return std::nullopt;
        if (HWInstrRef{asWire.getDefI()}.isDescendantOf(condBlock, ctx))
          return std::nullopt;
      }
      bound = op->fat();
      boundIdx = i;
    }
    if (!bound || !iter)
      return std::nullopt;

    auto opc = compInstr.getDialectOpcode();
    if (boundIdx == 0)
      opc = reverseOperandOrderICmpOpcode(opc);
    return ForLoopCondition{yieldIdx, bound, opc};
  }

public:
  InstrRef runOnLoop(InstrRef loop) {

    SmallVec<YieldVal, 8> yieldVals;
    SmallVec<BlockRef, 4> blocks;
    switch (*loop.getDialectOpcode()) {
    case *OP_WHILE:
      blocks.emplace_back(loop.as<WhileInstrRef>().getCondBlock());
      blocks.emplace_back(loop.as<WhileInstrRef>().getBodyBlock());
      yieldVals.resize(loop.as<WhileInstrRef>().getNumYieldValues());
      for (auto [i, val] : Range{yieldVals}.enumerate())
        val.init = loop.as<WhileInstrRef>().getInputValue(i).as<HWValue>();
      break;
    case *OP_DO_WHILE:
      blocks.emplace_back(loop.as<DoWhileInstrRef>().getBlock());
      yieldVals.resize(loop.as<DoWhileInstrRef>().getNumYieldValues());
      for (auto [i, val] : Range{yieldVals}.enumerate())
        val.init = loop.as<DoWhileInstrRef>().getInputValue(i).as<HWValue>();
      break;
    case *OP_FOR:
      blocks.emplace_back(loop.as<ForInstrRef>().getBlock());
      yieldVals.resize(loop.as<ForInstrRef>().getNumYieldValues());
      for (auto [i, val] : Range{yieldVals}.enumerate())
        val.init = (*(loop.as<ForInstrRef>().inputValues().begin() + i))
                       ->as<WireRef>();
      break;
    default:
      dyno_unreachable("unknown loop instr");
    }

    if (yieldVals.size() == 0)
      return nullref;

    // categorize yields
    for (auto [blockID, block] : Range{blocks}.enumerate()) {
      auto yield = getYield(block);
      auto range = Range{yield};
      if (yield.getNumOperands() == yieldVals.size() + 1)
        range = range.drop_front();
      for (auto [opIdx, op] : range.enumerate()) {

        if (auto asConst = op->dyn_as<ConstantRef>()) {
          yieldVals[opIdx].set(blockID, YieldVal::Invariant{asConst});
          continue;
        }
        if (auto asWire = op->dyn_as<WireRef>()) {
          auto defI = asWire.getDefI();
          if (!HWInstrRef{defI}.isDescendantOf(block, ctx)) {
            yieldVals[opIdx].set(blockID, YieldVal::Invariant{asWire});
            continue;
          }
          if (defI.isOpc(OP_UNYIELD)) {
            unsigned idx = asWire.getDef() - *defI.begin();
            if (idx == opIdx)
              yieldVals[opIdx].set(blockID, YieldVal::Passthru{});
            else
              yieldVals[opIdx].set(blockID, YieldVal::Permute{idx});

            continue;
          }

          if (defI.getNumOthers() == 2 && defI.isOpc(OP_ADD)) {
            FatDynObjRef<> operand = nullref;
            FatDynObjRef<> increment = nullref;
            for (auto cur : defI.others()) {
              if (cur->is<ConstantRef>()) {
                increment = cur->fat();
              } else if (auto wire = cur->dyn_as<WireRef>()) {
                // operand is wire which is just passed through -> iter
                if (wire.getDefI().isOpc(OP_UNYIELD)) {
                  unsigned idx = wire.getDef() - *wire.getDef().instr().begin();
                  if (idx != opIdx)
                    goto complex;
                  operand = wire;
                } else {
                  // operand is not iter, so must be increment. only valid if
                  // invariant while running through loop.
                  if (HWInstrRef{wire.getDefI()}.isDescendantOf(block, ctx))
                    goto complex;
                  increment = wire;
                }
              } else
                dyno_unreachable("invalid hwvalue");
            }

            if (!operand || !increment)
              goto complex;

            yieldVals[opIdx].set(blockID, YieldVal::Increment{increment});
            continue;
          }

        complex:
          yieldVals[opIdx].set(blockID, YieldVal::Complex{});
        }
      }
    }

    DYNO_DBG("LoopSimplify", {
      dumpInstr(loop, ctx);
      dbgs() << "yield analysis results:\n";
      for (auto [i, val] : Range{yieldVals}.enumerate()) {
        dbgs() << "yield val #" << i << ": ";
        if (std::get_if<YieldVal::Passthru>(&val.variant))
          dbgs() << "Passthru";
        else if (auto incr = std::get_if<YieldVal::Increment>(&val.variant)) {
          dbgs() << "Increment(";
          dumpObj(incr->step);
          dbgs() << ")";
        } else if (auto vari = std::get_if<YieldVal::Invariant>(&val.variant)) {
          dbgs() << "Invariant(";
          dumpObj(vari->value);
          dbgs() << ")";
        } else if (auto perm = std::get_if<YieldVal::Permute>(&val.variant)) {
          dbgs() << "Permute(" << perm->idx << ")";
        } else {
          dbgs() << "Complex";
        }
        dbgs() << "\n";
      }
    });

    bool change = false;
    HWValue forLower = nullref;
    HWValue forUpper;
    HWValue forStep;
    Optional<unsigned> forLoopIter = nullopt;
    bool isNewForLoop = false;
    bool isOldForLoop = false;

    if (loop.isOpc(OP_FOR)) {
      forLower = loop.as<ForInstrRef>().getLower()->as<HWValue>();
      forUpper = loop.as<ForInstrRef>().getUpper()->as<HWValue>();
      forStep = loop.as<ForInstrRef>().getStep()->as<HWValue>();
      isOldForLoop = true;
    }
    // find loop condition
    else if (auto result = analyzeCondition(blocks.front(), yieldVals)) {
      if (!(blocks.size() == 1 || blocks.front().size() == 3))
        goto end;

      forLower = yieldVals[result->iterYieldIdx].init;
      forUpper = result->bound;
      forStep =
          std::get<YieldVal::Increment>(yieldVals[result->iterYieldIdx].variant)
              .step;

      auto forLowerC = forLower.dyn_as<ConstantRef>();
      auto forUpperC = forUpper.dyn_as<ConstantRef>();
      auto forStepC = forStep.dyn_as<ConstantRef>();

      if (!forLowerC || !forUpperC || !forStepC)
        goto end;

      auto cmpPred = BigInt::ICmpPred(
          BigInt::ICMP_EQ + (result->comparison.opc - OP_ICMP_EQ.opc));
      auto initial = BigInt::icmpOp4S(forLowerC, forUpperC, cmpPred);
      if (!initial) {
        // no iterations
        forUpper = yieldVals[result->iterYieldIdx].init;
        change |= 1;
        forLoopIter = result->iterYieldIdx;
        isNewForLoop = true;
        goto end;
      }

      if (forStepC.getSignBit() == FourState::S0) {
        // positive step
        switch (*result->comparison) {
        case *OP_ICMP_SLT:
        case *OP_ICMP_ULT: {
          auto rem = ((forUpperC - forLowerC) % forStepC);
          if (!rem.valueEquals(0))
            rem = forStepC - rem;
          forUpper = ctx.constBuild().val(forUpperC).add(rem).get();
          break;
        }
        case *OP_ICMP_NE: {
          auto rem = (forUpperC - forLowerC) % forStepC;
          if (!rem.valueEquals(0))
            goto end;
          break;
        }
        case *OP_ICMP_ULE:
        case *OP_ICMP_SLE: {
          auto upperPlusOne = forUpperC + "1'b1"_bv;
          auto rem = ((upperPlusOne - forLowerC) % forStepC);
          if (!rem.valueEquals(0))
            rem = forStepC - rem;
          forUpper = ctx.constBuild().val(upperPlusOne).add(rem).get();
          break;
        }
        default:
          goto end;
        }
      } else if (forStepC.getSignBit() == FourState::S1) {
        // negative step
        switch (*result->comparison) {
        case *OP_ICMP_SGT:
        case *OP_ICMP_UGT: {
          auto rem = ((forLowerC - forUpperC) % (-forStepC));
          if (!rem.valueEquals(0))
            rem = (-forStepC) - rem;
          forUpper = ctx.constBuild().val(forUpperC).sub(rem).get();
          break;
        }
        case *OP_ICMP_NE: {
          auto rem = (forLowerC - forUpperC) % (-forStepC);
          if (!rem.valueEquals(0))
            goto end;
          break;
        }
        case *OP_ICMP_UGE:
        case *OP_ICMP_SGE: {
          auto upperMinusOne = forUpperC - "1'b1"_bv;
          auto rem = ((forLowerC - upperMinusOne) % (-forStepC));
          if (!rem.valueEquals(0))
            rem = (-forStepC) - rem;
          forUpper = ctx.constBuild().val(upperMinusOne).sub(rem).get();
          break;
        }
        default:
          goto end;
        }
      }

      // check for overflows
      auto final =
          BigInt::icmpOp4S(forLowerC, forUpper.as<ConstantRef>(), cmpPred);
      if (!final)
        goto end;

      change |= 1;
      forLoopIter = result->iterYieldIdx;
      isNewForLoop = true;
    }
  end:
    bool isForLoop = isNewForLoop || isOldForLoop;

    // apply new yields
    auto isSkipYieldVal = [](const YieldVal &val) {
      return (std::get_if<YieldVal::Passthru>(&val.variant));
    };

    unsigned newNumYieldVals = 0;
    for (auto [i, val] : Range{yieldVals}.enumerate()) {
      if (isSkipYieldVal(val) || i == forLoopIter) {
        change |= 1;
        continue;
      }
      if (std::get_if<YieldVal::Invariant>(&val.variant))
        change |= 1;
      newNumYieldVals++;
    }
    if (!change)
      return nullref;

    auto token = autoDebugInfo.addWithToken(loop);

    // simplify yields by deleting unused.
    for (auto block : blocks) {
      auto unyield = getUnyield(block);
      auto yield = getYield(block);
      bool hasCond = (yield.getNumOperands() == yieldVals.size() + 1);
      bool newHasCond = hasCond && !isForLoop;

      build.setInsertPoint(unyield);
      auto unyieldBuild =
          build.buildInstrRaw(OP_UNYIELD, newNumYieldVals + isForLoop);

      build.setInsertPoint(yield);

      auto yieldBuildVals = newNumYieldVals + newHasCond;

      std::optional<InstrBuilder> yieldBuild = std::nullopt;
      if (yieldBuildVals != 0) {
        yieldBuild = build.buildInstrRaw(OP_YIELD, yieldBuildVals);
        yieldBuild->other();
      }

      WireRef forIterWire;
      if (isForLoop) {
        forIterWire = isNewForLoop
                          ? ctx.getWires().create(
                                yieldVals[*forLoopIter].init.getNumBits())
                          : unyield.def(0)->as<WireRef>();
        unyieldBuild.addRef(forIterWire);
      }
      if (newHasCond)
        yieldBuild->addRef(yield.operand(0)->fat());

      if (forLoopIter) {
        unyield.operand(*forLoopIter)
            ->as<WireRef>()
            .replaceAllUsesWith(forIterWire);
      }

      for (unsigned i = 0; i < yieldVals.size(); i++) {
        auto yieldOp = yield.operand(i + hasCond);
        auto unyieldOp = unyield.operand(i + isOldForLoop);
        auto analysis = yieldVals[i];

        if (auto *passthru [[maybe_unused]] =
                std::get_if<YieldVal::Passthru>(&analysis.variant)) {
          unyieldOp->as<WireRef>().replaceAllUsesWith(analysis.init);
          continue;
        }
        if (auto *invariant [[maybe_unused]] =
                std::get_if<YieldVal::Invariant>(&analysis.variant)) {
          // there's some initial iteration shenanigans here, fixme
          // unyieldOp->as<WireRef>().replaceAllUsesWith(invariant->value);
          // continue;
        }

        if (i == forLoopIter)
          continue;

        yieldBuild->addRef(yieldOp->fat());
        unyieldBuild.addRef(unyieldOp->fat());
        unyieldOp.replace(FatDynObjRef<>{nullref});
      }

      build.destroyInstr(unyield);
      build.destroyInstr(yield);
    }

    build.setInsertPoint(loop);

    auto originalNumBlocks = blocks.size();
    bool destroyFirstBlock = false;
    if (isNewForLoop && blocks.size() == 2) {
      destroyFirstBlock = true;
      blocks.erase(blocks.begin());
    }

    auto ibuild = build.buildInstrRaw(
        isForLoop ? static_cast<DialectOpcode>(OP_FOR)
                  : loop.getDialectOpcode(),
        blocks.size() + 2 * newNumYieldVals + (isForLoop ? 3 : 0));
    for (auto block : blocks)
      ibuild.addRef(block);
    for (auto [i, val] : Range{yieldVals}.enumerate()) {
      auto def = loop.def(originalNumBlocks + i);
      if (i == forLoopIter) {
        def->as<WireRef>().replaceAllUsesWith(forUpper);
        continue;
      }
      if (auto *passthru [[maybe_unused]] =
              std::get_if<YieldVal::Passthru>(&val.variant)) {
        def->as<WireRef>().replaceAllUsesWith(loop.other(i)->fat());
      } else {
        // if (auto *invariant = std::get_if<YieldVal::Invariant>(&val.variant))
        // {
        //   def->as<WireRef>().replaceAllUsesWith(invariant->value);
        // }
        ibuild.addRef(def->fat());
        def.replace(FatDynObjRef<>{nullref});
      }
    }
    ibuild.other();
    if (isForLoop) {
      ibuild.addRef(forLower);
      ibuild.addRef(forUpper);
      ibuild.addRef(forStep);
    }
    for (auto [i, val] : Range{yieldVals}.enumerate()) {
      if (i == forLoopIter)
        continue;
      if (isSkipYieldVal(val))
        continue;
      ibuild.addRef(loop.other(i)->fat());
    }

    // not replaced -> gets deleted
    unsigned replaceStart = destroyFirstBlock ? 1 : 0;
    for (unsigned i = replaceStart; i < originalNumBlocks; i++) {
      loop.def(i).replace(FatDynObjRef<>{nullref});
    }

    build.destroyInstr(loop);

    return ibuild.instr();
  }

public:
  LoopSimplifer(HWContext &ctx) : ctx(ctx), autoDebugInfo(ctx), build(ctx) {}
};

class LoopSimplifyPass {
  HWContext &ctx;
  LoopSimplifer loopSimplifier;

public:
  struct Config {
    bool errorOnNonForLoop = true;
  };
  Config config;

private:
  void runOnProcess(ProcessIRef proc) {
    auto instrs = getSCFInstrsPreorder(proc.block());
    for (auto instr : Range{instrs}.reverse()) {
      if (instr.isOpc(OP_FOR, OP_WHILE, OP_DO_WHILE))
        loopSimplifier.runOnLoop(instr);
    }
  }

  void runOnModule(ModuleIRef module) {
    for (auto proc : module.procs()) {
      runOnProcess(proc);
    }
  }

public:
  void run() {
    for (auto mod : ctx.activeModules()) {
      runOnModule(mod.iref());
    }
  }

  explicit LoopSimplifyPass(HWContext &ctx) : ctx(ctx), loopSimplifier(ctx) {}
};
}; // namespace dyno
