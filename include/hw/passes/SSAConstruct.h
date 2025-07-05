#pragma once
#include "dyno/CFG.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "dyno/ObjMap.h"
#include "hw/AutoDebugInfo.h"
#include "hw/HWAbstraction.h"
#include "hw/HWInstr.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/analysis/RegisterValue.h"
#include "op/IDs.h"
#include "op/StructuredControlFlow.h"
#include "support/ArrayRef.h"
#include "support/Debug.h"
#include "support/Ranges.h"

namespace dyno {

class SSAConstructPass {
  HWContext &ctx;
  uint depth = 0;
  ObjMapVec<Instr, bool> isNewInstr;
  AutoCopyDebugInfoStack autoDebugInfo;

public:
  struct Config {

    enum Mode {
      IMMEDIATE,
      DEFERRED,
    };
    Mode mode = IMMEDIATE;
  };
  Config config;

private:
  using TriggerID = decltype(RegisterValue::Fragment::triggerID);

  struct RegState {
    SmallVec<RegisterValue, 4> stack;

    RegisterValue &getOrSetDefault(uint depth, RegisterRef reg) {
      if (!stack.empty() && stack.back().depth == depth)
        return stack.back();
      else {
        auto &rv = stack.emplace_back(RegisterValue{reg, depth, true, nullopt});
        rv.untouched = true;
        return rv;
      }
    }
    RegisterValue &get() {
      assert(!stack.empty());
      return stack.back();
    }

    bool has(uint depth) {
      if (stack.empty())
        return false;
      return stack.back().depth == depth;
    }

    void clear(uint depth, RegisterRef reg) {
      getOrSetDefault(depth, reg) = RegisterValue{reg, depth, true, nullopt};
    }

    void plainValue(uint depth, RegisterRef reg, HWValue value,
                    TriggerID trigger) {
      getOrSetDefault(depth, reg) =
          RegisterValue{value, *reg->numBits, depth, false, trigger};
    }
  };

  ObjMapVec<Register, RegState> regMap;

  InstrRef resolve(ObjRef<Instr> ref) {
    if (!ref)
      return nullref;
    return InstrRef{ctx.getInstrs().resolve(ref)};
  }

public:
  explicit SSAConstructPass(HWContext &ctx) : ctx(ctx), autoDebugInfo(ctx) {}

  struct MultiwayResult {
    SmallVec<std::tuple<RegisterRef, std::pair<uint32_t, uint32_t>, TriggerID>,
             2>
        yieldRegs;
    SmallVec<SmallVec<HWValue, 4>, 2> yieldVals;

    explicit operator bool() { return !yieldRegs.empty(); }
  };
  MultiwayResult runOnMultiway(ProcessIRef proc, ArrayRef<BlockRef> wayBlocks) {
    HWInstrBuilder build{ctx};
    auto startDepth = depth;

    if (wayBlocks.size() == 0)
      return {};
    if (wayBlocks.size() == 1) {
      runOnBlock(proc, wayBlocks[0]);
      return {};
    }

    // Run through all blocks. They get a copy of the current var state as their
    // initial value which they modify according to their contents.

    // could save on copying with a copy-on-write version of RegisterValue
    SmallVec<std::pair<ObjRef<Register>, RegisterValue>, 4> priorValues;
    for (auto [obj, regState] : regMap) {
      if (regState.has(startDepth)) {
        auto &value = regState.get();
        assert(value.frags.data());
        priorValues.emplace_back(obj, value);

        auto &copy = regState.stack.emplace_back(priorValues.back().second);
        copy.depth = depth + 1;
        copy.untouched = true;
      }
    }

    // first
    depth++;
    runOnBlock(proc, wayBlocks[0]);
    // remaining
    for (auto block : Range{wayBlocks}.drop_front()) {
      for (auto [reg, value] : priorValues) {
        auto &copy = regMap[reg].stack.emplace_back(value);
        copy.depth = depth + 1;
        copy.untouched = true;
      }
      depth++;
      runOnBlock(proc, block);
    }

    // Check if register state was modified in one or more of the blocks.
    MultiwayResult rv;
    rv.yieldVals.resize(wayBlocks.size());

    // todo: better data structure, don't iter thru all regs
    for (auto [obj, regState] : regMap) {
      if (regState.stack.empty())
        continue;
      auto topDepth = regState.stack.back().depth;
      // skip if not modified in either branch
      if (topDepth <= startDepth)
        continue;

      auto reg = RegisterRef{ctx.getRegs().resolve(obj)};

      RegisterValue lazyLoad{reg, startDepth, true, nullopt};
      SmallVec<RegisterValue *, 2> vals(wayBlocks.size(), &lazyLoad);

      bool allUntouched = true;
      ssize_t i = regState.stack.size() - 1;
      while (i >= 0 && regState.stack[i].depth > startDepth) {
        auto &val = regState.stack[i--];
        vals[val.depth - startDepth - 1] = &val;
        allUntouched &= val.untouched;
      }

      if (allUntouched) {
        regState.stack.resize(i + 1);
        continue;
      }

      auto diffs = diffRegisterValues(vals);
      if (diffs.empty()) {
        regState.stack.resize(i + 1);
        continue;
      }

      for (auto [i, block] : Range{wayBlocks}.enumerate()) {
        // Three Options
        // 1. Value set in this block => just yield this block's version.
        // 2. Value set in parent block => yield parent's version.
        // 3. Value not set at all => load register and yield that.
        // (all handled implicitly)

        auto insert = BlockRef{block}.end();
        if (!BlockRef{block}.empty() && insert.pred()->isOpc(OP_YIELD))
          --insert;
        build.setInsertPoint(insert);

        rv.yieldVals[i].reserve(rv.yieldVals[i].size() + diffs.size());
        for (auto diff : diffs) {
          if (diff.untouched())
            continue;
          rv.yieldVals[i].emplace_back(
              vals[i]->get(build, diff.addr(), diff.len(), false));
        }
      }

      for (auto diff : diffs) {
        if (diff.untouched())
          continue;
        rv.yieldRegs.emplace_back(reg, diff.pair(), diff.triggerID());
      }

      regState.stack.resize(i + 1);
    }
    depth = startDepth;
    return rv;
  }

  // todo: unmutable array ref, requires fixing const block iter
  template <uint numLoopBlocks>
  auto analyzeAndCreateLoopYields(HWInstrBuilder &build, InstrRef loopInstr,
                                  MutArrayRef<BlockRef> loopBlocks) {
    assert(loopBlocks.size() == numLoopBlocks);
    auto startDepth = depth - numLoopBlocks;
    SmallVec<std::tuple<RegisterRef, std::pair<uint32_t, uint32_t>, TriggerID>,
             4>
        yieldVals;
    std::array<SmallVec<HWValue, 4>, numLoopBlocks> materializedYieldVals;
    std::array<SmallVec<WireRef, 4>, numLoopBlocks> unyieldWires;

    // similar to do while but yield values are union of stores in cond and
    // body.
    for (auto [obj, state] : regMap) {
      // entire register unmodified in parent or body.
      if (state.stack.empty() || state.stack.back().depth < startDepth)
        continue;
      // entire register unmodified in body.
      if (state.has(startDepth)) {
        // for next pass: expose state of untouched (invariant) reg to
        // body.
        for (uint32_t i = 0; i < numLoopBlocks; i++) {
          auto &copy = state.stack.emplace_back(RegisterValue{state.get()});
          copy.depth = startDepth + i + 1;
        }
        continue;
      }
      auto reg = RegisterRef{ctx.getRegs().resolve(obj)};

      // check if parent block has a state for this reg.
      RegisterValue defaultVal{reg, ~0U, true, nullopt};
      std::array<RegisterValue *, numLoopBlocks + 1> blockVals;
      {
        size_t i = 0;
        while (i <= numLoopBlocks) {
          uint32_t expectedDepth = startDepth + numLoopBlocks - i;
          if (state.stack.size() < i + 1) {
            state.stack.insert(
                state.stack.begin(),
                RegisterValue{reg, expectedDepth, true, nullopt});
          } else {
            auto &entryBelow = state.stack[state.stack.size() - i - 1];
            if (entryBelow.depth != expectedDepth) {
              state.stack.insert(
                  &entryBelow + 1,
                  RegisterValue{reg, expectedDepth, true, nullopt});
            }
          }
          i++;
        }
      }

      for (size_t i = 0; i < numLoopBlocks + 1; i++) {
        size_t sz = state.stack.size();
        blockVals[i] = &state.stack[sz - numLoopBlocks - 1 + i];
      }

      auto *parentVal = blockVals.front();
      blockVals.front() = &defaultVal;

      // diffRegisterValues is a bit overkill, could specialize
      // difference with defaultVal if too slow (but prob fine).
      auto diffs = diffRegisterValues(blockVals);

      for (auto diff : diffs) {
        if (diff.untouched()) {
          // value not actually different, we just got a diff because value
          // was materialized. Hoist materialization out of loop.
          build.setInsertPoint(ctx.getCFG()[loopInstr]);
          auto matVal = parentVal->get(build, diff.addr(), diff.len());
          for (auto [i, val] : Range{blockVals}.drop_front().enumerate())
            // could also overwriteNoMaterialize here to not hoist but lazily
            // compute.
            val->overwrite(matVal, 0, diff.addr(), diff.len(), true);
          continue;
        }
        // modified anywhere in loop -> becomes yield value in every loop
        // block.
        yieldVals.emplace_back(reg, diff.pair(), diff.triggerID());
        for (auto [i, val] : Range{blockVals}.drop_front().enumerate()) {
          // we need to materialize yield values here before we overwrite
          // them.
          build.setInsertPoint(loopBlocks[i].end());
          if (!loopBlocks[i].empty() &&
              loopBlocks[i].end().pred()->isOpc(OP_YIELD)) {
            build.setInsertPoint(loopBlocks[i].end().pred());
          }
          auto matVal = val->get(build, diff.addr(), diff.len(), false);
          materializedYieldVals[i].emplace_back(matVal);

          // now overwrite with unyield wire for next pass (source for this
          // value becomes yield of previous).
          auto wire =
              unyieldWires[i].emplace_back(ctx.getWires().create(diff.len()));
          val->overwrite(wire, 0, diff.addr(), diff.len(), false,
                         diff.triggerID());
        }
      }
    }

    return std::make_tuple(yieldVals, materializedYieldVals, unyieldWires);
  }

  // this is not really needed, template also works with size == 1.
  // already written and quite a bit more efficient though, so keep this for
  // now.
  template <>
  auto analyzeAndCreateLoopYields<1>(HWInstrBuilder &build, InstrRef loopInstr,
                                     MutArrayRef<BlockRef> loopBlocks) {
    assert(loopBlocks.size() == 1);
    uint startDepth = depth - 1;
    uint bodyDepth = depth;

    SmallVec<std::tuple<RegisterRef, std::pair<uint32_t, uint32_t>, TriggerID>,
             4>
        yieldVals;
    SmallVec<HWValue, 4> materializedYieldVals(yieldVals.size());
    SmallVec<WireRef, 4> unyieldWires;
    for (auto [obj, state] : regMap) {
      // entire register unmodified in body.
      if (state.has(startDepth)) {
        // for next pass: expose state of untouched (invariant) reg to
        // body.
        auto &copy = state.stack.emplace_back(state.get());
        copy.depth = bodyDepth;
        continue;
      }
      if (!state.has(bodyDepth))
        continue;

      // check if parent block has a state for this reg.
      RegisterValue *parentVal = nullptr;
      if (state.stack.size() > 1) {
        auto &entryBelow = state.stack[state.stack.size() - 2];
        if (entryBelow.depth == startDepth)
          parentVal = &entryBelow;
      }
      auto reg = RegisterRef{ctx.getRegs().resolve(obj)};
      if (!parentVal) {
        parentVal =
            state.stack.insert(state.stack.end() - 1,
                               RegisterValue{reg, startDepth, true, nullopt});
      }

      auto *val = &state.get();

      for (auto &frag : val->frags) {
        // skip default frags
        if (frag.ref == obj && frag.srcAddr == frag.dstAddr)
          continue;
        if (frag.untouched) {
          build.setInsertPoint(ctx.getCFG()[loopInstr]);

          auto matVal = parentVal->get(build, frag.dstAddr, frag.len);
          frag.ref = matVal;
          frag.srcAddr = 0;
          assert(frag.untouched);

          // dumpCtx(ctx);
          continue;
        }
        yieldVals.emplace_back(reg, std::make_pair(frag.dstAddr, frag.len),
                               frag.triggerID);

        build.setInsertPoint(loopBlocks[0].end());
        if (!loopBlocks[0].empty() &&
            loopBlocks[0].end().pred()->isOpc(OP_YIELD)) {
          build.setInsertPoint(loopBlocks[0].end().pred());
        }
        auto [matVal, matRange] = frag.getValue(build, 0, frag.len);
        assert(matRange == (BitRange{(uint32_t)0, (uint32_t)frag.len}));
        materializedYieldVals.emplace_back(matVal);

        // for next pass: make this point to the unyield value.
        frag.ref = unyieldWires.emplace_back(ctx.getWires().create(frag.len));
        frag.srcAddr = 0;
      }
    }

    return std::make_tuple(yieldVals, materializedYieldVals, unyieldWires);
  }

  bool addYieldsToLoopInstr(
      HWInstrBuilder &build, InstrRef loopInstr,
      ArrayRef<
          std::tuple<RegisterRef, std::pair<uint32_t, uint32_t>, TriggerID>>
          yieldVals) {
    if (yieldVals.size() == 0)
      return false;

    build.setInsertPoint(HWInstrRef{loopInstr}.iter(ctx));

    SmallVec<HWValue, 4> newUses;
    newUses.reserve(yieldVals.size());
    for (auto [reg, range, trigger] : yieldVals) {
      auto &val = regMap[reg].getOrSetDefault(depth, reg);
      // set initial yield vals to value before loop.
      newUses.emplace_back(val.get(build, range.first, range.second, false));
    }

    SmallVec<WireRef, 4> newDefs;
    newDefs.reserve(yieldVals.size());

    for (auto [reg, range, trigger] : yieldVals) {
      auto &val = regMap[reg].getOrSetDefault(depth, reg);
      // set reg state after loop to output yield vals.
      auto wire = ctx.getWires().create(range.second);
      newDefs.emplace_back(wire);
      val.overwrite(wire, 0, range.first, range.second, false, trigger);
    }

    build.addOperands(loopInstr, newDefs, newUses);
    return true;
  }

  void dumpState() {
    DEBUG("SSAConstruct", {
      dbgs() << "state at depth " << depth << "\n";
      for (auto [obj, state] : regMap) {
        if (!state.has(depth))
          continue;

        auto reg = ctx.getRegs().resolve(obj);
        dumpInstr(reg.iref());

        HWPrinter print{dbgs()};

        for (auto frag : state.get().frags) {
          dbgs() << "[" << frag.dstAddr << "+:" << frag.len << "] = ";
          print.printTypeDefault(frag.ref);
          dbgs() << "[" << frag.ref.getObjID() << "]";
          if (frag.untouched)
            dbgs() << " (untouched)";
          dbgs() << "\n";
        }
        dbgs() << "\n";
      }
    })
  }

  void runOnBlock(ProcessIRef proc, BlockRef block) {

    HWInstrBuilder build{ctx};
    SmallVec<FatDynObjRef<>, 32> destroyList;

    for (auto instr : block) {
      auto token = autoDebugInfo.addWithToken(instr);
      switch (*instr.getDialectOpcode()) {

      case *HW_STORE_DEFER:
      case *HW_STORE: {
        if (instr.getDialectOpcode() == HW_STORE_DEFER &&
            config.mode != Config::DEFERRED)
          break;
        if (instr.getDialectOpcode() == HW_STORE &&
            config.mode != Config::IMMEDIATE)
          break;

        auto asStore = instr.as<StoreIRef>();
        auto &regState = regMap[asStore.reg()];

        TriggerID trigger = nullopt;
        if (instr.getDialectOpcode() == HW_STORE_DEFER)
          trigger = asStore.trigger().getObjID().num;

        uint32_t addr = 0;
        uint32_t len = *asStore.reg()->numBits;
        if (auto range = asStore.range()) {
          if (!range->isConstant()) {
            build.setInsertPoint(asStore.iter(ctx));
            auto val = regState.getOrSetDefault(depth, asStore.reg())
                           .get(build, 0, *asStore.reg()->numBits, false);
            val = build.buildInsert(val, asStore.value(), *asStore.range());

            // plain value doesn't check for conflicting triggers.
            // regState.plainValue(depth, asStore.reg(), val, trigger);

            regState.get().overwrite(val, 0, 0, len, false, trigger);
            destroyList.emplace_back(asStore);
            break;
          }
          addr = range->getAddr().as<ConstantRef>().getExactVal();
          len = range->getLen().as<ConstantRef>().getExactVal();
        }

        destroyList.emplace_back(asStore);

        auto &state = regState.getOrSetDefault(depth, asStore.reg());
        state.overwrite(asStore.value(), 0, addr, len, false, trigger);
        break;
      }

      case *HW_LOAD: {
        if (config.mode == Config::DEFERRED && !isNewInstr[instr]) {
          // deferred stores are not yet visible, can't prop to loads.
          break;
        }
        auto asLoad = instr.as<LoadIRef>();
        auto &regState = regMap[asLoad.reg()];

        auto &val = regState.getOrSetDefault(depth, asLoad.reg());

        uint32_t addr = 0;
        uint32_t len = *asLoad.reg()->numBits;
        if (asLoad.hasRange()) {
          auto range = asLoad.range();
          if (!range->isConstant()) {
            build.setInsertPoint(ctx.getCFG()[asLoad]);
            auto matVal =
                build.buildSplice(val.get(build), BitRange{*asLoad.range()});
            asLoad.defW().replaceAllUsesWith(matVal);
            destroyList.emplace_back(asLoad);
            break;
          }
          addr = range->getAddr().as<ConstantRef>().getExactVal();
          len = range->getLen().as<ConstantRef>().getExactVal();
        }

        build.setInsertPoint(asLoad.iter(ctx));
        auto newVal = val.get(build, addr, len);
        asLoad.defW().replaceAllUsesWith(newVal);
        destroyList.emplace_back(asLoad);
        break;
      }

      case *OP_IF: {
        auto asIf = instr.as<IfInstrRef>();

        BlockRef falseBlock =
            asIf.hasFalseBlock() ? asIf.getFalseBlock() : nullref;
        if (!falseBlock)
          falseBlock = ctx.createBlock();

        std::array<BlockRef, 2> blocks{asIf.getTrueBlock(), falseBlock};
        auto res = runOnMultiway(proc, blocks);

        // Create yields for modified register values.
        uint addedYieldVals = res.yieldVals[0].size();
        if (addedYieldVals == 0) {
          if (!asIf.hasFalseBlock())
            build.destroyObj(falseBlock);
          break;
        }

        uint oldYieldVals = asIf.getNumYieldValues();
        uint newYieldVals = addedYieldVals + oldYieldVals;

        for (size_t i = 0; i < blocks.size(); i++) {
          auto [oldYield, newYield] =
              build.extendOrNewYield(blocks[i], res.yieldVals[i]);
          if (oldYield)
            destroyList.emplace_back(oldYield);
        }

        build.setInsertPoint(HWInstrRef{asIf}.iter(ctx));
        auto newIf = build.buildIfElse(asIf, newYieldVals, falseBlock);

        for (auto [i, tup] : Range{res.yieldRegs}.enumerate()) {
          auto [reg, range, trig] = tup;

          auto yieldVal = newIf.getYieldValue(i + oldYieldVals)->as<WireRef>();
          yieldVal->numBits = range.second;
          regMap[reg]
              .getOrSetDefault(depth, reg)
              .overwrite(yieldVal, 0, range.first, range.second, false, trig);
        }

        destroyList.emplace_back(asIf);
        break;
      }

      case *OP_WHILE: {
        auto asWhile = instr.as<WhileInstrRef>();
        auto startDepth = depth;

        depth++;
        runOnBlock(proc, asWhile.getCondBlock());

        depth++;
        runOnBlock(proc, asWhile.getBodyBlock());

        std::array<BlockRef, 2> loopBlocks = {asWhile.getCondBlock(),
                                              asWhile.getBodyBlock()};

        auto [yieldVals, materializedYieldVals, unyieldWires] =
            analyzeAndCreateLoopYields<loopBlocks.size()>(build, asWhile,
                                                          loopBlocks);

        // second pass (in reverse order, pop values from top of stack)
        size_t i = loopBlocks.size() - 1;
        for (auto block : Range{loopBlocks}.reverse()) {
          // build yield and unyield
          if (!materializedYieldVals[i].empty()) {
            auto [oldY, newY] =
                build.extendOrNewYield(block, materializedYieldVals[i]);
            if (oldY)
              destroyList.emplace_back(oldY);
          }
          if (!unyieldWires[i].empty()) {
            auto [oldUY, newUY] =
                build.extendOrNewUnyield(block, unyieldWires[i]);
            if (oldUY)
              destroyList.emplace_back(oldUY);
          }

          // run block, can now resolve loads from both yield and invariant
          // vals.
          runOnBlock(proc, block);

          // done, erase block's values from stack.
          for (auto [obj, state] : regMap) {
            if (state.has(depth))
              state.stack.pop_back();
            assert(state.stack.size() <= depth &&
                   "too many values on stack. missing cleanup?");
          }
          depth--;
          i--;
        }
        assert(depth == startDepth);
        bool deleteOld = addYieldsToLoopInstr(build, asWhile, yieldVals);
        if (deleteOld)
          destroyList.emplace_back(asWhile);
        break;
      }

      case *OP_DO_WHILE: {
        auto asWhile = instr.as<DoWhileInstrRef>();

        auto startDepth = depth;
        auto bodyDepth = ++depth;

        auto block = asWhile.getBlock();

        // First pass through loop body, detect stores and convert to yields.
        runOnBlock(proc, block);

        auto [yieldVals, materializedYieldVals, unyieldWires] =
            analyzeAndCreateLoopYields<1>(build, asWhile,
                                          MutArrayRef{&block, 1});
        if (!materializedYieldVals.empty()) {
          auto [oldY, newY] =
              build.extendOrNewYield(block, materializedYieldVals);
          if (oldY)
            destroyList.emplace_back(oldY);
        }
        if (!unyieldWires.empty()) {
          auto [oldUY, newUY] = build.extendOrNewUnyield(block, unyieldWires);
          if (oldUY)
            destroyList.emplace_back(oldUY);
        }

        // Second pass, can now resolve loads from both yield and invariant
        // vals.
        runOnBlock(proc, block);

        // Clear body state.
        for (auto [obj, state] : regMap) {
          if (state.has(bodyDepth))
            state.stack.pop_back();
          assert(state.stack.size() <= depth &&
                 "too many values on stack. missing cleanup?");
        }
        depth = startDepth;

        bool deleteOld = addYieldsToLoopInstr(build, asWhile, yieldVals);
        if (deleteOld)
          destroyList.emplace_back(asWhile);

        break;
      }

      case *OP_SWITCH: {
        auto asSwitch = instr.as<SwitchInstrRef>();
        SmallVec<BlockRef, 8> blocks;
        blocks.reserve(asSwitch.block().size());
        for (auto instr : asSwitch.block()) {
          auto caseInstr = instr.as<CaseInstrRef>();
          blocks.emplace_back(caseInstr.block());
        }
        auto res = runOnMultiway(proc, blocks);

        if (res.yieldRegs.empty())
          break;

        for (size_t i = 0; i < blocks.size(); i++) {
          auto [oldYield, newYield] =
              build.extendOrNewYield(blocks[i], res.yieldVals[i]);
          if (oldYield)
            destroyList.emplace_back(oldYield);
        }

        SmallVec<WireRef, 4> newYields;
        newYields.reserve(res.yieldRegs.size());

        for (auto [reg, range, trig] : res.yieldRegs) {
          auto yieldVal =
              newYields.emplace_back(ctx.getWires().create(range.second));
          regMap[reg]
              .getOrSetDefault(depth, reg)
              .overwrite(yieldVal, 0, range.first, range.second, false, trig);
        }

        build.addOperands(asSwitch, newYields, ArrayRef<HWValue>::emptyRef());
        destroyList.emplace_back(asSwitch);
        break;
      }

      default:
        break;
      }
    }

    if (depth == 0) {
      build.setInsertPoint(proc.block().end());
      for (auto [obj, regState] : regMap) {
        if (regState.stack.empty())
          continue;
        if (regState.get().untouched)
          continue;
        auto reg = RegisterRef{ctx.getRegs().resolve(obj)};

        uint32_t addr = 0;
        uint32_t len = 0;
        Optional<uint16_t> triggerID;

        auto commitPrev = [&]() {
          if (len == 0)
            return;
          TriggerIRef trigger = nullref;
          if (triggerID) {
            trigger = ctx.getTriggers()
                          .resolve(ObjRef<Trigger>{ObjID{*triggerID}})
                          .iref();
          }
          build.buildStore(reg, regState.get().get(build, addr, len),
                           BitRange{addr, len}, config.mode == Config::DEFERRED,
                           trigger);
        };
        for (auto frag : regState.get().frags) {
          if (frag.untouched) {
            commitPrev();
            addr = frag.dstAddr + frag.len;
            len = 0;
          } else {
            if (frag.triggerID != triggerID) {
              commitPrev();
              addr = frag.dstAddr;
              len = 0;
            }
            len += frag.len;
            triggerID = frag.triggerID;
          }
        }
        commitPrev();
      }
    }

    for (auto obj : Range{destroyList}.reverse()) {
      DEBUG("SSAConstruct", {
        dbgs() << "depth=" << depth << ", destroying: ";
        dumpObj(obj);
      })
      build.destroyObj(obj);
    }
  }

  void runOnProc(ModuleIRef mod, ProcessIRef proc) {
    // ObjMapVec regMap assumes that we have reasonably few (or just one)
    // modules in the context. Otherwise better use some other type of map
    // or double indirection for mapping without eager allocation.
    regMap.clear();
    regMap.resize(ctx.getRegs().numIDs());

    runOnBlock(proc, proc.block());
  }

  void runOnModule(ModuleIRef mod) {
    for (auto proc : mod.procs()) {
      runOnProc(mod, proc);
    }
  }

  void run() {
    isNewInstr.resize(ctx.getInstrs().numIDs());
    auto &createHooks = ctx.getInstrs().createHooks;
    auto hookSize = createHooks.size();

    if (config.mode == Config::DEFERRED)
      createHooks.emplace_back(
          [&](InstrRef ref) { isNewInstr.get_ensure(ref) = true; });

    for (auto mod : Range{ctx.getModules()}.as<ModuleRef>()) {
      runOnModule(mod.iref());
    }

    createHooks.resize(hookSize);
  }
};

}; // namespace dyno
