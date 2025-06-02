#pragma once
#include "dyno/CFG.h"
#include "dyno/Obj.h"
#include "hw/HWAbstraction.h"
#include "hw/HWInstr.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/analysis/RegisterValue.h"
#include "op/IDs.h"
#include "op/StructuredControlFlow.h"
#include "support/ArrayRef.h"
#include "support/Ranges.h"

namespace dyno {

class SSAConstructPass {
  HWContext &ctx;
  uint depth = 0;

  struct RegState {
    SmallVec<RegisterValue, 4> stack;

    RegisterValue &getOrSetDefault(uint depth, RegisterRef reg) {
      if (!stack.empty() && stack.back().depth == depth)
        return stack.back();
      else
        return stack.emplace_back(RegisterValue{reg, depth});
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
      getOrSetDefault(depth, reg) = RegisterValue{reg, depth};
    }

    void plainValue(uint depth, RegisterRef reg, HWValue value) {
      getOrSetDefault(depth, reg) = RegisterValue{value, reg->numBits, depth};
    }
  };

  ObjMapVec<Register, RegState> regMap;

  InstrRef resolve(ObjRef<Instr> ref) {
    if (!ref)
      return nullref;
    return InstrRef{ctx.getInstrs().resolve(ref)};
  }

public:
  explicit SSAConstructPass(HWContext &ctx) : ctx(ctx) {}

  struct MultiwayResult {
    SmallVec<std::pair<RegisterRef, std::pair<uint32_t, uint32_t>>, 2>
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
    SmallVec<std::pair<ObjRef<Register>, RegisterValue *>, 4> priorValues;
    for (auto [obj, regState] : regMap) {
      if (regState.has(startDepth)) {
        auto &value = regState.get();
        auto &copy = regState.stack.emplace_back(value);
        copy.depth = depth + 1;
        copy.untouched = true;
        priorValues.emplace_back(obj, &value);
      }
    }
    // first
    depth++;
    runOnBlock(proc, wayBlocks[0]);
    // remaining
    for (auto block : Range{wayBlocks}.drop_front()) {
      for (auto [reg, value] : priorValues) {
        auto &copy = regMap[reg].stack.emplace_back(*value);
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

      RegisterValue lazyLoad{reg, startDepth};
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

        auto insert = BlockRef{block}.end();
        if (!BlockRef{block}.empty() && insert.pred()->isOpc(OP_YIELD))
          --insert;
        build.setInsertPoint(insert);

        rv.yieldVals[i].reserve(rv.yieldVals[i].size() + diffs.size());
        for (auto diff : diffs)
          rv.yieldVals[i].emplace_back(
              vals[i]->get(build, diff.first, diff.second, false));
      }

      for (auto diff : diffs)
        rv.yieldRegs.emplace_back(reg, diff);

      regState.stack.resize(i + 1);
    }
    depth = startDepth;
    return rv;
  }

  // todo: unmutable array ref, requires fixing const block iter
  template <uint numLoopBlocks>
  auto analyzeAndCreateLoopYields(HWInstrBuilder &build,
                                  MutArrayRef<BlockRef> loopBlocks) {
    assert(loopBlocks.size() == numLoopBlocks);
    auto startDepth = depth - numLoopBlocks;
    SmallVec<std::pair<RegisterRef, std::pair<uint32_t, uint32_t>>, 4>
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
          auto &copy = state.stack.emplace_back(state.get());
          copy.depth = startDepth + i;
        }
        continue;
      }
      auto reg = RegisterRef{ctx.getRegs().resolve(obj)};

      // check if parent block has a state for this reg.
      RegisterValue defaultVal{reg, reg->numBits, ~0U};
      std::array<RegisterValue *, numLoopBlocks + 1> blockVals;
      {
        size_t i = 0;
        while (i <= numLoopBlocks) {
          uint32_t expectedDepth = startDepth + numLoopBlocks - i;
          if (state.stack.size() < i + 1) {
            state.stack.insert(state.stack.begin(),
                               RegisterValue{reg, expectedDepth});
          } else {
            auto &entryBelow = state.stack[state.stack.size() - i - 1];
            if (entryBelow.depth != expectedDepth) {
              state.stack.insert(&entryBelow + 1,
                                 RegisterValue{reg, expectedDepth});
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

      // invert the diff on the fly to get invariant regions.
      // overwrite those with parent val if exists.
      uint32_t invDiffAddr = 0;
      auto invDiffStep = [&](std::pair<uint32_t, uint32_t> diff) {
        // no use if no parent val
        if (!parentVal)
          return;
        if (uint32_t invDiffLen = diff.first - invDiffAddr) {
          for (auto *val : Range{blockVals}.drop_front()) {
            val->overwriteNoMaterialize(*parentVal, invDiffAddr, invDiffAddr,
                                        invDiffLen);
          }
        }
        invDiffAddr = diff.first + diff.second;
      };

      for (auto diff : diffs) {
        // modified anywhere in loop -> becomes yield value in every loop
        // block.
        yieldVals.emplace_back(reg, diff);

        for (auto [i, val] : Range{blockVals}.drop_front().enumerate()) {
          // we need to materialize yield values here before we overwrite
          // them.
          build.setInsertPoint(loopBlocks[i].end());
          if (!loopBlocks[i].empty() &&
              loopBlocks[i].end().pred()->isOpc(OP_YIELD)) {
            build.setInsertPoint(loopBlocks[i].end().pred());
          }
          auto matVal = val->get(build, diff.first, diff.second, false);
          materializedYieldVals[i].emplace_back(matVal);

          // now overwrite with unyield wire for next pass (source for this
          // value becomes yield of previous).
          auto wire =
              unyieldWires[i].emplace_back(ctx.getWires().create(diff.second));
          val->overwrite(wire, 0, diff.first, diff.second);
        }

        invDiffStep(diff);
      }
      // fencepost step
      invDiffStep(std::make_pair(reg->numBits, 0));
    }

    return std::make_tuple(yieldVals, materializedYieldVals, unyieldWires);
  }

  // this is not really needed, template also works with size == 1.
  // already written and quite a bit more efficient though, so keep this for
  // now.
  template <>
  auto analyzeAndCreateLoopYields<1>(HWInstrBuilder &build,
                                     MutArrayRef<BlockRef> loopBlocks) {
    assert(loopBlocks.size() == 1);
    uint startDepth = depth - 1;
    uint bodyDepth = depth;

    SmallVec<std::pair<RegisterRef, std::pair<uint32_t, uint32_t>>, 4>
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

      auto &val = state.get();
      auto reg = RegisterRef{ctx.getRegs().resolve(obj)};

      for (auto &frag : val.frags) {
        // skip default frags
        if (frag.ref == obj && frag.srcAddr == frag.dstAddr) {
          // for next pass: overwrite untouched frag with parent's value.
          if (parentVal)
            val.overwriteNoMaterialize(*parentVal, &frag);
          continue;
        }
        yieldVals.emplace_back(reg, std::make_pair(frag.dstAddr, frag.len));

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
      ArrayRef<std::pair<RegisterRef, std::pair<uint32_t, uint32_t>>>
          yieldVals) {
    if (yieldVals.size() == 0)
      return false;

    build.setInsertPoint(HWInstrRef{loopInstr}.iter(ctx));

    SmallVec<HWValue, 4> newUses;
    newUses.reserve(yieldVals.size());
    for (auto [reg, range] : yieldVals) {
      auto &val = regMap[reg].getOrSetDefault(depth, reg);
      // set initial yield vals to value before loop.
      newUses.emplace_back(val.get(build, range.first, range.second, false));
    }

    SmallVec<WireRef, 4> newDefs;
    newDefs.reserve(yieldVals.size());

    for (auto [reg, range] : yieldVals) {
      auto &val = regMap[reg].getOrSetDefault(depth, reg);
      // set reg state after loop to output yield vals.
      auto wire = ctx.getWires().create(range.second);
      newDefs.emplace_back(wire);
      val.overwrite(wire, 0, range.first, range.second);
    }

    build.addOperands(loopInstr, newDefs, newUses);
    return true;
  }

  void runOnBlock(ProcessIRef proc, BlockRef block) {

    HWInstrBuilder build{ctx};
    SmallVec<FatDynObjRef<>, 32> destroyList;

    for (auto instr : block) {
      switch (*instr.getDialectOpcode()) {
      case *HW_STORE: {
        auto asStore = instr.as<StoreIRef>();
        auto &regState = regMap[asStore.reg()];

        uint32_t addr = 0;
        uint32_t len = asStore.reg()->numBits;
        if (auto range = asStore.range()) {
          if (!range->isConstant()) {
            build.setInsertPoint(asStore.iter(ctx));
            auto val = regState.getOrSetDefault(depth, asStore.reg())
                           .get(build, 0, asStore.reg()->numBits);
            val = build.buildInsert(val, asStore.value(), *asStore.range());
            regState.plainValue(depth, asStore.reg(), val);
            destroyList.emplace_back(asStore);
            break;
          }
          addr = range->getAddr().as<ConstantRef>().getExactVal();
          len = range->getLen().as<ConstantRef>().getExactVal();
        }

        destroyList.emplace_back(asStore);
        regState.getOrSetDefault(depth, asStore.reg())
            .overwrite(asStore.value(), 0, addr, len);
        break;
      }

        // case *HW_STORE_DEFER: {
        //   auto asStore = instr.as<StoreIRef>();
        //   auto &regState = regMap[asStore.reg()];
        //   assert(0);
        //   break;
        // }

      case *HW_LOAD: {
        auto asLoad = instr.as<LoadIRef>();
        auto &regState = regMap[asLoad.reg()];

        auto &val = regState.getOrSetDefault(depth, asLoad.reg());

        uint32_t addr = 0;
        uint32_t len = asLoad.reg()->numBits;
        if (asLoad.hasRange()) {
          auto range = asLoad.range();
          if (!range.isConstant())
            // todo load via splice (ideally only if any overlapping bits known)
            break;
          addr = range.getAddr().as<ConstantRef>().getExactVal();
          len = range.getLen().as<ConstantRef>().getExactVal();
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

        for (auto [i, pair] : Range{res.yieldRegs}.enumerate()) {
          auto reg = pair.first;
          auto range = pair.second;

          auto yieldVal = newIf.getYieldValue(i + oldYieldVals)->as<WireRef>();
          yieldVal->numBits = range.second;
          regMap[reg]
              .getOrSetDefault(depth, reg)
              .overwrite(yieldVal, 0, range.first, range.second);
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
            analyzeAndCreateLoopYields<loopBlocks.size()>(build, loopBlocks);

        // second pass (in reverse order, pop values from top of stack)
        size_t i = loopBlocks.size() - 1;
        for (auto block : Range{loopBlocks}.reverse()) {
          // build yield and unyield
          auto [oldY, newY] =
              build.extendOrNewYield(block, materializedYieldVals[i]);
          if (oldY)
            destroyList.emplace_back(oldY);
          auto [oldUY, newUY] =
              build.extendOrNewUnyield(block, unyieldWires[i]);
          if (oldUY)
            destroyList.emplace_back(oldY);

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
            analyzeAndCreateLoopYields<1>(build, MutArrayRef{&block, 1});

        auto [oldY, newY] =
            build.extendOrNewYield(block, materializedYieldVals);
        if (oldY)
          destroyList.emplace_back(oldY);
        auto [oldUY, newUY] = build.extendOrNewUnyield(block, unyieldWires);
        if (oldUY)
          destroyList.emplace_back(oldUY);

        // Second pass, can now resolve loads from both yield and invariant
        // vals.
        runOnBlock(proc, block);

        // Clear body state.
        for (auto [obj, state] : regMap) {
          if (state.has(bodyDepth))
            state.stack.pop_back();
        }
        depth = startDepth;

        bool deleteOld = addYieldsToLoopInstr(build, asWhile, yieldVals);
        if (deleteOld)
          destroyList.emplace_back(asWhile);

        break;
      }

      case *OP_SWITCH: {

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
        auto reg = RegisterRef{ctx.getRegs().resolve(obj)};

        build.buildStore(
            reg, regState.getOrSetDefault(0, reg).get(build, 0, reg->numBits));
      }
    }

    for (auto obj : Range{destroyList}.reverse())
      build.destroyObj(obj);
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
    for (auto mod : Range{ctx.getModules()}.as<ModuleRef>()) {
      runOnModule(mod.iref());
    }
  }
};

}; // namespace dyno
