#pragma once
#include "dyno/Obj.h"
#include "hw/HWAbstraction.h"
#include "hw/HWInstr.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "op/IDs.h"
#include "op/StructuredControlFlow.h"
#include "support/Ranges.h"

namespace dyno {

class SSAConstructPass {
  HWContext &ctx;
  uint depth = 0;

  // we essentially need a concat value style recursive value representation.
  // values may be:
  // -> HWValue
  // -> maybe store directly in case we want to backref.
  // -> old value lazy backref?
  struct RegisterValue {
    struct Fragment {
      DynObjRef ref;
      uint32_t srcAddr;
      uint32_t dstAddr;
      uint32_t len;

      std::pair<HWValue, BitRange> getValue(HWInstrBuilder &build,
                                            uint32_t addr, uint32_t len) {
        addr += srcAddr;
        if (auto regThin = ref.dyn_as<ObjRef<Register>>()) {
          auto reg = RegisterRef{build.ctx.getRegs().resolve(regThin)};
          return std::make_pair(build.buildLoad(reg, BitRange{addr, len}),
                                BitRange{0, len});
        } else if (auto wireThin = ref.dyn_as<ObjRef<Wire>>()) {
          auto wire = WireRef{build.ctx.getWires().resolve(wireThin)};
          return std::make_pair(wire, BitRange{addr, len});
        } else if (ref.is<ObjRef<Constant>>()) {
          auto constant = ref.isCustom()
                              ? ConstantRef{ref}
                              : ConstantRef{build.ctx.getConstants().resolve(
                                    ref.as<ObjRef<Constant>>())};
          return std::make_pair(constant, BitRange{addr, len});
        } else {
          assert(0 && "unsupported");
          auto store =
              StoreIRef{build.ctx.getInstrs().resolve(ref.as<ObjRef<Instr>>())};
          assert(len <= this->len);
          return std::make_pair(store.value(), BitRange{addr, len});
        }
      }
    };
    // sorted by dst addr
    SmallVec<Fragment, 1> frags;
    uint32_t depth;
    bool untouched;

    RegisterValue() = default;
    RegisterValue(const RegisterValue &) = default;
    RegisterValue(RegisterValue &&) = default;
    RegisterValue &operator=(const RegisterValue &) = default;
    RegisterValue &operator=(RegisterValue &&) = default;

    RegisterValue(RegisterRef reg, uint32_t depth)
        : frags{{reg, 0, 0, reg->numBits}}, depth(depth) {}
    RegisterValue(DynObjRef value, uint32_t bits, uint32_t depth)
        : frags{{value, 0, 0, bits}}, depth(depth) {}

    // returns last iterator before regions with higher start addr.
    auto getInsertIt(uint32_t dstAddr) {
      // todo: binary search when large
      auto it = frags.begin();
      while (it != frags.end() && it->dstAddr <= dstAddr)
        it++;
      if (it == frags.begin())
        return it;
      return it - 1;
    }

    void overwrite(DynObjRef ref, uint32_t srcAddr, uint32_t dstAddr,
                   uint32_t len) {
      untouched = false;
      uint32_t newStart = dstAddr;
      uint32_t newEnd = dstAddr + len;

      auto it = getInsertIt(newStart);
      auto insertPos = it - frags.begin();

      // adjust or remove overlapping existing fragments
      while (it != frags.end()) {
        uint32_t start = it->dstAddr;
        uint32_t end = it->dstAddr + it->len;

        if (start >= newEnd)
          break;

        if (end <= newStart) {
          ++it;
          assert(false);
          continue;
        }

        // full cover: split existing into two
        if (newStart > start && newEnd < end) {
          uint32_t leftLen = newStart - start;
          uint32_t rightLen = end - newEnd;
          // create right
          Fragment right{it->ref, it->srcAddr + (newEnd - start), newEnd,
                         rightLen};
          // shrink left
          it->len = leftLen;
          frags.insert(it + 1, right);
          ++insertPos;
          break;
        }

        // overlap on left: trim right side of existing
        if (start < newStart && end > newStart) {
          it->len = newStart - start;
          ++it;
          ++insertPos;
          continue;
        }

        // overlap on right: trim left side of existing
        if (start < newEnd && end > newEnd) {
          uint32_t cut = newEnd - start;
          it->srcAddr += cut;
          it->dstAddr = newEnd;
          it->len = end - newEnd;
          ++it;
          continue;
        }

        // else: existing perfectly matches new
        *it = Fragment{
            ref,
            srcAddr,
            newStart,
            len,
        };
        return;
      }

      // insert new fragment in sorted order
      frags.insert(frags.begin() + insertPos,
                   Fragment{ref, srcAddr, newStart, len});
    }

    HWValue get(HWInstrBuilder &build, uint32_t addr, uint32_t len,
                bool update = true) {
      auto it = getInsertIt(addr);

      auto addrB = addr;
      auto lenB = len;

      uint32_t end = addr + len;

      SmallVec<std::pair<HWValue, BitRange>, 4> operands;

      while (it != frags.end() && len != 0) {
        uint32_t itEnd = it->dstAddr + it->len;

        if (addr < itEnd && it->dstAddr < end) {

          uint32_t start = addr - it->dstAddr;
          if (it->dstAddr > addr)
            start = 0;

          uint32_t pieceLen =
              std::min(end, itEnd) - std::max(addr, it->dstAddr);
          operands.emplace_back(it->getValue(build, start, pieceLen));

          addr += pieceLen;
          len -= pieceLen;
          it++;
        } else
          break;
      }

      auto rv = build.buildSplice(ArrayRef{operands});
      if (update)
        overwrite(rv, 0, addrB, lenB);
      return rv;
    }

    HWValue get(HWInstrBuilder &build, bool update = true) {
      SmallVec<std::pair<HWValue, BitRange>, 4> operands;
      uint32_t len = 0;
      for (auto frag : frags) {
        operands.emplace_back(frag.getValue(build, 0, frag.len));
        len += frag.len;
      }

      auto rv = build.buildSplice(ArrayRef{operands});
      if (update)
        overwrite(rv, 0, 0, len);
      return rv;
    }

    // void replaceDefault(HWInstrBuilder &build, RegisterValue &newDefault) {
    //   for (auto frag : frags) {
    //     if (frag.ref.is<RegisterRef>()) {
    //       // todo: don't always materialize value. can just set ref in many
    //       // cases.
    //       overwrite(newDefault.get(build, frag.dstAddr, frag.len), 0,
    //                 frag.dstAddr, frag.len);
    //     }
    //   }
    // }

    friend bool operator==(const RegisterValue &lhs, const RegisterValue &rhs) {
      if (lhs.frags.size() != rhs.frags.size())
        return false;
      for (size_t i = 0; i < lhs.frags.size(); i++)
        if (lhs.frags[i].ref != rhs.frags[i].ref ||
            lhs.frags[i].srcAddr != rhs.frags[i].srcAddr ||
            lhs.frags[i].dstAddr != rhs.frags[i].dstAddr ||
            lhs.frags[i].len != rhs.frags[i].len)
          return false;
      return true;
    }
  };

  auto diffRegisterValues(ArrayRef<RegisterValue *> regVals) {
    SmallVec<std::pair<uint32_t, uint32_t>, 4> diffs;

    SmallVec<uint32_t, 4> idxs(regVals.size());

    uint32_t curAddr = 0;

    while (idxs[0] < regVals[0]->frags.size()) {
      uint32_t overlapEnd = UINT32_MAX;
      bool equalChunk = true;

      uint32_t sa;
      DynObjRef ref;

      for (size_t i = 0; i < regVals.size(); i++) {
        auto &frag = regVals[i]->frags[idxs[i]];
        uint32_t end = frag.dstAddr + frag.len;
        overlapEnd = std::min(overlapEnd, end);

        uint32_t sa2 = frag.srcAddr + (curAddr - frag.dstAddr);
        DynObjRef ref2 = frag.ref;
        if (i == 0) {
          sa = sa2;
          ref = ref2;
        } else
          equalChunk &= (ref == ref2) && (sa2 == sa);
      }
      uint32_t chunkLen = overlapEnd - curAddr;

      if (!equalChunk) {
        // either start a new diff run or extend the last one
        if (!diffs.empty() &&
            diffs.back().first + diffs.back().second == curAddr) {
          diffs.back().second += chunkLen;
        } else
          diffs.emplace_back(curAddr, chunkLen);
      }

      curAddr = overlapEnd;

      // advance fragment indices when we hit their end
      for (size_t i = 0; i < regVals.size(); i++) {
        auto frag = regVals[i]->frags[idxs[i]];
        uint32_t end = frag.dstAddr + frag.len;
        if (curAddr == end)
          idxs[i]++;
      }
    }

    return diffs;
  }

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
    uint numNewYieldVals;

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
    SmallVec<SmallVec<HWValue, 4>, 2> yieldVals(wayBlocks.size());
    MultiwayResult rv;

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
      if (allUntouched)
        continue;

      auto diffs = diffRegisterValues(vals);
      if (diffs.empty())
        continue;

      for (auto [i, block] : Range{wayBlocks}.enumerate()) {
        // Three Options
        // 1. Value set in this block => just yield this block's version.
        // 2. Value set in parent block => yield parent's version.
        // 3. Value not set at all => load register and yield that.

        build.setInsertPoint(BlockRef{block}.end());

        yieldVals[i].reserve(yieldVals[i].size() + diffs.size());
        for (auto diff : diffs)
          yieldVals[i].emplace_back(
              vals[i]->get(build, diff.first, diff.second, false));
      }

      for (auto diff : diffs)
        rv.yieldRegs.emplace_back(reg, diff);

      regState.stack.resize(i + 1);
    }

    if (yieldVals[0].empty())
      return {};

    auto createYield = [&](BlockRef block, ArrayRef<HWValue> vals) {
      InstrRef lastYield = nullref;
      if (!block.empty() && block.end().pred()->isOpc(OP_YIELD))
        lastYield = block.end().pred().instr();
      build.setInsertPoint(block.end());
      build.buildYield(lastYield, vals);
    };

    for (size_t i = 0; i < wayBlocks.size(); i++)
      createYield(wayBlocks[i], yieldVals[i]);

    rv.numNewYieldVals = yieldVals[0].size();

    return rv;
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

        if (!regState.has(depth))
          // todo: still update known value to materialized
          break;

        auto &val = regState.getOrSetDefault(depth, asLoad.reg());
        // todo: only load from defer

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
        assert(asIf.hasFalseBlock() && "todo");
        auto startDepth = depth;

        std::array<BlockRef, 2> blocks{asIf.getTrueBlock(),
                                       asIf.getFalseBlock()};
        auto res = runOnMultiway(proc, blocks);

        // Create yields for modified register values.
        uint oldYieldVals = asIf.getNumYieldValues();
        uint newYieldVals = res.numNewYieldVals + oldYieldVals;

        build.setInsertPoint(HWInstrRef{asIf}.iter(ctx));
        auto newIf = build.buildIfElse(asIf, newYieldVals);
        depth = startDepth;

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
