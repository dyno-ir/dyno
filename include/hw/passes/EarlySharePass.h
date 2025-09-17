#pragma once

#include "dyno/Obj.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/HWValue.h"
#include "hw/LoadStore.h"
#include "op/IDs.h"
#include "op/StructuredControlFlow.h"
#include "support/Utility.h"
namespace dyno {

class EarlySharePass {
  HWContext &ctx;

public:
  struct Config {
    DialectOpcode opToShare = OP_ADD;
  };
  Config config;

private:
  static bool orderInstrs(InstrRef lhs, InstrRef rhs) {
    if (lhs.getNumOthers() < rhs.getNumOthers())
      return true;
    if (*lhs.def(0)->as<WireRef>().getNumBits() <
        *rhs.def(0)->as<WireRef>().getNumBits())
      return true;
    return false;
  }

  struct BlockResult {
    SmallVec<InstrRef, 4> candidates;

    void addCandidate(InstrRef instr) { candidates.emplace_back(instr); }
    void sort() { Range{candidates}.stable_sort(orderInstrs); }
  };

  bool isCandidate(InstrRef instr) {
    if (!instr.isOpc(config.opToShare))
      return false;
    if (config.opToShare == HW_SPLICE) {
      if (instr.as<SpliceIRef>().isConstantOffs())
        return false;
    }
    if (config.opToShare == HW_INSERT) {
      if (instr.as<InsertIRef>().isConstantOffs())
        return false;
    }
    return true;
  }

  InstrRef tryMergeCommOps(ArrayRef<InstrRef> instrs) {
    auto maxOps = instrs[0].getNumOthers();
    auto maxBits = *instrs[0].def(0)->as<WireRef>().getNumBits();
    for (auto instr : Range{instrs}.drop_front()) {
      maxOps = std::max(maxOps, instr.getNumOthers());
      maxBits = std::max(maxBits, *instr.def(0)->as<WireRef>().getNumBits());
    }
    auto mod = HWInstrRef{instrs[0]}.parentMod(ctx);

    HWInstrBuilder build{ctx};
    HWInstrBuilder regBuild{ctx};
    regBuild.setInsertPoint(mod.regs_end());

    SmallVec<RegisterRef, 4> regs;
    build.setInsertPoint(HWInstrRef{instrs[0]}.parentProc(ctx).block().begin());
    regs.reserve(maxOps);
    for (uint i = 0; i < maxOps; i++) {
      auto reg = regs.emplace_back(regBuild.buildRegister(maxBits));
      build.buildStore(reg, ctx.constBuild().undef(maxBits).get());
    }

    RegisterRef resultReg = regBuild.buildRegister(maxBits);

    for (auto instr : instrs) {
      build.setInsertPoint(instr);
      for (auto [i, use] : Range{instr.others()}.enumerate()) {
        build.buildStore(
            regs[i], build.buildExt(maxBits, use->as<HWValue>(), OP_ANYEXT));
      }
      for (uint i = instr.getNumOthers(); i < regs.size(); i++)
        build.buildStore(regs[i], ctx.constBuild().zero(maxBits).get());

      auto reqBits = *instr.def()->as<WireRef>().getNumBits();
      instr.def(0)->as<WireRef>().replaceAllUsesWith(
          build.buildLoad(resultReg, reqBits));
    }

    build.setInsertPoint(mod.regs_end());
    auto proc = build.buildProcess();
    build.setInsertPoint(proc.block().end());
    auto range = Range{regs}.transform(
        [&](size_t, RegisterRef reg) { return build.buildLoad(reg); });

    auto ib =
        build.buildInstrRaw(instrs[0].getDialectOpcode(), 1 + regs.size());
    auto defW = ctx.getWires().create(maxBits);
    build.buildStore(resultReg, defW);

    build.setInsertPoint(ib.instr());
    ib.addRef(defW).other();
    ib.addRefs(range);

    return ib.instr();
  }

  template <typename T = SpliceIRef>
  InstrRef tryMergeSpliceInsert(ArrayRef<InstrRef> instrs) {
    auto base = instrs.front().as<T>();
    // todo: relax comparison. addressing does not have to be exactly equal,
    // shared implementation just has to be beneficial.
    for (auto instr : instrs.drop_front()) {
      auto splice = instr.as<T>();
      if (splice.in()->template as<HWValue>() !=
          base.in()->template as<HWValue>())
        return nullref;
      if (splice.getNumTerms() != base.getNumTerms())
        return nullref;
      if (splice.getBase() != base.getBase())
        return nullref;
      if (splice.getLen() != base.getLen())
        return nullref;

      for (auto [a, b] : base.terms().zip(splice.terms())) {
        if (a.getFact() != b.getFact())
          return nullref;
        if (a.getMax() != b.getMax())
          return nullref;
      }
    }

    auto numTerms = base.getNumTerms();
    auto resultBits =
        std::is_same_v<T, SpliceIRef> ? base.getLen() : base.getMemoryLen();

    auto mod = HWInstrRef{instrs[0]}.parentMod(ctx);

    HWInstrBuilder build{ctx};
    HWInstrBuilder regBuild{ctx};
    regBuild.setInsertPoint(mod.regs_end());

    SmallVec<RegisterRef, 4> regs;
    build.setInsertPoint(HWInstrRef{instrs[0]}.parentProc(ctx).block().begin());
    regs.reserve(numTerms);
    for (uint i = 0; i < numTerms; i++) {
      auto reg = regs.emplace_back(regBuild.buildRegister(32));
      build.buildStore(reg, ctx.constBuild().undef(32).get());
    }
    RegisterRef inputReg = regBuild.buildRegister(base.getMemoryLen());
    build.buildStore(inputReg,
                     ctx.constBuild().undef(base.getMemoryLen()).get());

    RegisterRef valueReg;
    if constexpr (requires { base.val(); }) {
      valueReg = regBuild.buildRegister(base.getLen());
    }

    RegisterRef resultReg = regBuild.buildRegister(resultBits);

    for (auto instr : instrs) {
      auto splice = instr.as<T>();
      build.setInsertPoint(instr);
      build.buildStore(inputReg, splice.in()->template as<HWValue>());
      if constexpr (requires { base.val(); }) {
        build.buildStore(valueReg, splice.val()->template as<HWValue>());
      }
      for (auto [i, term] : Range{splice.terms()}.enumerate()) {
        build.buildStore(regs[i], term.getIdx());
      }
      instr.def(0)->as<WireRef>().replaceAllUsesWith(
          build.buildLoad(resultReg));
    }

    build.setInsertPoint(mod.regs_end());
    auto proc = build.buildProcess();
    build.setInsertPoint(proc.block().end());
    auto terms =
        base.terms().transform([&](size_t i, AddressGenTermOperand ref) {
          auto point = build.insert;
          build.setInsertPoint(proc.block().begin());
          auto rv = AddressGenTerm{build.buildLoad(regs[i]), ref.getFact(),
                                   ref.getMax()};
          build.setInsertPoint(proc.block().end());
          return rv;
        });
    HWValue val;
    if constexpr (requires { base.val(); }) {
      val = build.buildInsert(build.buildLoad(inputReg),
                              build.buildLoad(valueReg), base.getBase(), terms);
    } else {
      val = build.buildSplice(build.buildLoad(inputReg), base.getLen(),
                              base.getBase(), terms);
    }

    build.setInsertPoint(proc.block().end());
    build.buildStore(resultReg, val);
    return val.as<WireRef>().getDefI();
  }

  InstrRef tryMerge(ArrayRef<InstrRef> instrs) {
    if (config.opToShare.is(OP_ADD, OP_MUL))
      return tryMergeCommOps(instrs);
    if (config.opToShare.is(HW_SPLICE))
      return tryMergeSpliceInsert<SpliceIRef>(instrs);
    if (config.opToShare.is(HW_INSERT))
      return tryMergeSpliceInsert<InsertIRef>(instrs);
    dyno_unreachable("merging unimplemented");
  }

  auto findMergeCandidates(SmallVecImpl<BlockResult> &results) {
    // iterate, incrementing smallest one every iter.
    // if tryMerge succeeds, replace all with nullref and increment all.

    SmallVec<uint32_t, 4> idxs(results.size());

    SmallVec<uint32_t, 16> mergeCandidates;
    SmallVec<uint32_t, 4> mergeCandidatesStartIdxs;

    SmallVec<InstrRef, 4> mergedInstrs;

    auto hasMore = [&](size_t i) -> bool {
      return idxs[i] < results[i].candidates.size();
    };

    // To avoid quadratic runtime we sort instrs by number of operands
    // and bit size first and then only consider merging adjacent instrs.
    while (true) {
      size_t smallestIdx;
      InstrRef smallestInstr = nullref;

      for (size_t i = 0; i < results.size(); ++i) {
        if (!hasMore(i))
          continue;

        InstrRef cur = results[i].candidates[idxs[i]];
        if (!smallestInstr || orderInstrs(cur, smallestInstr)) {
          smallestInstr = cur;
          smallestIdx = i;
        }
      }

      SmallVec<InstrRef, 4> curInstrs;
      for (size_t i = 0; i < results.size(); ++i) {
        if (!hasMore(i))
          continue;
        curInstrs.emplace_back(results[i].candidates[idxs[i]]);
      }
      if (curInstrs.size() < 2)
        break;

      if (auto mergedInstr = tryMerge(curInstrs)) {
        for (size_t i = 0; i < results.size(); ++i) {
          if (!hasMore(i))
            continue;
          results[i].candidates[idxs[i]] = nullref;
          ++idxs[i];
        }
        mergedInstrs.emplace_back(mergedInstr);
      } else {
        ++idxs[smallestIdx];
      }
    }

    for (auto &res : results) {
      uint64_t idx = 0;
      for (auto cand : res.candidates) {
        if (cand == nullref)
          continue;
        res.candidates[idx++] = cand;
      }
      res.candidates.downsize(idx);
    }

    return mergedInstrs;
  }

  void handleMultiway(BlockResult &curRes, ArrayRef<BlockRef> blocks) {
    SmallVec<BlockResult, 4> results;
    results.reserve(blocks.size());

    for (auto block : blocks) {
      auto &res = results.emplace_back(runOnBlock(block));
      res.sort();
    }

    auto mergedInstrs = findMergeCandidates(results);

    for (auto res : results) {
      curRes.candidates.push_back_range(Range{res.candidates});
    }

    // also make sucessfully merged instrs candidates again.
    curRes.candidates.push_back_range(Range{mergedInstrs});
  }

  BlockResult runOnBlock(BlockRef block) {
    BlockResult res;
    for (auto instr : block) {
      switch (*instr.getDialectOpcode()) {
      case *OP_IF: {
        auto asIf = instr.as<IfInstrRef>();
        if (!asIf.hasFalseBlock())
          break;
        auto blocks =
            std::to_array({asIf.getTrueBlock(), asIf.getFalseBlock()});
        handleMultiway(res, blocks);
        break;
      }

      case *OP_SWITCH: {
        auto asSwitch = instr.as<SwitchInstrRef>();
        if (asSwitch.getNumCases() < 2)
          break;
        SmallVec<BlockRef, 4> blocks;
        blocks.reserve(asSwitch.getNumCases());
        blocks.push_back_range(asSwitch.caseBlocks());
        handleMultiway(res, blocks);
        break;
      }

      default: {
        if (isCandidate(instr))
          res.addCandidate(instr);
        break;
      }
      }
    }

    return res;
  }

  void runOnProcess(ProcessIRef proc) { runOnBlock(proc.block()); }

  void runOnModule(ModuleIRef mod) {
    for (auto proc : mod.procs())
      runOnProcess(proc);
  }

public:
  explicit EarlySharePass(HWContext &ctx) : ctx(ctx) {}
  void run() {
    for (auto mod : ctx.activeModules()) {
      runOnModule(mod.iref());
    }
  }
};

}; // namespace dyno
