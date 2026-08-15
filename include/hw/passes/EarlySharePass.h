#pragma once

#include "dyno/Context.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "dyno/ObjMap.h"
#include "dyno/Pass.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Wire.h"
#include "hw/analysis/WireVariable.h"
#include "op/IDs.h"
#include "op/StructuredControlFlow.h"
#include "support/Bits.h"
#include "support/Debug.h"
#include "support/DenseMap.h"
#include "support/DenseMultimap.h"
#include "support/TemplateUtil.h"
#include "support/TwoLevelSet.h"
#include "support/Utility.h"
#include <bit>
#include <initializer_list>
#include <type_traits>
namespace dyno {

class EarlySharePass : public Pass<EarlySharePass> {
  Context &ctx;

public:
#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM)                                      \
  FIELD(DialectOpcode, opToShare, OP_ADD)                                      \
  FIELD(bool, checkAllPairs, false)
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
  Config config;

private:
  struct PotentialMerge {
    // real instr instance, s.t. we're API compatible w/ regular instructions.
    // Not in CFG and no real defs though.
    SmallVec<InstrRef, 2> sources;
    InstrRef getCanon() const { return sources[0]; }

    explicit PotentialMerge(ArrayRef<PotentialMerge> merges) {
      for (auto &merge : merges)
        sources.push_back_range(Range{merge.sources});
    }
    PotentialMerge() = default;
    PotentialMerge(InstrRef instr) : sources{instr} {}

    explicit operator bool() const { return !sources.empty(); }
  };

  static bool orderInstrs(const PotentialMerge &lhs,
                          const PotentialMerge &rhs) {
    if (lhs.getCanon().getNumOthers() < rhs.getCanon().getNumOthers())
      return true;
    if (*lhs.getCanon().def(0)->as<WireRef>().getNumBits() <
        *rhs.getCanon().def(0)->as<WireRef>().getNumBits())
      return true;
    return false;
  }

  template <typename T>
  bool spliceInsertMergeCompatible(const PotentialMerge &lhs,
                                   const PotentialMerge &rhs) const {
    auto lhsI = lhs.getCanon();
    auto rhsI = rhs.getCanon();

    auto base = lhsI.template as<T>();
    auto baseIn = lookThruRemaps(base.in()->template as<HWValue>());

    auto splice = rhsI.template as<T>();
    auto spliceIn = lookThruRemaps(splice.in()->template as<HWValue>());
    if (spliceIn != baseIn) {
      return false;
    }
    if (splice.getNumTerms() != base.getNumTerms())
      return false;
    if (splice.getBase() != base.getBase())
      return false;
    if (splice.getLen() != base.getLen())
      return false;

    for (auto [a, b] : base.terms().zip(splice.terms())) {
      if (a.getFact() != b.getFact())
        return false;
      if (a.getMax() != b.getMax())
        return false;
    }
    return true;
  }

  bool mergeCompatible(PotentialMerge &lhs, PotentialMerge &rhs) {
    auto lhsI = lhs.getCanon();
    auto rhsI = rhs.getCanon();
    assert(lhsI.getDialectOpcode() == rhsI.getDialectOpcode());
    assert(lhsI.getDialectOpcode() == config.opToShare);

    switch (*config.opToShare) {
      // add/mul are always share compatible
    case *OP_ADD:
    case *OP_MUL:
      return true;

    case *HW_SPLICE:
      return spliceInsertMergeCompatible<SpliceIRef>(lhs, rhs);

    case *HW_INSERT:
      return spliceInsertMergeCompatible<InsertIRef>(lhs, rhs);

    default:
      dyno_unreachable("unexpected opcode");
    }
  }

  template <typename T> uint64_t spliceInsertMergeHash(PotentialMerge &lhs) {
    // hash only used for splice/insert
    assert(lhs.getCanon().isOpc(HW_SPLICE, HW_INSERT));

    auto lhsI = lhs.getCanon();

    auto base = lhsI.template as<T>();
    auto baseIn = lookThruRemaps(base.in()->template as<HWValue>());

    uint64_t hash = hash_u64(std::bit_cast<uint64_t>(baseIn));
    hash = hash_combine64(hash, hash_u64(base.getBase()));
    hash = hash_combine64(hash, hash_u32(base.getLen()));

    for (auto t : base.terms()) {
      hash = hash_combine64(hash, hash_u32(t.getFact()));
      hash = hash_combine64(hash, hash_u32(t.getMax().value_or(~0ULL)));
    }

    return hash;
  }

  uint64_t mergeHash(PotentialMerge &merge) {
    switch (*config.opToShare) {

    case *HW_SPLICE:
      return spliceInsertMergeHash<SpliceIRef>(merge);

    case *HW_INSERT:
      return spliceInsertMergeHash<InsertIRef>(merge);

    default:
      dyno_unreachable("unexpected opcode");
    }
  }

  struct BlockResult {
    Vec<PotentialMerge, 4> candidates;
    void addCandidate(PotentialMerge &&instr) {
      candidates.emplace_back(std::move(instr));
    }
    void addCandidate(const PotentialMerge &instr) {
      candidates.emplace_back(instr);
    }
  };

  void sortBlockResult(BlockResult &res) {
    if (config.opToShare != Any{HW_INSERT, HW_SPLICE})
      Range{res.candidates}.stable_sort(orderInstrs);
  }

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

  InstrRef doMergeCommOps(ArrayRef<InstrRef> instrs) {
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
    for (unsigned i = 0; i < maxOps; i++) {
      auto reg = regs.emplace_back(regBuild.buildRegister(maxBits));
      build.buildStore(
          reg, ConstantBuilder{ctx.getStore<Constant>()}.undef(maxBits).get());
    }

    RegisterRef resultReg = regBuild.buildRegister(maxBits);

    for (auto instr : instrs) {
      build.setInsertPoint(instr);
      for (auto [i, use] : Range{instr.others()}.enumerate()) {
        build.buildStore(
            regs[i], build.buildExt(maxBits, use->as<HWValue>(), OP_ANYEXT));
      }
      for (unsigned i = instr.getNumOthers(); i < regs.size(); i++)
        build.buildStore(
            regs[i],
            ConstantBuilder{ctx.getStore<Constant>()}.zero(maxBits).get());

      auto reqBits = *instr.def()->as<WireRef>().getNumBits();
      instr.def(0)->as<WireRef>().replaceAllUsesWith(
          build.buildLoad(resultReg, reqBits));
    }

    build.setInsertPoint(mod.header_end());
    auto proc = build.buildProcess();
    build.setInsertPoint(proc.block().end());
    auto range = Range{regs}.transform(
        [&](size_t, RegisterRef reg) { return build.buildLoad(reg); });

    auto ib =
        build.buildInstrRaw(instrs[0].getDialectOpcode(), 1 + regs.size());
    auto defW = ctx.getStore<Wire>().create(maxBits);
    build.buildStore(resultReg, defW);

    build.setInsertPoint(ib.instr());
    ib.addRef(defW).other();
    ib.addRefs(range);

    return ib.instr();
  }

  SmallDenseMap<ObjRef<Wire>, ObjRef<Wire>> valueRemap;
  ObjRef<Wire> lookThruRemaps(ObjRef<Wire> wire) const {
    while (auto it = valueRemap.find(wire))
      wire = it.val();
    return wire;
  }
  DynObjRef lookThruRemaps(HWValue val) const {
    if (val.is<WireRef>())
      return lookThruRemaps(val.as<WireRef>());
    return val;
  }

  // // ephemeral
  ProcessIRef proc = nullref;
  ModuleIRef mod = nullref;

  ObjMapVec<Register, bool> isShareReg;
  RegisterRef getShareRegister(HWValue val, RegisterRef existing) {
    auto defI = val.is<WireRef>() ? val.as<WireRef>().getDefI() : nullref;
    if (!defI || !defI.isOpc(HW_LOAD) ||
        !isShareReg.inRange(defI.as<LoadIRef>().reg()) ||
        !isShareReg[defI.as<LoadIRef>().reg()]) {
      if (existing)
        return existing;
      // create register in wire's process, defaulting to 'x
      HWInstrBuilder build{ctx};
      build.setInsertPoint(mod.regs_end());
      auto reg = build.buildRegister(val.getNumBits());
      build.setInsertPoint(proc.block().begin());
      build.buildStore(reg, ConstantBuilder{ctx.getStore<Constant>()}
                                .undef(*val.getNumBits())
                                .get());
      isShareReg.get_ensure(reg) = 1;
      return reg;
    }
    auto load = defI.as<LoadIRef>();

    if (existing) {
      auto reg = load.reg();
      reg.replaceAllUsesWith(existing);
      return existing;
    }

    assert(load.isFullReg());
    return load.reg();
  }

  // template <typename T = SpliceIRef>
  // std::optional<PotentialMerge>
  // potentialMergeSpliceInsert(ArrayRef<PotentialMerge> instrs) {
  //   auto base = instrs.front().getCanon().as<T>();
  //   auto baseIn = lookThruRemaps(base.in()->template as<HWValue>());

  //   // todo: relax comparison. addressing does not have to be exactly equal,
  //   // shared implementation just has to be beneficial.
  //   for (auto instr :
  //        Range{instrs}.drop_front().tf([](auto &&e) { return e.getCanon();
  //        })) {
  //     auto splice = instr.template as<T>();
  //     auto spliceIn = lookThruRemaps(splice.in()->template as<HWValue>());
  //     if (spliceIn != baseIn) {
  //       DYNO_DBG(if (baseIn.template is<WireRef>() &&
  //                    spliceIn.template is<WireRef>()) {
  //         dbgs() << "conflict:\n";
  //         dumpInstr(ctx.resolve(baseIn).template as<WireRef>().getDefI(),
  //         ctx); dumpInstr(ctx.resolve(spliceIn).template
  //         as<WireRef>().getDefI(),
  //                   ctx);
  //         dbgs() << "\n";
  //       })
  //       return nullref;
  //     }
  //     if (splice.getNumTerms() != base.getNumTerms())
  //       return nullref;
  //     if (splice.getBase() != base.getBase())
  //       return nullref;
  //     if (splice.getLen() != base.getLen())
  //       return nullref;

  //     for (auto [a, b] : base.terms().zip(splice.terms())) {
  //       if (a.getFact() != b.getFact())
  //         return nullref;
  //       if (a.getMax() != b.getMax())
  //         return nullref;
  //     }
  //   }
  // }

  template <typename T = SpliceIRef>
  InstrRef doMergeSpliceInsert(ArrayRef<InstrRef> instrs) {
    auto base = instrs.front().as<T>();
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
    for (unsigned i = 0; i < numTerms; i++) {
      auto reg = regs.emplace_back(regBuild.buildRegister(32));
      build.buildStore(
          reg, ConstantBuilder{ctx.getStore<Constant>()}.undef(32).get());
    }
    RegisterRef inputReg = regBuild.buildRegister(base.getMemoryLen());
    build.buildStore(inputReg, ConstantBuilder{ctx.getStore<Constant>()}
                                   .undef(base.getMemoryLen())
                                   .get());

    RegisterRef valueReg;
    if constexpr (requires { base.val(); }) {
      valueReg = regBuild.buildRegister(base.getLen());
    }

    RegisterRef resultReg = regBuild.buildRegister(resultBits);

    for (auto [front, instr] : Range{instrs}.mark_front()) {
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

    build.setInsertPoint(mod.header_end());
    auto proc = build.buildProcess();
    build.setInsertPoint(proc.block().end());

    auto terms =
        base.terms().transform([&](size_t i, AddressGenTermOperand ref) {
          auto rv = AddressGenTerm{build.buildLoad(regs[i]), ref.getFact(),
                                   ref.getMax()};
          return rv;
        });
    HWValue val;
    SmallVec<AddressGenTerm, 4> termsV{terms};
    auto inpVal = build.buildLoad(inputReg);

    // store mapping from old to new input wire (used as heuristic to only share
    // insert/splice of same mem)
    if (base.in().template is<WireRef>())
      valueRemap.insert(inpVal.as<WireRef>(), base.in().template as<WireRef>());

    if constexpr (requires { base.val(); }) {
      val = build.buildInsert(inpVal, build.buildLoad(valueReg), base.getBase(),
                              Range{termsV});
    } else {
      val = build.buildSplice(inpVal, base.getLen(), base.getBase(),
                              Range{termsV});
    }
    build.setInsertPoint(proc.block().end());

    if (auto defW = val.as<WireRef>())
      for (auto src : instrs)
        ctx.getCtx<CoreDialectContext>().instrSourceLocInfo.copyDebugInfo(
            src, defW.getDefI());

    build.buildStore(resultReg, val);

    DYNO_DBG({
      std::print(dbgs(), "into:\n");
      std::print(dbgs(), " ");
      if (auto asWire = val.dyn_as<WireRef>())
        dumpInstr(asWire.getDefI(), ctx);
      else {
        val.as<ConstantRef>().toStream(dbgs());
        std::print(dbgs(), "\n");
      }
      std::print(dbgs(), "\n");
    })

    return val.as<WireRef>().getDefI();
  }

  PotentialMerge findPotentialMerge(MutArrayRef<PotentialMerge> instrs) {
    bool compat = Range{instrs}.all_equal(
        [&](auto &a, auto &b) { return mergeCompatible(a, b); });
    if (compat)
      return PotentialMerge(instrs);
    return PotentialMerge();
  }

  InstrRef doMerge(ArrayRef<InstrRef> instrs) {
    if (config.opToShare.is(OP_ADD, OP_MUL))
      return doMergeCommOps(instrs);
    if (config.opToShare.is(HW_SPLICE))
      return doMergeSpliceInsert<SpliceIRef>(instrs);
    if (config.opToShare.is(HW_INSERT))
      return doMergeSpliceInsert<InsertIRef>(instrs);
    dyno_unreachable("merging unimplemented");
  }

  auto findMergeCandidates(SmallVecImpl<BlockResult> &results) {

    SmallVec<uint32_t, 4> idxs(results.size());

    SmallVec<uint32_t, 16> mergeCandidates;
    SmallVec<uint32_t, 4> mergeCandidatesStartIdxs;

    SmallVec<PotentialMerge, 4> mergedInstrs;

    if (config.checkAllPairs) {
      // collect per-block candidates with same hash
      SmallDenseMap<uint64_t, SmallVec<SmallVec<PotentialMerge, 4>, 4>> map;
      for (auto [resIdx, result] : Range{results}.enumerate()) {
        for (auto &candidate : result.candidates) {
          auto hash = mergeHash(candidate);
          auto &vec = map.findOrInsert(
                             hash,
                             [&]() -> SmallVec<SmallVec<PotentialMerge, 4>, 4> {
                               return (results.size());
                             })
                          .second.val();
          vec[resIdx].emplace_back(candidate);
        }
      }

      // for each hash, try merging all possible combinations (larger first)
      for (auto [hash, blocks] : map) {
        SmallVec<uint32_t, 4> idxs(results.size());
        auto hasMore = [&](size_t i) -> bool {
          return idxs[i] < blocks[i].size();
        };
        while (true) {
          SmallVec<PotentialMerge, 4> curInstrs;
          for (size_t i = 0; i < results.size(); ++i) {
            if (!hasMore(i))
              continue;
            curInstrs.emplace_back(results[i].candidates[idxs[i]]);
          }
          if (curInstrs.size() >= 2) {
            if (auto mergedInstr = findPotentialMerge(curInstrs)) {
              for (size_t i = 0; i < results.size(); ++i) {
                if (!hasMore(i))
                  continue;
                results[i].candidates.erase(results[i].candidates.begin() +
                                            idxs[i]);
                idxs[i] = 0; // todo: depessimize idx counter
              }
              mergedInstrs.emplace_back(std::move(mergedInstr));
            }
          }

          for (size_t i = 0; i < results.size(); i++) {
            // allow counting 1 OOB to exclude (todo: remap to lowest prio)
            if (idxs[i] < results[i].candidates.size()) {
              idxs[i]++;
              goto cont_outer;
            } else {
              idxs[i] = 0;
            }
          }
          break;
        cont_outer:
        }
      }

    } else {
      auto hasMore = [&](size_t i) -> bool {
        return idxs[i] < results[i].candidates.size();
      };
      // iterate, incrementing smallest one every iter.
      // if tryMerge succeeds, replace all with nullref and increment all.

      // To avoid quadratic runtime we sort instrs by number of operands
      // and bit size first and then only consider merging adjacent instrs.
      while (true) {
        size_t smallestIdx;
        InstrRef smallestInstr = nullref;

        for (size_t i = 0; i < results.size(); ++i) {
          if (!hasMore(i))
            continue;

          auto cur = results[i].candidates[idxs[i]].getCanon();
          if (!smallestInstr || orderInstrs(cur, smallestInstr)) {
            smallestInstr = cur;
            smallestIdx = i;
          }
        }

        SmallVec<PotentialMerge, 4> curInstrs;
        for (size_t i = 0; i < results.size(); ++i) {
          if (!hasMore(i))
            continue;
          curInstrs.emplace_back(results[i].candidates[idxs[i]]);
        }
        if (curInstrs.size() < 2)
          break;

        if (auto mergedInstr = findPotentialMerge(curInstrs)) {
          for (size_t i = 0; i < results.size(); ++i) {
            if (!hasMore(i))
              continue;
            results[i].candidates[idxs[i]] = PotentialMerge();
            ++idxs[i];
          }
          mergedInstrs.emplace_back(mergedInstr);
        } else {
          ++idxs[smallestIdx];
        }
      }
      for (auto &res : results) {
        uint64_t idx = 0;
        for (auto &cand : res.candidates) {
          if (!cand)
            continue;
          res.candidates[idx++] = std::move(cand);
        }
        res.candidates.downsize(idx);
      }
    }

    return mergedInstrs;
  }

  void handleMultiway(BlockResult &curRes, ArrayRef<BlockRef> blocks) {
    SmallVec<BlockResult, 4> results;
    results.reserve(blocks.size());

    for (auto block : blocks) {
      auto &res = results.emplace_back(runOnBlock(block));
      sortBlockResult(res);
    }

    auto mergedInstrs = findMergeCandidates(results);

    for (auto &res : results) {
      curRes.candidates.push_back_range(Range{res.candidates});
    }

    // also make sucessfully merged instrs candidates again.
    curRes.candidates.push_back_range(Range{mergedInstrs});
  }

  BlockResult runOnBlock(BlockRef block) {
    BlockResult res;
    for (auto &instr : block) {
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

  void runOnProcess(ProcessIRef proc) {
    auto result = runOnBlock(proc.block());
    valueRemap.clear();
    for (auto &merge : result.candidates) {
      if (merge.sources.size() <= 1)
        continue;
      DYNO_DBG({
        dbgs() << "shared:\n";
        for (auto &src : merge.sources) {
          dbgs() << "  ";
          dumpInstr(src, ctx, true, false);
        }
      })
      doMerge(merge.sources);
    }
  }

  void runOnModule(ModuleIRef mod) {
    for (auto proc : mod.procs())
      runOnProcess(proc);
  }

public:
  void run() {
    for (auto mod : ctx.getCtx<HWDialectContext>().activeModules()) {
      runOnModule(mod.iref());
    }
  }
  void runModule(ModuleIRef mod) { runOnModule(mod); }
  void runProcess(ProcessIRef proc) { runOnProcess(proc); }
  void runBlock(BlockRef block) { runOnBlock(block); }

  static constexpr auto runFuncs =
      mk_tuple(&EarlySharePass::runModule, &EarlySharePass::runProcess,
               &EarlySharePass::runBlock, &EarlySharePass::run);

  auto make(Context &ctx) { return EarlySharePass(ctx); }
  explicit EarlySharePass(Context &ctx) : ctx(ctx) {}
};

}; // namespace dyno
