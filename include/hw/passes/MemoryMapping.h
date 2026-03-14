#pragma once

#include "dyno/CFG.h"
#include "dyno/Constant.h"
#include "dyno/Context.h"
#include "dyno/MutInstr.h"
#include "dyno/Obj.h"
#include "dyno/Pass.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Module.h"
#include "hw/analysis/RegisterValue.h"
#include "hw/analysis/WireVariable.h"
#include "support/Algorithm.h"
#include "support/Any.h"
#include "support/Bits.h"
#include "support/DenseMap.h"
#include "support/DynBitSet.h"
#include "support/ErrorRecovery.h"
#include "support/Ranges.h"
#include "support/SmallVec.h"
#include <algorithm>
#include <bit>
#include <cstdint>
#include <type_traits>

namespace dyno {

class MemoryMappingPass : public Pass<MemoryMappingPass> {
  Context &ctx;

  struct PortFrag {
    uint32_t dstAddr;
    uint32_t len;
    UnsizedBitSet<SmallVec<uint64_t, 1>> set = {};

    PortFrag(const PortFrag &) = default;
    PortFrag(PortFrag &&) = default;
    PortFrag &operator=(const PortFrag &) = default;
    PortFrag &operator=(PortFrag &&) = default;

    PortFrag(uint32_t dstAddr, uint32_t len) : dstAddr(dstAddr), len(len) {}
    PortFrag(uint32_t dstAddr, uint32_t len, uint32_t idx)
        : dstAddr(dstAddr), len(len) {
      set.setDyn(idx);
    };

    bool overwrites(PortFrag &other) { return true; }
    bool fuses(PortFrag &other) { return false; }
    bool intersects(PortFrag &other) { return false; }

    PortFrag intersect(PortFrag &other) {
      PortFrag frag = *this;
      frag.set |= other.set;
      return frag;
    }

    bool abstractEquals(const PortFrag &) const { return false; }
  };

  class PortPartition : public GenericPartitions<PortFrag, 4> {
  public:
    using GenericPartitions::GenericPartitions;

    bool isCovered(uint32_t addr, uint32_t len) {
      // oob is covered
      if (addr >= getLen())
        return true;
      auto it = getInsertIt(addr);
      if (it->set.count() == 0)
        return false;

      while (it->dstAddr + it->len < addr + len) {
        ++it;
        if (it->set.count() == 0)
          return false;
      }

      return true;
    }
  };

  struct BoundPort {
    ObjRef<Pointer> addr = nullref;
    uint32_t idxOffs = 0;
  };
  struct MemoryMapping {
    // metadata attached to model ports (small vec for duplicated ports)
    SmallDenseMap<ObjRef<Pointer>, SmallVec<BoundPort, 4>> boundPorts;

    // metadata attached to model triggers (small vec for duplicated ports)
    SmallDenseMap<ObjRef<Trigger>, SmallVec<ObjRef<Trigger>, 4>> boundTriggers;

    SmallVec<PortPartition, 2> storePartitions;
    SmallVec<PortPartition, 4> loadPartitions;

    // indexed by repeatIdx
    SmallVec<uint32_t, 16> portSizes;
    uint32_t repeatCount;
    uint32_t tgtAdjModelWidth;
    // todo efficient
    uint32_t getAdjModelWidth() const { return Range{portSizes}.sum(); }
  };
  struct MemoryMapper {

    static constexpr auto filterStore = [](OperandRef op) {
      return op.instr().isOpc(HW_MEM_STORE);
    };
    static constexpr auto filterLoad = [](OperandRef op) {
      return op.instr().isOpc(HW_MEM_LOAD);
    };
    static constexpr auto getInstr = [](size_t, auto op) { return op.instr(); };

    RegisterIRef actual;
    RegisterIRef model;
    uint32_t modelPortWidth, actualStoresWidth, modelStoresWidth,
        actualLoadsWidth, modelLoadsWidth, actualPortsLCM;

    // for convenience, actual/model store port arrays
    SmallVec<MemStoreIRef, 4> actualStores;
    SmallVec<MemStoreIRef, 4> modelStores;
    SmallVec<MemLoadIRef, 4> actualLoads;
    SmallVec<MemLoadIRef, 4> modelLoads;

    MemoryMapping mapping;

    MemoryMapper(RegisterIRef actual, RegisterIRef model)
        : actual(actual), model(model),
          actualStores(
              actual.oref().uses().filter(filterStore).transform(getInstr)),
          modelStores(
              model.oref().uses().filter(filterStore).transform(getInstr)),
          actualLoads(
              actual.oref().uses().filter(filterLoad).transform(getInstr)),
          modelLoads(
              model.oref().uses().filter(filterLoad).transform(getInstr)),
          mapping{{}, {}, (actualStores.size()), (actualLoads.size()), {},
                  0,  0} {
      modelPortWidth = modelStores[0].getLen();
      assert(Range{modelStores}.all(
          [&](auto st) { return st.getLen() == modelPortWidth; }));
      assert(Range{modelLoads}.all(
          [&](auto st) { return st.getLen() == modelPortWidth; }));
      actualStoresWidth =
          Range{actualStores}
              .transform([](size_t, MemStoreIRef st) { return st.getLen(); })
              .sum();
      actualLoadsWidth =
          Range{actualLoads}
              .transform([](size_t, MemLoadIRef ld) { return ld.getLen(); })
              .sum();
      // this is just a product
      modelStoresWidth =
          Range{modelStores}
              .transform([](size_t, MemStoreIRef st) { return st.getLen(); })
              .sum();
      modelLoadsWidth =
          Range{modelLoads}
              .transform([](size_t, MemLoadIRef ld) { return ld.getLen(); })
              .sum();

      auto storesLCM =
          Range{actualStores}
              .transform([](size_t, MemStoreIRef st) { return st.getLen(); })
              .lcm();
      auto loadsLCM =
          Range{actualLoads}
              .transform([](size_t, MemLoadIRef ld) { return ld.getLen(); })
              .lcm();
      actualPortsLCM = std::lcm(storesLCM, loadsLCM);

      // How many times to repeat the model memory. This is a lower bound, other
      // constraints may push this up, if mapping fails we exponential search
      // upwards. Lower bound is set by bandwidth and/or capacity.
      mapping.repeatCount =
          *InitListRange{
              round_up_div(actualStoresWidth, modelStoresWidth),
              round_up_div(actualLoadsWidth, modelLoadsWidth),
              round_up_div(*actual.getNumBits(), *model.getNumBits())}
               .max();
    }

    // if true & newly mapped also returns functor to undo the mapping
    template <typename RefT> struct AutoRollback {
      AutoRollback(const AutoRollback &) = delete;
      AutoRollback(AutoRollback &&o) {
        trigger = std::exchange(o.trigger, nullptr);
      }
      AutoRollback &operator=(const AutoRollback &) = delete;
      AutoRollback &operator=(AutoRollback &&o) {
        if (trigger)
          *trigger = nullref;
        trigger = std::exchange(o.trigger, nullptr);
        return *this;
      }
      AutoRollback() = default;
      AutoRollback(RefT *trigger) : trigger(trigger) {};

      RefT *trigger = nullptr;
      ~AutoRollback() {
        if (trigger)
          *trigger = nullref;
      }
      void commit() { trigger = nullptr; }
    };
    std::pair<bool, AutoRollback<ObjRef<Trigger>>>
    mapTrigger(TriggerRef act, TriggerRef mod, uint32_t repIdx) {
      if (act->size() != mod->size())
        return {false, {}};
      auto &mapped = mapping.boundTriggers
                         .findOrInsert(mod, {mapping.repeatCount, nullref})
                         .second.val()[repIdx];
      if (mapped)
        return {false, {}};
      for (unsigned i = 0; i < act->size(); i++) {
        if (mod->getMode(i) != act->getMode(i))
          return {false, {}};
      }
      mapped = act;
      return {true, {&mapped}};
    }

    template <typename RefT>
    bool mapPorts(SmallVecImpl<PortPartition> &partitions,
                  SmallVecImpl<RefT> &actualPorts,
                  SmallVecImpl<RefT> &modelPorts) {
      DynBitSet<SmallVec<uint64_t, 4>> usedModelPort(modelPorts.size() *
                                                     mapping.repeatCount);

      SmallVec<uint32_t, 32> worklist(IntRange(actualPorts.size()).reverse());

      // Usually the modelWidth is mapping.repeatCount * modelPortWidth. We can
      // slightly reduce it to e.g. implement a 32-bit wide memory with 2x
      // 18-bit model memories. This is done greedily via this function. We can
      // only do it once, if there's multiple conflicting values it just fails.
      auto setAdjModelWidth = [&](uint32_t index, uint32_t width) {
        mapping.portSizes[index] = width;

        // not enough bits -> downsize stripe count
        if (mapping.getAdjModelWidth() < mapping.tgtAdjModelWidth) {
          // todo: fail if can't fulfill bandwidth or depth requirements.
          mapping.tgtAdjModelWidth =
              (mapping.getAdjModelWidth() / actualPortsLCM) * actualPortsLCM;
          // if depth too small, fail
          if ((mapping.tgtAdjModelWidth *
               *modelLoads[0].terms().front().getMax()) < *actual.getNumBits())
            return false;
        }

        // restart from beginning
        worklist.clear();
        worklist.push_back_range(IntRange(actualPorts.size()).reverse());

        mapping.boundPorts.clear();
        mapping.boundTriggers.clear();

        usedModelPort.clearAllBits();

        return true;
      };
    retry:

      while (!worklist.empty()) {
        auto actStIdx = worklist.pop_back_val();
        auto actSt = actualPorts[actStIdx];
        PortPartition &part = partitions[actStIdx];
        auto adjActPortLen = actSt.getLen();

        auto subPortBoundary = adjActPortLen;

        // every port needs access to the full memory array. If a port is too
        // narrow, artifically widen it - we then later add MUXs to reduce it.
        assert(mapping.tgtAdjModelWidth % actSt.getLen() == 0);
        if (adjActPortLen < mapping.tgtAdjModelWidth)
          adjActPortLen = mapping.tgtAdjModelWidth;
        part = PortPartition{adjActPortLen};

        // operates on conceptual duplicated model stores. one
        // actualStore might need to be covered by multiple modelStores
        for (const auto globIdx : usedModelPort.unsetBitIdxs()) {
          uint32_t repIdx = globIdx % mapping.repeatCount;

          if (mapping.portSizes[repIdx] == 0)
            continue;

          uint32_t modStIdx = globIdx / mapping.repeatCount;
          auto &modSt = modelPorts[modStIdx];
          auto adjModPortLen = mapping.portSizes[repIdx];

          // base address of duplicates is shifted.
          // their factor is implicitly multiplied by repIdx.
          const auto baseAddr =
              modSt.base() + Range{mapping.portSizes}.subrange(0, repIdx).sum();
          auto adjAddr = baseAddr;

          // can't make loads faster, skip.
          // TODO: implement store fowarding & do not skip for stores
          if (modSt.port()->delay > modSt.port()->delay)
            continue;

          // sub instance stores may not cross the striping boundaries
          if constexpr (std::is_same_v<RefT, MemStoreIRef>) {
            // todo: relax this rule for known-fused stores (e.g. capacity
            // increasing repeats)
            auto lowIdx = baseAddr / subPortBoundary;
            auto highIdx = (baseAddr + adjModPortLen - 1) / subPortBoundary;
            auto overflBits =
                (baseAddr + adjModPortLen - 1) - (lowIdx * subPortBoundary);

            bool isLast = (adjActPortLen / subPortBoundary) == highIdx;

            if (lowIdx != highIdx) {
              assert(overflBits);
              //  if we set adjModelWidth we need to restart from beginning.
              //  this is not real recursion, can only happen once.
              if (!setAdjModelWidth(repIdx, adjModPortLen - overflBits))
                return false;
              goto retry;
            }
          }

          // constant stuff - we can probably just define no const addr for
          // model mems
          assert(!modSt.addr().template is<ConstantRef>() &&
                 !actSt.addr().template is<ConstantRef>());

          // If already covered attempt to move forward and cover more via idx
          // offset. (in general not good, wastes alignment)
          bool cov;
          while ((cov = part.isCovered(adjAddr, adjModPortLen)) &&
                 (adjAddr - baseAddr) < adjModPortLen) {
            // fixme: correct shift
            if (modSt.terms().front().getFact() != adjModPortLen)
              break;
            adjAddr += adjModPortLen * mapping.repeatCount;
          }
          // can't cover anything new, port is wasted (also not good)
          if (cov)
            continue;

          auto modPtr = modSt.addr().template as<ObjRef<Pointer>>();
          auto actPtr = actSt.addr().template as<ObjRef<Pointer>>();

          AutoRollback<ObjRef<Trigger>> triggerMapping;
          if (TriggerRef actTrig = actSt.trigger()) {
            TriggerRef modTrig = modSt.trigger();
            assert(modTrig && "expected trigger on model port");
            auto pair = mapTrigger(actTrig, modTrig, repIdx);
            if (!pair.first)
              continue;
            triggerMapping = std::move(pair.second);
          }

          auto &modelPortMeta =
              mapping.boundPorts
                  .findOrInsert(modPtr,
                                SmallVec<BoundPort, 4>(mapping.repeatCount))
                  .second.val()[repIdx];
          // address object is already bound to something else.
          if (modelPortMeta.addr != Any{actPtr, nullref}) {
            continue;
          }
          modelPortMeta.addr = actPtr;

          modelPortMeta.idxOffs =
              (adjAddr - baseAddr) / modSt.terms().front().getFact();

          // commit mapping now that we can't fail anymore
          triggerMapping.commit();
          usedModelPort[globIdx] = 1;
          part.writeSingle(adjAddr,
                           std::min(part.getLen() - adjAddr, adjModPortLen),
                           unsigned(globIdx));
        }

        if (!part.isCovered(0, part.getLen()))
          return false;
      }
      return true;
    }

    template <typename RefT>
    void applyPort(Context &ctx, RefT actLoad, PortPartition &part,
                   uint32_t factor, auto &&connect, auto &&connectReverse,
                   OtherVec<HWValue> *outWires, HWValue &outSubIdx) {
      HWInstrBuilder build{ctx, actLoad};

      HWValue subIdx = nullref;
      // todo: more than one term
      auto addr = actLoad.terms().front().getIdx();
      if (factor != 1) {
        subIdx = build.buildUMod(actLoad.terms().front().getIdx(),
                                 ConstantRef::fromU32(factor));
        addr = build.buildUDiv(addr, ConstantRef::fromU32(factor));
      }

      for (auto &frag : part.frags) {
        auto idx = frag.set.ctz();
        auto repIdx = idx % mapping.repeatCount;
        RefT modLoad;
        if constexpr (std::is_same_v<RefT, MemLoadIRef>)
          modLoad = modelLoads[idx / mapping.repeatCount];
        else
          modLoad = modelStores[idx / mapping.repeatCount];
        auto &binding =
            mapping
                .boundPorts[modLoad.addr().template as<PointerRef>()][repIdx];

        // find port
        if (modLoad.en()) {
          HWValue en = ConstantRef::fromBool(true);
          if (actLoad.en())
            en = actLoad.en();
          assert(en.getNumBits() == 1);
          if (subIdx) {
            auto idx = frag.dstAddr / actLoad.getLen();
            assert(idx == ((frag.dstAddr + frag.len - 1) / actLoad.getLen()) &&
                   "frag overlaps multiple boundary?");
            en = build.buildAnd(en, build.buildICmp(subIdx,
                                                    ConstantRef::fromU32(idx),
                                                    BigInt::ICMP_EQ));
          }

          connect(en, modLoad.en(), repIdx);
        }

        // shift in the address (could also do non pow2 but likely too slow)
        // int32_t addrShamt = -std::bit_width(factor - 1);
        // uint32_t len = part.getLen(); // actLoad.getLen(); // TODO mismatch

        // // length of the model port with repeats. this assumes repeats do the
        // // same thing which might be broken.
        // uint32_t repLen = mapping.adjModelWidth;//(mapping.repeatCount *
        // modelPortWidth); if (len < repLen)
        //   ; // too long, simply truncate, wasting capacity. we can add MUXs
        //     // here at some point
        // else if (len > repLen) {
        //   addrShamt += flog2(len / repLen);
        //   // TODO: case where we hit this branch but also shrink port size
        //   // back down bc of adjSize
        // }

        // if (addrShamt >= 0)
        //   addr = build.buildSLL(addr, ConstantRef::fromU32(addrShamt));
        // else
        //   addr = build.buildSRL(addr, ConstantRef::fromU32(-addrShamt));
        addr = build.buildAdd(addr, ConstantRef::fromU32(binding.idxOffs));
        connect(addr, modLoad.terms().front().getIdx().template as<WireRef>(),
                repIdx);

        if constexpr (std::is_same_v<RefT, MemLoadIRef>) {
          HWValue rdval = connectReverse(modLoad.value(), repIdx);
          rdval = build.buildTrunc(frag.len, rdval);
          (*outWires).emplace_back(rdval);
        } else {
          HWValue wrval = actLoad.value();
          wrval = build.buildRepeat(wrval, factor);
          wrval = build.buildSplice(wrval, frag.len, frag.dstAddr);
          connect(wrval, modLoad.value(), repIdx);
        }
      }

      outSubIdx = subIdx;
    }

    void apply(Context &ctx) {
      HWInstrBuilder build{ctx};
      auto cell = HWInstrRef{model}.parentMod(ctx);
      assert(cell.isOpc(HW_STDCELL_DEF));

      uint32_t numPorts = 0, numOutputs = 0;
      for (auto p : cell.ports()) {
        numOutputs += p.isOpc(HW_OUTPUT_REGISTER_DEF);
        numPorts++;
      }
      uint32_t numInputs = numPorts - numOutputs;

      SmallVec<MutInstr<FatDynObjRef<>>, 16> instances;
      instances.reserve(mapping.repeatCount);
      for (unsigned i = 0; i < mapping.repeatCount; i++) {
        auto &instance = instances.emplace_back(ctx, HW_STDCELL_INSTANCE,
                                                numOutputs, 1 + numInputs);
        std::fill(instance.begin(), instance.end(), nullref);
        instance.other(0) = cell.mod();
      }

      SmallVec<ObjRef<Register>, 8> cellInputs(
          cell.ports()
              .filter([](auto p) { return p.isOpc(HW_INPUT_REGISTER_DEF); })
              .transform([](size_t, auto ir) { return ir.oref(); }));
      SmallVec<ObjRef<Register>, 4> cellOutputs(
          cell.ports()
              .filter([](auto p) { return p.isOpc(HW_OUTPUT_REGISTER_DEF); })
              .transform([](size_t, auto ir) { return ir.oref(); }));

      // Port reg -> I/O index map.
      // We should maybe cache this in module.
      auto makeMapPair = [](size_t i, auto elem) {
        return std::make_pair(elem, uint32_t(i));
      };
      SmallDenseMap<ObjRef<Register>, uint32_t> inputIdxMap(
          Range(cellInputs).transform(makeMapPair));
      SmallDenseMap<ObjRef<Register>, uint32_t> outputIdxMap(
          Range(cellOutputs).transform(makeMapPair));

      // connect act to mod in instance #repIdx
      auto connect = [&](HWValue act, WireRef mod, unsigned repIdx) {
        auto port = WireVariable::checkIsInputLookthru(mod);
        if (!port)
          report_fatal_error("expected port");
        auto idx = inputIdxMap.find(port.oref()).val();
        act = build.buildResize(act, *port.getNumBits());
        instances[repIdx].others().drop_front()[idx] = act;
      };
      auto connectReverse = [&](WireRef mod, unsigned repIdx) -> WireRef {
        auto port = WireVariable::checkIsPort(mod, HW_OUTPUT_REGISTER_DEF);
        if (!port)
          report_fatal_error("expected port");
        auto idx = outputIdxMap.find(port.oref()).val();
        auto w = ctx.getStore<Wire>().create(*port.getNumBits());
        instances[repIdx].defs()[idx] = w;
        return w;
      };

      for (auto [part, actLoad] :
           Range{mapping.loadPartitions}.zip(actualLoads)) {
        build.setInsertPoint(actLoad);

        OtherVec<HWValue> wires{ctx, 1, part.frags.size()};
        // todo: this needs to be error not assert
        // assert(part.getLen() % actLoad.getLen() == 0);
        uint32_t factor = part.getLen() / actLoad.getLen();
        // todo: lower score when factor is non pow2
        // assert(std::popcount(factor) == 1 && "expected pow2 factor");

        HWValue subIdx;
        applyPort(ctx, actLoad, part, factor, connect, connectReverse, &wires,
                  subIdx);

        wires.others().do_reverse();
        auto val = build.buildConcat(std::move(wires));

        if (factor != 1) {
          val = build.buildSplice(
              val, actLoad.getLen(), 0,
              AddressGenTerm{subIdx, actLoad.getLen(), factor});
        }

        actLoad.def()->as<WireRef>().replaceAllUsesWith(val);
      }

      for (auto [part, actStore] :
           Range{mapping.storePartitions}.zip(actualStores)) {
        build.setInsertPoint(actStore);

        assert(part.getLen() % actStore.getLen() == 0);
        uint32_t factor = part.getLen() / actStore.getLen();
        assert(std::popcount(factor) == 1 && "expected pow2 factor");

        HWValue subIdx;
        applyPort(ctx, actStore, part, factor, connect, connectReverse, nullptr,
                  subIdx);
      }

      for (auto [modTrigO, actTrigs] : mapping.boundTriggers) {
        auto modTrig = ctx.resolve(modTrigO);
        for (auto [repIdx, actTrigO] : Range{actTrigs}.enumerate().filter(
                 [](auto p) { return !!p.second; })) {
          auto instanceInputs = instances[repIdx].others().drop_front();
          auto actTrig = ctx.resolve(actTrigO);

          for (auto [actReg, modReg] :
               actTrig.iref().others().as<RegisterRef>().zip(
                   modTrig.iref().others().as<RegisterRef>())) {
            assert(modReg.iref().isOpc(HW_INPUT_REGISTER_DEF) &&
                   "expected input");
            auto modInputIdx = inputIdxMap.find(modReg).val();
            instanceInputs[modInputIdx] = build.buildLoad(actReg);
          }
        }
      }

      // build
      BlockRef block = actualLoads.empty()
                           ? actualStores.front().parentProc(ctx).block()
                           : actualLoads.front().parentProc(ctx).block();
      for (auto &inst : instances) {

        for (auto [i, def] : inst.defs().enumerate()) {
          if (def)
            continue;
          def = ctx.getStore<Wire>().create(
              *ctx.resolve(cellOutputs[i]).getNumBits());
        }
        for (auto [i, use] : inst.others().drop_front().enumerate()) {
          if (use)
            continue;
          use = ConstantBuilder{ctx.getStore<Constant>()}
                    .undef(*ctx.resolve(cellInputs[i]).getNumBits())
                    .get();
        }

        auto instr = inst.build();
        block.end().insertPrev(instr);
      }
    }
  };

  // Size mismatch
  /*
    - Increase size of memory ports
      OR
    - Decrease size of std cells.
      ^ doesn't work, we only want to shrink last cell.

    - artifically make memory ports a multiple of the modelPortSize.
      -> already doign this but there is no information broadcast which bits are
    cut off.

  */

  bool tryMap(MemoryMapper &mapper, uint32_t repeatCount) {
    mapper.mapping.repeatCount = repeatCount;
    mapper.mapping.boundPorts.clear();
    mapper.mapping.boundTriggers.clear();

    // ports can't be smaller than this, otherwise they wouldn't cover all
    // repeats/stripes and wouldn't be able to access all bits. We increase
    // their size here, apply later detects it and MUXes/DEMUXes back down.
    uint32_t canonicalModelWidth =
        mapper.mapping.repeatCount * mapper.modelPortWidth;

    uint32_t adjModelWidth =
        (canonicalModelWidth / mapper.actualPortsLCM) * mapper.actualPortsLCM;

    mapper.mapping.tgtAdjModelWidth = adjModelWidth;

    uint32_t unusedBits = canonicalModelWidth - adjModelWidth;

    mapper.mapping.portSizes = SmallVec<uint32_t, 16>(
        mapper.mapping.repeatCount, mapper.modelPortWidth);

    if (!mapper.mapPorts(mapper.mapping.storePartitions, mapper.actualStores,
                         mapper.modelStores))
      return false;

    if (!mapper.mapPorts(mapper.mapping.loadPartitions, mapper.actualLoads,
                         mapper.modelLoads))
      return false;

    return true;
  }

  bool lowerMemory(RegisterIRef actual, RegisterIRef model) {
    MemoryMapper mapper{actual, model};
    auto origRepCnt = mapper.mapping.repeatCount;
    auto maxRepCnt = origRepCnt * 4;

    // We start with a lower bound of the number of repeats required.
    // Bounded exponential search to find the lowest number of repeats to
    // make it work (or fail).
    IntRange universe(0u, maxRepCnt + 1);
    auto searchSpace = universe.subrange(origRepCnt);
    auto it = exp_search(universe, searchSpace,
                         [&](auto cnt) { return tryMap(mapper, cnt); });

    if (it == searchSpace.end())
      return false;

    // best value may not have been the last one we tried
    if (mapper.mapping.repeatCount != *it) {
      auto rv = tryMap(mapper, *it);
      assert(rv);
    }

    mapper.apply(ctx);
    return true;
  }
  SmallVec<RegisterIRef, 16> memStdCells;

public:
  void runOnModule(ModuleIRef mod) {
    if (mod.isOpc(HW_STDCELL_DEF))
      return;
    for (auto reg : mod.regs()) {
      auto uses = reg.oref().uses();
      if (uses.empty())
        continue;
      if (!uses.front().instr().isOpc(HW_MEM_LOAD, HW_MEM_STORE))
        continue;
      assert(memStdCells.size() == 1);
      lowerMemory(reg, memStdCells[0]);
    }
  }

  void findMemStdCells() {
    memStdCells.clear();
    for (auto mod : ctx.getStore<Module>()) {
      if (!mod.iref().isOpc(HW_STDCELL_DEF))
        continue;

      auto internal = mod.iref().internal_regs();
      if (internal.empty())
        continue;

      auto max = *internal.max([](RegisterIRef a, RegisterIRef b) {
        return *a.getNumBits() < *b.getNumBits();
      });

      if (max.oref().getNumUses() == 0)
        continue;
      if (!max.oref().uses().front().instr().isOpc(HW_MEM_LOAD, HW_MEM_STORE))
        continue;

      memStdCells.emplace_back(max);
    }
  }

  void runWrapper(auto &&runFunc) {
    findMemStdCells();
    runFunc();
  }

  void runModule(ModuleIRef mod) {
    runWrapper([&] { runOnModule(mod); });
  }

  void run() {
    runWrapper([&] {
      for (auto mod : ctx.getCtx<HWDialectContext>().activeModules())
        runOnModule(mod.iref());
    });
  }

  static constexpr auto runFuncs =
      mk_tuple(&MemoryMappingPass::run, &MemoryMappingPass::runModule);

  explicit MemoryMappingPass(Context &ctx) : ctx(ctx) {}
  auto make(Context &ctx) { return MemoryMappingPass{ctx}; }
};
}; // namespace dyno

/*
You can:
  - reduce write delay (make observable on all reads)
  - add read ports (duplicate)
  - increase width (duplicate)
  - increase depth (duplicate, split address, guard enable)
  - add write enable where there is none (loopback read)


You can't
  - reduce read delay
  - add write ports
  - make port clocks different if not
*/
