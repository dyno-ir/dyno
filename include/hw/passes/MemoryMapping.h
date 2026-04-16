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
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Module.h"
#include "hw/analysis/PipelineAnalysis.h"
#include "hw/analysis/RegisterValue.h"
#include "hw/analysis/WireVariable.h"
#include "support/Algorithm.h"
#include "support/Any.h"
#include "support/ArrayRef.h"
#include "support/Bits.h"
#include "support/Debug.h"
#include "support/DenseMap.h"
#include "support/DynBitSet.h"
#include "support/ErrorRecovery.h"
#include "support/Optional.h"
#include "support/RTTI.h"
#include "support/Ranges.h"
#include "support/SmallVec.h"
#include "support/StringRef.h"
#include <algorithm>
#include <bit>
#include <cmath>
#include <cstdint>
#include <ostream>
#include <print>
#include <string>
#include <type_traits>

namespace dyno {

class MemoryMappingPass : public Pass<MemoryMappingPass> {
  Context &ctx;

public:
#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM)                                      \
  /* experimental, combine ports to make wider ports, e.g. 2r memory -> 1 wide \
   * read. mainly useful for wide ROMs or asymmetric r/w width, otherwise      \
   * limited by other port */                                                  \
  FIELD(bool, virtualWidePorts, false)                                         \
  FIELD(uint32_t, maxRepeatCountFactor, 8)
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
  Config config;

private:
  struct PortFrag {
    uint32_t dstAddr;
    uint32_t len;
    Optional<uint32_t> mapping = nullopt;

    PortFrag(const PortFrag &) = default;
    PortFrag(PortFrag &&) = default;
    PortFrag &operator=(const PortFrag &) = default;
    PortFrag &operator=(PortFrag &&) = default;

    PortFrag(uint32_t dstAddr, uint32_t len) : dstAddr(dstAddr), len(len) {}
    PortFrag(uint32_t dstAddr, uint32_t len, uint32_t idx)
        : dstAddr(dstAddr), len(len), mapping(idx) {};

    bool overwrites(PortFrag &other) { return true; }
    bool fuses(PortFrag &other) { return false; }
    bool intersects(PortFrag &other) { return false; }

    PortFrag intersect(PortFrag &other) {
      PortFrag frag = *this;
      mapping = other.mapping;
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
      if (!it->mapping)
        return false;

      while (it->dstAddr + it->len < addr + len) {
        ++it;
        if (!it->mapping)
          return false;
      }

      return true;
    }

    uint32_t getNumCoveredBits(uint32_t addr, uint32_t len) {
      uint32_t numBits = 0;
      forActiveRegions(addr, len,
                       [&](uint32_t, uint32_t actLen) { numBits += actLen; });
      return numBits;
    }

    // calls callback(addr, len) for all active regions in the larger addr+:len
    // range
    void forActiveRegions(uint32_t addr, uint32_t len, auto &&callback) {
      if (addr >= getLen() || len == 0)
        return;
      auto it = getInsertIt(addr);
      do {
        auto overlap = it->len - (addr - it->dstAddr);
        if (it->mapping) {
          callback(addr, std::min(overlap, len));
        }
        if (overlap >= len)
          break;
        addr += overlap;
        len -= overlap;

        ++it;
      } while (1);
    }
  };

  template <typename RefT> static Optional<uint32_t> getMinFact(RefT ref) {
    auto facts = ref.terms().transform(
        [](size_t, AddressGenTermOperand op) { return op.getFact(); });
    auto min = *facts.min();
    if (!facts.all([&](uint32_t fact) { return fact % min == 0; }))
      return nullopt;
    return min;
  }

  struct BoundPort {
    ObjRef<Pointer> addr = nullref;
    uint32_t idxOffs = 0;
  };
  struct MemoryMapping {
    // metadata attached to model ports (small vec forrepeatCount duplicated
    // ports)
    SmallDenseMap<ObjRef<Pointer>, SmallVec<BoundPort, 4>> boundPorts;

    // metadata attached to model triggers (small vec for repeatCount duplicated
    // ports)
    SmallDenseMap<ObjRef<Trigger>, SmallVec<ObjRef<Trigger>, 4>> boundTriggers;

    // maps actual load/store ports to model load/store ports
    SmallVec<PortPartition, 2> storePartitions;
    SmallVec<PortPartition, 4> loadPartitions;

    // mask on wide model load/store, to disable bits for smaller memories
    PortPartition modelRangeEnable;

    uint32_t repeatCount;
    uint32_t tgtAdjModelWidth;

    uint64_t cost;

    // todo efficient
    uint32_t getAdjModelWidth() const {
      uint32_t sum = 0;
      for (auto &frag : modelRangeEnable.frags)
        if (frag.mapping)
          sum += frag.len;
      return sum;
    }
  };
  struct MemoryMapper {

    static constexpr auto filterStore = [](OperandRef op) {
      return op.instr().isOpc(HW_MEM_STORE);
    };
    static constexpr auto filterLoad = [](OperandRef op) {
      return op.instr().isOpc(HW_MEM_LOAD);
    };
    static constexpr auto getInstr = [](size_t, auto op) { return op.instr(); };
    Config &config;
    RegisterIRef actual;
    RegisterIRef model;
    uint32_t modelPortWidth, actualStoresWidth, modelStoresWidth,
        actualLoadsWidth, modelLoadsWidth, actualPortsLCM;

    // for convenience, actual/model store port arrays
    // actual: RTL memory we want to map
    // model: primitive memory, conceptually repeated mapping.repeatCount times
    SmallVec<MemStoreIRef, 4> actualStores;
    SmallVec<MemStoreIRef, 4> modelStores;
    SmallVec<MemLoadIRef, 4> actualLoads;
    SmallVec<MemLoadIRef, 4> modelLoads;

    MemoryMapping mapping;

    MemoryMapper(Config &config, RegisterIRef actual, RegisterIRef model)
        : config(config), actual(actual), model(model),
          actualStores(
              actual.oref().uses().filter(filterStore).transform(getInstr)),
          modelStores(
              model.oref().uses().filter(filterStore).transform(getInstr)),
          actualLoads(
              actual.oref().uses().filter(filterLoad).transform(getInstr)),
          modelLoads(
              model.oref().uses().filter(filterLoad).transform(getInstr)),
          mapping{{}, {}, (actualStores.size()), (actualLoads.size()), {}, 0,
                  0,  0} {
      modelPortWidth = *getMinFact(modelStores[0]);
      assert(Range{modelStores}.all(
          [&](auto st) { return *getMinFact(st) == modelPortWidth; }));
      assert(Range{modelLoads}.all(
          [&](auto ld) { return *getMinFact(ld) == modelPortWidth; }));
      actualStoresWidth =
          Range{actualStores}
              .transform([](size_t, MemStoreIRef st) { return st.getLen(); })
              .sum();
      actualLoadsWidth =
          Range{actualLoads}
              .transform([](size_t, MemLoadIRef ld) { return ld.getLen(); })
              .sum();
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
      // constraints may push this up, if mapping fails we search upwards. Lower
      // bound is set by bandwidth and/or capacity.
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
        return {mapped == act, {}};
      for (unsigned i = 0; i < act->size(); i++) {
        if (mod->getMode(i) != act->getMode(i))
          return {false, {}};
      }
      mapped = act;
      return {true, {&mapped}};
    }

    void visualize(std::ostream &os) {
      uint32_t totalWidth = mapping.modelRangeEnable.getLen();
      uint32_t S_max = modelPortWidth;
      auto getLen = [](size_t, auto p) { return p.getLen(); };
      uint32_t S_min = std::min(*Range{modelStores}.transform(getLen).min(),
                                *Range{modelLoads}.transform(getLen).min());
      auto getNewIdx = [&](uint32_t j) { return j + (j / S_min); };

      auto visualizePorts = [&](StringRef label, auto &partitions,
                                auto &modelPorts) {
        std::print(os, "{}:\n", label);
        for (size_t i = 0; i < modelPorts.size(); ++i) {
          std::print(os, "  Port {}: ", i);
          uint32_t rowLen =
              totalWidth + (totalWidth > 0 ? (totalWidth - 1) / S_min : 0);
          std::string row(rowLen, ' ');
          uint32_t portWidth = modelPorts[i].getLen();
          for (uint32_t j = S_min; j < totalWidth; j += S_min)
            if (j % portWidth == 0)
              row[getNewIdx(j) - 1] = '|';
          for (uint32_t r = 0; r < mapping.repeatCount; ++r) {
            uint32_t modelBase = r * S_max + modelPorts[i].base();
            uint32_t portLen = modelPorts[i].getLen();
            for (uint32_t j = modelBase;
                 j < modelBase + portLen && j < totalWidth; ++j)
              row[getNewIdx(j)] = '.';
            for (size_t actIdx = 0; actIdx < partitions.size(); ++actIdx) {
              for (auto &frag : partitions[actIdx].frags) {
                if (frag.mapping &&
                    *frag.mapping == r * modelPorts.size() + i) {
                  for (uint32_t j = modelBase;
                       j < modelBase + portLen && j < totalWidth; ++j) {
                    if (mapping.modelRangeEnable.isCovered(j, 1))
                      row[getNewIdx(j)] = '0' + (actIdx % 10);
                  }
                }
              }
            }
          }
          std::print(os, "{}\n", row);
        }
      };
      visualizePorts("Store Mapping", mapping.storePartitions, modelStores);
      visualizePorts("Load Mapping", mapping.loadPartitions, modelLoads);
      std::print(os, "Enable:   ");
      uint32_t rowLen =
          totalWidth + (totalWidth > 0 ? (totalWidth - 1) / S_min : 0);
      std::string enableRow(rowLen, ' ');
      // for (uint32_t j = S_min; j < totalWidth; j += S_min)
      //   enableRow[getNewIdx(j) - 1] = ' ';
      for (auto &frag : mapping.modelRangeEnable.frags) {
        if (frag.mapping) {
          for (uint32_t j = frag.dstAddr; j < frag.dstAddr + frag.len; ++j)
            if (j < totalWidth)
              enableRow[getNewIdx(j)] = '_';
        }
      }
      std::print(os, "{}\n", enableRow);
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
      // 18-bit model memories. This is done greedily via this function.
      auto setAdjModelWidth = [&](uint32_t repIdx, uint32_t offs,
                                  uint32_t numDisable) {
        mapping.modelRangeEnable.writeSingle(repIdx * modelPortWidth + offs,
                                             numDisable);

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

        auto actFact = getMinFact(actSt);
        if (!*actFact)
          return false;
        auto subPortBoundary = actSt.getLen();

        // every port needs access to the full memory array. If a port is too
        // narrow, artifically widen it - we then later add MUXs to reduce it.
        assert(mapping.tgtAdjModelWidth % actSt.getLen() == 0);
        assert(mapping.tgtAdjModelWidth % *actFact == 0);
        if (adjActPortLen < mapping.tgtAdjModelWidth) {
          adjActPortLen = mapping.tgtAdjModelWidth;
        }
        part = PortPartition{adjActPortLen};
        if (*actFact != actSt.getLen()) {
          // if the port doesn't actually access the whole factor size (e.g.
          // byte enable) mask off.
          uint32_t undefModelInst = usedModelPort.size();
          for (uint i = 0; i < (adjActPortLen / *actFact); i++) {
            auto baseIdx = *actFact * i;
            part.writeSingle(baseIdx + 0, actSt.base(), undefModelInst);
            auto idx = actSt.base() + actSt.getLen();
            part.writeSingle(baseIdx + idx, *actFact - idx, undefModelInst);
          }
        }

        // operates on conceptual duplicated model stores. one
        // actualStore might need to be covered by multiple modelStores
        for (const auto globIdx : usedModelPort.unsetBitIdxs()) {
          uint32_t modStIdx = globIdx % modelPorts.size();
          auto &modSt = modelPorts[modStIdx];

          uint32_t repIdx = globIdx / modelPorts.size();
          auto adjModPortLen = mapping.modelRangeEnable.getNumCoveredBits(
              repIdx * modelPortWidth + modSt.base(), modSt.getLen());

          if (adjModPortLen == 0)
            continue;

          // base address of duplicates is shifted.
          // their factor is implicitly multiplied by repIdx.
          const auto baseAddr = mapping.modelRangeEnable.getNumCoveredBits(
              0, repIdx * modelPortWidth + modSt.base());
          auto adjAddr = baseAddr;

          // can't make loads faster, skip.
          // TODO: implement store fowarding & do not skip for stores
          if (modSt.port()->delay > modSt.port()->delay)
            continue;

          // load output pipelining
          if constexpr (std::is_same_v<RefT, MemLoadIRef>) {
            // model dictates required number of stages
            // but provides en/rst/init etc on stages
            auto modPipe = PipelineAnalysis::getOutPipeline(modSt.value());
            auto actPipe = PipelineAnalysis::getOutPipeline(actSt.value());

            if (!PipelineAnalysis::isImplementableWithPipe(actPipe, modPipe))
              return false;
          }

          // sub instance stores may not cross the striping boundaries
          if constexpr (std::is_same_v<RefT, MemStoreIRef>) {
            // todo: relax this rule for known-fused stores (e.g. capacity
            // increasing repeats)
            auto lowIdx = baseAddr / subPortBoundary;
            auto highIdx = (baseAddr + adjModPortLen - 1) / subPortBoundary;
            auto overflBits = (baseAddr + adjModPortLen - 1) -
                              ((lowIdx + 1) * subPortBoundary) + 1;

            if (lowIdx != highIdx) {
              //  if we set adjModelWidth we need to restart from beginning
              if (!setAdjModelWidth(repIdx,
                                    adjModPortLen - overflBits + modSt.base(),
                                    overflBits))
                return false;
              goto retry;
            }
          }

          // constant stuff - we can probably just define no const addr for
          // model mems
          assert(!modSt.addr().template is<ConstantRef>() &&
                 !actSt.addr().template is<ConstantRef>());

          auto fragAccessLen = std::min(part.getLen() - adjAddr, adjModPortLen);
          // If already covered attempt to move forward and cover more via idx
          // offset. (in general not good, wastes alignment)
          // todo: check that this works with byte enable
          bool cov;
          while ((cov = part.isCovered(adjAddr, fragAccessLen)) &&
                 (adjAddr - baseAddr) < adjModPortLen) {
            if (!config.virtualWidePorts)
              break;
            adjAddr += mapping.modelRangeEnable.getNumCoveredBits(
                0, mapping.modelRangeEnable.getLen());
          }
          // can't cover anything new, port is unused for now
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
          part.writeSingle(adjAddr, fragAccessLen, unsigned(globIdx));
          if (part.isCovered(0, part.getLen()))
            break;
        }

        if (!part.isCovered(0, part.getLen()))
          return false;
      }

      if constexpr (std::is_same_v<RefT, MemStoreIRef>) {
        // Mask off unused ports
        for (const auto globIdx : usedModelPort.unsetBitIdxs()) {
          uint32_t modStIdx = globIdx % modelPorts.size();
          auto &modSt = modelPorts[modStIdx];
          uint32_t repIdx = globIdx / modelPorts.size();
          mapping.modelRangeEnable.writeSingle(
              repIdx * modelPortWidth + modSt.base(), modSt.getLen());
        }
      }
      return true;
    }

    void computeMappingCost() {
      // todo: stdcell info area when available
      double bitsCost = *model.getNumBits() * mapping.repeatCount;

      // simply assume bitcell cost scales linearily for extra accesses.
      uint32_t totalPortLen = 0;
      for (auto &ld : modelLoads)
        totalPortLen += ld.getLen();
      for (auto &st : modelStores)
        totalPortLen += st.getLen();

      bitsCost *= totalPortLen;
      bitsCost /= modelPortWidth;

      // add one for each port to avoid ties
      uint64_t portsCost =
          mapping.repeatCount * (modelLoads.size() + modelStores.size());

      // penalty on higher repeat count
      double decoderCost = 10 * mapping.repeatCount *
                           log2(double(*model.getNumBits()) / modelPortWidth);

      mapping.cost = bitsCost + portsCost + decoderCost;
    }

    template <typename RefT>
    void applyPort(Context &ctx, RefT actLoad, PortPartition &part,
                   uint32_t factor, auto &&connect, auto &&connectReverse,
                   OperandVec<HWValue> *outWires, HWValue &outSubIdx) {
      HWInstrBuilder build{ctx, actLoad};

      HWValue subIdx = nullref;
      // todo: more than one term
      assert(actLoad.terms().size() == 1);
      auto addr = actLoad.terms().front().getIdx();
      if (factor != 1) {
        subIdx = build.buildUMod(actLoad.terms().front().getIdx(),
                                 ConstantRef::fromU32(factor));
        addr = build.buildUDiv(addr, ConstantRef::fromU32(factor));
      }

      uint32_t adjFragAddr = 0;
      for (auto &frag : part.frags) {
        auto idx = *frag.mapping;
        uint32_t repIdx;
        RefT modLoad;

        if constexpr (std::is_same_v<RefT, MemLoadIRef>) {
          repIdx = idx / modelLoads.size();
          if (idx % modelLoads.size() == modelLoads.size())
            continue;
          modLoad = modelLoads[idx % modelLoads.size()];
        } else {
          repIdx = idx / modelStores.size();
          if (idx % modelStores.size() == modelStores.size())
            continue;
          modLoad = modelStores[idx % modelStores.size()];
        }

        // marked as unused
        if (repIdx == mapping.repeatCount)
          continue;
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
            auto idx = frag.dstAddr / *getMinFact(actLoad);
            assert(idx ==
                       ((frag.dstAddr + frag.len - 1) / *getMinFact(actLoad)) &&
                   "frag overlaps multiple boundary?");
            en = build.buildAnd(en, build.buildICmp(subIdx,
                                                    ConstantRef::fromU32(idx),
                                                    BigInt::ICMP_EQ));
          }

          connect(en, modLoad.en(), repIdx);
        }

        HWValue adjAddr = addr;
        if (config.virtualWidePorts) {
          // reconstruct shift information: if this actPort is larger than
          // can be implemented with this modPort and its repeat, there must
          // be supplemental ports. shift/offset to our fraction.
          uint32_t validBitsInModPort = 0;
          for (unsigned i = 0; i < mapping.repeatCount; i++)
            validBitsInModPort += mapping.modelRangeEnable.getNumCoveredBits(
                i * modelPortWidth + modLoad.base(), modLoad.getLen());
          assert(actLoad.getLen() % validBitsInModPort == 0);
          uint32_t multFact = actLoad.getLen() / validBitsInModPort;
          adjAddr = build.buildMul(adjAddr, ConstantRef::fromU32(multFact));
          adjAddr =
              build.buildAdd(adjAddr, ConstantRef::fromU32(binding.idxOffs));
        }
        connect(adjAddr,
                modLoad.terms().front().getIdx().template as<WireRef>(),
                repIdx);

        if constexpr (std::is_same_v<RefT, MemLoadIRef>) {
          auto modPipe =
              PipelineAnalysis::getOutPipeline(modLoad.value(), false);
          auto actPipe =
              PipelineAnalysis::getOutPipeline(actLoad.value(), false);

          auto ldValPostPipe = ctx.resolve(modPipe.slice.wire);
          assert(modPipe.slice.addr == 0 &&
                 modPipe.slice.len == ldValPostPipe.getNumBits() && "todo");
          HWValue rdval = connectReverse(ldValPostPipe, repIdx);

          // unpack bits (some may be unused)
          // assumes frag starts at port boundary, maybe add src addr to
          // PortPartition?
          mapping.modelRangeEnable.forActiveRegions(
              repIdx * modelPortWidth + modLoad.base(), modLoad.getLen(),
              [&](uint32_t actAddr, uint32_t actLen) {
                auto val = build.buildSplice(rdval, actLen,
                                             actAddr - repIdx * modelPortWidth);
                assert(!val.is<ConstantRef>() && "oob 'x splice");
                (*outWires).emplace_back(val);
              });
        } else {
          HWValue wrval = actLoad.value();
          wrval = build.buildRepeat(wrval, factor);
          wrval = build.buildSplice(
              wrval, std::min(frag.len, *wrval.getNumBits()), adjFragAddr);
          // todo: remove assert
          assert(!wrval.is<ConstantRef>() ||
                 !wrval.as<ConstantRef>().allBitsUndef());
          connect(wrval, modLoad.value(), repIdx);
        }
        adjFragAddr += frag.len;
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

      SmallVec<RegisterValue, 16> instanceInputs(mapping.repeatCount *
                                                 numInputs);
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

      auto connectEn = [&](HWValue act, unsigned repIdx, LoadIRef ld,
                           WireVariable::EnableSignal sig) {
        auto reg = ld.reg();
        auto idx = inputIdxMap.find(reg).val();
        auto &inp = instanceInputs[repIdx * numInputs + idx];
        if (inp.getLen() == 0)
          inp = RegisterValue{
              ConstantBuilder{ctx.getStore<Constant>()}.zeroLike(reg),
              *reg.getNumBits()};
        auto old = inp.getRange(sig.addr, 1);
        old.defragmentValues(ctx); // normalize constants
        assert(old.frags.size() == 1 && old.frags.front().srcAddr == 0);
        auto oldVal = ctx.resolve(old.frags.front().ref).as<HWValue>();
        auto newV = build.buildCommutative(sig.polarity ? OP_OR : OP_AND,
                                           InitListRange{act, oldVal});
        inp.overwrite(newV, 0, sig.addr, 1);
      };

      // connect act to mod in instance #repIdx
      auto connect = [&](HWValue act, WireRef mod, unsigned repIdx) {
        // use connect enable for 1 bit signals. this is strictly more powerful,
        // so no problem running this on data signals in case those are 1 bit
        // too.
        if (mod.getNumBits() == 1) {
          // handle or/and for single bit signals, there might be something
          // like we && wmask[0]
          auto rv = WireVariable::connectEnSignal(
              {mod, 0, 1}, [&](LoadIRef ld, WireVariable::EnableSignal sig) {
                connectEn(act, repIdx, ld, sig);
              });
          if (!rv)
            report_fatal_error(
                "ill-formed model memory: {}\ncannot resolve 1-bit "
                "inputs, too complex enable logic?",
                HWInstrRef{model}.parentMod(ctx).mod()->name);
          return;
        }

        auto port = WireVariable::checkIsInputLookthru(mod);
        if (!port)
          report_fatal_error("expected port");
        auto idx = inputIdxMap.find(port->reg.oref()).val();
        auto &inp = instanceInputs[repIdx * numInputs + idx];
        if (inp.getLen() == 0)
          inp = RegisterValue{
              ConstantBuilder{ctx.getStore<Constant>()}.zeroLike(port->reg),
              *port->reg.getNumBits()};
        inp.overwrite(act, 0, port->addr,
                      std::min(*act.getNumBits(), port->len));
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
        OperandVec<HWValue> wires{ctx, 1, part.frags.size()};
        wires.pushPlaceholderDefs();

        uint32_t factor = part.getLen() / *getMinFact(actLoad);
        // todo: lower score when factor is non pow2

        HWValue subIdx;
        applyPort(ctx, actLoad, part, factor, connect, connectReverse, &wires,
                  subIdx);

        wires.others().do_reverse();
        auto val = build.buildConcat(std::move(wires));

        auto defWire = actLoad.def()->as<WireRef>();
        auto delay =
            PipelineAnalysis::getOutPipeline(modelLoads.front().value())
                .totalDelay;
        auto pipe = PipelineAnalysis::getOutPipeline(actLoad.value(), false);
        if (delay != 0) {
          defWire = pipe.stages[delay - 1].ff.q();
        }

        if (factor != 1) {
          if (delay != 0) {
            auto clk = pipe.stages.front().ff.clk();
            for (unsigned i = 0; i < delay; i++) {
              subIdx = build.buildFlipFlop(clk, subIdx);
            }
          }

          val = build.buildSplice(
              val, actLoad.getLen(), 0,
              AddressGenTerm{subIdx, actLoad.getLen(), factor});
        }

        defWire.replaceAllUsesWith(val);
      }

      for (auto [part, actStore] :
           Range{mapping.storePartitions}.zip(actualStores)) {
        build.setInsertPoint(actStore);

        uint32_t factor = part.getLen() / *getMinFact(actStore);

        HWValue subIdx;
        applyPort(ctx, actStore, part, factor, connect, connectReverse, nullptr,
                  subIdx);
      }

      // Flatten inputs
      for (auto [instIdx, inst] : Range{instances}.enumerate()) {
        for (auto [inpIdx, inp] : inst.others().drop_front().enumerate()) {
          auto &val = instanceInputs[instIdx * numInputs + inpIdx];
          if (val.getLen() == 0)
            continue;
          inp = val.get(build, false);
        }
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
          // todo: manually iterate through unused ports and set disabled
          // rather than just blanket zero.
          use = ConstantBuilder{ctx.getStore<Constant>()}
                    .zero(*ctx.resolve(cellInputs[i]).getNumBits())
                    .get();
        }

        auto instr = inst.build();
        block.end().insertPrev(instr);
      }
    }
  };

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
    mapper.mapping.modelRangeEnable = PortPartition(canonicalModelWidth, 1u);

    if (!mapper.mapPorts(mapper.mapping.storePartitions, mapper.actualStores,
                         mapper.modelStores))
      return false;

    if (!mapper.mapPorts(mapper.mapping.loadPartitions, mapper.actualLoads,
                         mapper.modelLoads))
      return false;

    mapper.computeMappingCost();
    return true;
  }

  bool lowerMemory(RegisterIRef actual, RegisterIRef model) {
    MemoryMapper mapper{config, actual, model};
    auto origRepCnt = mapper.mapping.repeatCount;
    auto maxRepCnt = origRepCnt * 8;

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

  bool findBestAndMap(RegisterIRef actual) {
    std::optional<std::pair<ObjRef<Register>, MemoryMapping>> best =
        std::nullopt;

    // todo: prune search space based on score
    for (auto modelCand : Range{memStdCells}.resolve(ctx)) {
      MemoryMapper mapper{config, actual, modelCand.iref()};
      auto vis = [&]() {
        DYNO_DBG({
          dumpInstr(actual, ctx);
          std::print(dbgs(), "possible mapping {}x {}, cost = {}\n",
                     mapper.mapping.repeatCount,
                     HWInstrRef{modelCand.iref()}.parentMod(ctx).mod()->name,
                     mapper.mapping.cost);
          mapper.visualize(dbgs());
          std::print(dbgs(), "\n");
        })
      };

      uint32_t repCount = mapper.mapping.repeatCount;
      uint32_t maxRepCount = repCount * config.maxRepeatCountFactor;

      // fast path if heuristic repeat count correct
      if (tryMap(mapper, repCount)) {
        vis();
        if (!best || mapper.mapping.cost < best->second.cost) {
          best.emplace(modelCand, std::move(mapper.mapping));
        }
        continue;
      }
      repCount += 1;

      // binary search
      while (repCount != maxRepCount) {
        uint32_t trial = (repCount + maxRepCount) / 2;
        if (tryMap(mapper, trial)) {
          maxRepCount = trial;
        } else {
          repCount = trial + 1;
        }
      }

      if (mapper.mapping.repeatCount != repCount) {
        auto rv = tryMap(mapper, repCount);
        assert(rv);
      }

      vis();
      if (!best || mapper.mapping.cost < best->second.cost) {
        best.emplace(modelCand, std::move(mapper.mapping));
      }
    }

    if (!best)
      return false;

    MemoryMapper mapper{config, actual, ctx.resolve(best->first).iref()};
    mapper.mapping = std::move(best->second);
    mapper.apply(ctx);

    return true;
  }

  SmallVec<ObjRef<Register>, 16> memStdCells;

public:
  void runOnModule(ModuleIRef mod) {
    if (mod.isOpc(HW_STDCELL_DEF))
      return;
    for (auto reg : mod.regs()) {
      auto uses = reg.oref().uses();
      if (uses.empty())
        continue;
      if (!Range{uses}.all([](auto use) {
            return use.instr().isOpc(HW_MEM_LOAD, HW_MEM_STORE);
          }))
        continue;
      findBestAndMap(reg);
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
      if (!Range{max.oref().uses()}.all([](auto use) {
            return use.instr().isOpc(HW_MEM_LOAD, HW_MEM_STORE);
          }))
        continue;

      memStdCells.emplace_back(max.oref());
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
