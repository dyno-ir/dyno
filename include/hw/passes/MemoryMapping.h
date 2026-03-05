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
#include "support/Any.h"
#include "support/Bits.h"
#include "support/DenseMap.h"
#include "support/DynBitSet.h"
#include "support/ErrorRecovery.h"
#include "support/ResultUnwrap.h"
#include "support/SmallVec.h"
#include <algorithm>
#include <bit>
#include <cstdint>

namespace dyno {

class MemoryMappingPass : public Pass<MemoryMappingPass> {
  Context &ctx;

  struct StoreLowering {
    uint32_t widthRepeats;
    uint32_t depthRepeats;
  };
  StoreLowering lowerStore(MemStoreIRef actual, MemStoreIRef model) {
    StoreLowering l;
    l.widthRepeats = round_up_div(actual.getLen(), model.getLen());

    // todo score w/ factor
  }

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
    // int32_t idxMult = 0;
  };
  struct MemoryMapping {
    // metadata attached to model ports (small vec for duplicated ports)
    SmallDenseMap<ObjRef<Pointer>, SmallVec<BoundPort, 4>> boundPorts;
    SmallVec<PortPartition, 2> storePartitions;
    SmallVec<PortPartition, 4> loadPartitions;
    uint32_t repeatCount;
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
          mapping{{}, (actualStores.size()), (actualLoads.size()), 0} {
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

      // this is repeats to get the required bandwidth - may be limited by loads
      // or stores.
      mapping.repeatCount =
          std::max(round_up_div(actualStoresWidth, modelStoresWidth),
                   round_up_div(actualLoadsWidth, modelLoadsWidth));
    }

    template <typename RefT>
    bool mapPorts(SmallVecImpl<PortPartition> &partitions,
                  SmallVecImpl<RefT> &actualPorts,
                  SmallVecImpl<RefT> &modelPorts, uint32_t minPortSize) {
      UnsizedBitSet<SmallVec<uint64_t, 4>> usedModelPort;
      usedModelPort.resizeBits(actualPorts.size() * mapping.repeatCount);

      for (auto [actStIdx, actSt] : Range{actualPorts}.enumerate()) {
        PortPartition &part = partitions[actStIdx];
        auto adjPortLen = actSt.getLen();
        // every port needs access to the full memory array. If a port is too
        // narrow, artifically widen it - we then later add MUXs to reduce it.
        if (adjPortLen < minPortSize)
          adjPortLen = minPortSize;
        part = PortPartition{adjPortLen};

        // operate on conceptual duplicated model stores. one
        // actualStore might need to be covered by multiple modelStores
        for (auto [modStIdx, modSt] : Range{modelPorts}.enumerate()) {
          for (unsigned repIdx = 0; repIdx < mapping.repeatCount; repIdx++) {

            // base address of duplicates is shifted.
            // their factor is implicitly multiplied by repIdx.
            const auto baseAddr = modSt.base() + modelPortWidth * repIdx;
            auto adjAddr = baseAddr;
            unsigned globIdx = modStIdx * mapping.repeatCount + repIdx;

            // store's already been used (we do very inefficient matching)
            if (usedModelPort[globIdx])
              continue;
            // constant stuff - we can probably just define no const addr for
            // model mems
            assert(!modSt.addr().template is<ConstantRef>() &&
                   !actSt.addr().template is<ConstantRef>());

            // If already covered attempt to move forward and cover more via idx
            // offset. (in general not good, wastes alignment)
            bool cov;
            while ((cov = part.isCovered(adjAddr, modSt.getLen())) &&
                   (adjAddr - baseAddr) < modelPortWidth) {
              assert(modSt.terms().front().getFact() == modSt.getLen());
              adjAddr += modSt.getLen() * mapping.repeatCount;
            }
            // can't cover anything new, port is wasted (also not good)
            if (cov)
              continue;

            auto modPtr = modSt.addr().template as<ObjRef<Pointer>>();
            auto actPtr = actSt.addr().template as<ObjRef<Pointer>>();

            auto &p =
                mapping.boundPorts
                    .findOrInsert(modPtr,
                                  SmallVec<BoundPort, 4>(mapping.repeatCount))
                    .second.val();
            // address object is already bound to something else.
            if (p[modStIdx].addr != Any{actPtr, nullref})
              continue;
            p[modStIdx].addr = actPtr;

            p[modStIdx].idxOffs =
                (adjAddr - baseAddr) / modSt.terms().front().getFact();
            // p[modStIdx].idxMult =
            //     actSt.getLen() / (mapping.repeatCount * modelPortWidth);

            part.writeSingle(adjAddr,
                             std::min(part.getLen() - adjAddr, modSt.getLen()),
                             unsigned(globIdx));
          }
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
      if (factor != 1)
        subIdx = build.buildUMod(actLoad.terms().front().getIdx(),
                                 ConstantRef::fromU32(factor));

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
          HWValue en = ConstantRef::fromU32(1);
          if (actLoad.en())
            en = actLoad.en();
          if (subIdx) {
            auto idx = frag.dstAddr / actLoad.getLen();
            assert(idx == ((frag.dstAddr + frag.len - 1) / actLoad.getLen()) &&
                   "frag overlaps multiple boundary?"); // this might be
                                                        // unavoidable...
            en = build.buildAnd(en, build.buildICmp(subIdx,
                                                    ConstantRef::fromU32(idx),
                                                    BigInt::ICMP_EQ));
          }

          connect(en, modLoad.en(), repIdx);
        }

        // shift in the address (could also do non pow2 but likely too slow)
        int32_t addrShamt = -std::bit_width(factor - 1);
        uint32_t len = part.getLen(); // actLoad.getLen(); // TODO mismatch

        // length of the model port with repeats. this assumes repeats do the
        // same thing which might be broken.
        uint32_t repLen = (mapping.repeatCount * modelPortWidth);
        if (len < repLen)
          ; // too long, simply truncate, wasting capacity. we can add MUXs
            // here at some point
        else if (len > repLen) {
          addrShamt += flog2(len / repLen);
          // TODO: case where we hit this branch but also shrink port size
          // back down bc of adjSize
        }

        // todo: more than one term
        auto addr = actLoad.terms().front().getIdx();
        if (addrShamt >= 0)
          addr = build.buildSLL(addr, ConstantRef::fromU32(addrShamt));
        else
          addr = build.buildSRL(addr, ConstantRef::fromU32(-addrShamt));
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
        std::fill(instance.begin(), instance.end(), ConstantRef::zeroBitZero());
        instance.other(0) = cell.mod();
      }

      // connect act to mod in instance #repIdx
      auto connect = [&](HWValue act, WireRef mod, unsigned repIdx) {
        auto port = WireVariable::checkIsInputLookthru(mod);
        if (!port)
          report_fatal_error("expected port");
        // todo: O(1)
        auto idx =
            *cell.ports()
                 .filter([](auto p) { return p.isOpc(HW_INPUT_REGISTER_DEF); })
                 .find_idx(port);

        act = build.buildResize(act, *port.getNumBits());
        instances[repIdx].others().drop_front()[idx] = act;
      };
      auto connectReverse = [&](WireRef mod, unsigned repIdx) -> WireRef {
        auto port = WireVariable::checkIsPort(mod, HW_OUTPUT_REGISTER_DEF);
        if (!port)
          report_fatal_error("expected port");
        // todo: O(1)
        auto idx =
            *cell.ports()
                 .filter([](auto p) { return p.isOpc(HW_OUTPUT_REGISTER_DEF); })
                 .find_idx(port);
        auto w = ctx.getStore<Wire>().create(*port.getNumBits());
        instances[repIdx].defs()[idx] = w;
        return w;
      };

      for (auto [part, actLoad] :
           Range{mapping.loadPartitions}.zip(actualLoads)) {
        build.setInsertPoint(actLoad);

        OtherVec<HWValue> wires{ctx, 1, part.frags.size()};
        assert(part.getLen() % actLoad.getLen() == 0);
        uint32_t factor = part.getLen() / actLoad.getLen();
        assert(std::popcount(factor) == 1 && "expected pow2 factor");

        HWValue subIdx;
        applyPort(ctx, actLoad, part, factor, connect, connectReverse, &wires,
                  subIdx);

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

      // build
      BlockRef block = actualLoads.empty()
                           ? actualStores.front().parentProc(ctx).block()
                           : actualLoads.front().parentProc(ctx).block();
      for (auto &inst : instances) {
        auto instr = inst.build();
        block.end().insertPrev(instr);
      }
    }
  };
  // 1. Find largest read/write access. This is how big the mem needs to be,
  // sets the widthRepeats
  // 2. Loads at this point trivial to map (some opt can be done if only loading
  // a subrange but is ok for now)
  // 3. For stores, greedily cover the whole access range by picking one store
  // and mapping it to a model store
  //    (respect address equality constraints while doing so.)

  bool lowerMemory(RegisterIRef actual, RegisterIRef model) {
    auto actualSize = actual.getNumBits();
    auto modelSize = model.getNumBits();

    MemoryMapper mapper{actual, model};
    auto origRepCnt = mapper.mapping.repeatCount;
    auto maxRepCnt = origRepCnt * 2;
    do {
      if (!mapper.mapPorts(mapper.mapping.storePartitions, mapper.actualStores,
                           mapper.modelStores, mapper.actualPortsLCM))
        continue;
      if (!mapper.mapPorts(mapper.mapping.loadPartitions, mapper.actualLoads,
                           mapper.modelLoads, mapper.actualPortsLCM))
        continue;
      goto success;
    } while (mapper.mapping.boundPorts.clear(),
             ++mapper.mapping.repeatCount <= maxRepCnt);
    return false;
  success:
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

      auto max =
          *mod.iref().internal_regs().max([](RegisterIRef a, RegisterIRef b) {
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
      std::make_tuple(&MemoryMappingPass::run, &MemoryMappingPass::runModule);

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
