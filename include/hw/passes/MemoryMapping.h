#pragma once

#include "dyno/CFG.h"
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
#include <algorithm>
#include <cstdint>

namespace dyno {

class MemoryMappingPass : public Pass<MemoryMappingPass> {
  Context &ctx;

  // uint32_t getAddressMultiple(RegisterIRef memory) {
  //   for (auto use : memory.oref().uses()) {
  //     auto instr = use.instr();
  //     switch (*instr.getDialectOpcode()) {
  //       case *HW_MEM_LOAD: {
  //         auto asLd = instr.as<MemLoadIRef>();
  //         auto addr = asLd.addr();
  //         if (auto ptr = addr.dyn_as<PointerRef>()) {
  //           auto gep = ptr.getDef().instr().as<GEPIRef>();
  //           for (auto term : gep.terms()) {
  //             term.getFact()
  //           }
  //         }
  //       }
  //       break;
  //     }
  //   }
  // }

  // you scale width and all factors by repeating the model memory
  // -> look for the same factors between factors

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
        actualLoadsWidth, modelLoadsWidth;

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
          mapping{{}, {modelStores.size()}, {modelLoads.size()}, 0} {
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

      // this is repeats to get the required bandwidth - may be limited by loads
      // or stores.
      mapping.repeatCount =
          std::max(round_up_div(actualStoresWidth, modelStoresWidth),
                   round_up_div(actualLoadsWidth, modelLoadsWidth));
    }

    bool mapStores() {
      UnsizedBitSet<SmallVec<uint64_t, 4>> usedModelStore;
      usedModelStore.resizeBits(modelStores.size() * mapping.repeatCount);

      for (auto [actStIdx, actSt] : Range{actualStores}.enumerate()) {
        PortPartition &part = mapping.storePartitions[actStIdx];
        part = PortPartition{actSt.getLen()};

        // operate on conceptual duplicated model stores. one
        // actualStore might need to be covered by multiple modelStores
        for (auto [modStIdx, modSt] : Range{modelStores}.enumerate()) {
          for (unsigned repIdx = 0; repIdx < mapping.repeatCount; repIdx++) {

            // base address of duplicates is shifted.
            // their factor is implicitly multiplied by repIdx.
            const auto baseAddr = modSt.base() + modelPortWidth * repIdx;
            auto adjAddr = baseAddr;
            unsigned globIdx = modStIdx * mapping.repeatCount + repIdx;

            // store's already been used (we do very inefficient matching)
            if (usedModelStore[globIdx])
              continue;
            // constant stuff - we can probably just define no const addr for
            // model mems
            assert(!modSt.addr().is<ConstantRef>() &&
                   !actSt.addr().is<ConstantRef>());

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

            auto modPtr = modSt.addr().as<ObjRef<Pointer>>();
            auto actPtr = actSt.addr().as<ObjRef<Pointer>>();

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

    bool mapLoads() {
      UnsizedBitSet<SmallVec<uint64_t, 4>> usedModelLoad;
      usedModelLoad.resizeBits(modelStores.size() * mapping.repeatCount);

      for (auto [actLdIdx, actLd] : Range{actualLoads}.enumerate()) {
        PortPartition &part = mapping.loadPartitions[actLdIdx];
        part = PortPartition{actLd.getLen()};

        // operate on conceptual duplicated model stores. one
        // actualStore might need to be covered by multiple modelStores
        for (auto [modLdIdx, modLd] : Range{modelLoads}.enumerate()) {

          // can't make loads faster, skip
          if (modLd.port()->delay > actLd.port()->delay)
            continue;

          for (unsigned repIdx = 0; repIdx < mapping.repeatCount; repIdx++) {

            // base address of duplicates is shifted.
            // their factor is implicitly multiplied by repIdx.
            const auto baseAddr = modLd.base() + modelPortWidth * repIdx;
            auto adjAddr = baseAddr;
            unsigned globIdx = modLdIdx * mapping.repeatCount + repIdx;

            // store's already been used (we do very inefficient matching)
            if (usedModelLoad[globIdx])
              continue;
            // constant stuff - we can probably just define no const addr for
            // model mems
            assert(!modLd.addr().is<ConstantRef>() &&
                   !actLd.addr().is<ConstantRef>());

            // If already covered attempt to move forward and cover more via idx
            // offset. (in general not good, wastes alignment)
            bool cov;
            while ((cov = part.isCovered(adjAddr, modLd.getLen())) &&
                   (adjAddr - baseAddr) < modelPortWidth) {
              assert(modLd.terms().front().getFact() == modLd.getLen());
              adjAddr += modLd.getLen() * mapping.repeatCount;
            }
            // can't cover anything new, port is wasted (also not good)
            if (cov)
              continue;

            auto modPtr = modLd.addr().as<ObjRef<Pointer>>();
            auto actPtr = actLd.addr().as<ObjRef<Pointer>>();

            auto &p =
                mapping.boundPorts
                    .findOrInsert(modPtr,
                                  SmallVec<BoundPort, 4>(mapping.repeatCount))
                    .second.val();
            // address object is already bound to something else.
            if (p[repIdx].addr != Any{actPtr, nullref})
              continue;
            p[repIdx].addr = actPtr;

            p[repIdx].idxOffs =
                (adjAddr - baseAddr) / modLd.terms().front().getFact();

            part.writeSingle(adjAddr,
                             std::min(part.getLen() - adjAddr, modLd.getLen()),
                             unsigned(globIdx));
          }
        }

        if (!part.isCovered(0, part.getLen()))
          return false;
      }
      return true;
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

        OperandVec<HWValue> wires{ctx, 1, part.frags.size()};
        wires.emplace_back(actLoad.value());

        for (auto &frag : part.frags) {
          auto idx = frag.set.ctz();
          auto repIdx = idx % mapping.repeatCount;
          auto modLoad = modelLoads[idx / mapping.repeatCount];
          auto &binding =
              mapping.boundPorts[modLoad.addr().as<PointerRef>()][repIdx];

          // find port
          if (actLoad.en())
            connect(actLoad.en(), modLoad.en(), repIdx);
          else if (modLoad.en())
            connect(ConstantRef::fromU32(1), modLoad.en(), repIdx);

          // shift in the address (could also do non pow2 but likely too slow)
          uint32_t addrShamt = 0;
          // length of the model port with repeats. this assumes repeats do the
          // same thing which might be broken.
          uint32_t repLen = (mapping.repeatCount * modelPortWidth);
          if (actLoad.getLen() < repLen)
            ; // too long, simply truncate, wasting capacity. we can add MUXs
              // here at some point
          else if (actLoad.getLen() > repLen) {
            addrShamt = flog2(actLoad.getLen() / repLen);
          }

          auto addr = actLoad.terms().front().getIdx();
          addr = build.buildAdd(
              build.buildSLL(addr, ConstantRef::fromU32(addrShamt)),
              ConstantRef::fromU32(binding.idxOffs));
          connect(addr, modLoad.terms().front().getIdx().as<WireRef>(), repIdx);

          HWValue rdval = connectReverse(modLoad.value(), repIdx);
          rdval = build.buildTrunc(frag.len, rdval);
          wires.emplace_back(rdval);
        }
        build.buildConcat(std::move(wires));
        actLoad.def().replace(FatDynObjRef{nullref});
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
    if (!mapper.mapStores())
      return false;
    if (!mapper.mapLoads())
      return false;

    mapper.apply(ctx);

    return true;

    // constexpr auto filterStore = [](OperandRef op) {
    //   return op.instr().isOpc(HW_MEM_STORE);
    // };
    // constexpr auto filterLoad = [](OperandRef op) {
    //   return op.instr().isOpc(HW_MEM_STORE);
    // };
    // constexpr auto getInstr = [](size_t, auto op) { return op.instr(); };
    // SmallVec<MemStoreIRef, 4> actualStores(
    //     actual.oref().uses().filter(filterStore).transform(getInstr));
    // SmallVec<MemStoreIRef, 4> modelStores(
    //     model.oref().uses().filter(filterStore).transform(getInstr));
    // assert(actualStores.size() <= modelStores.size() && "too few stores");
    // SmallVec<MemStoreIRef, 4> actualLoads(
    //     actual.oref().uses().filter(filterLoad).transform(getInstr));
    // SmallVec<MemStoreIRef, 4> modelLoads(
    //     model.oref().uses().filter(filterLoad).transform(getInstr));

    // uint32_t actualStoresWidth =
    //     Range{actualStores}
    //         .transform([](size_t, MemStoreIRef st) { return st.getLen(); })
    //         .sum();
    // uint32_t modelStoresWidth =
    //     Range{modelStores}
    //         .transform([](size_t, MemStoreIRef st) { return st.getLen(); })
    //         .sum();

    // // for now we assume all model ports are the same width
    // uint32_t modelPortWidth = modelStores[0].getLen();
    // assert(Range{modelStores}.all(
    //     [=](auto st) { return st.getLen() == modelPortWidth; }));
    // assert(Range{modelLoads}.all(
    //     [=](auto st) { return st.getLen() == modelPortWidth; }));
    // // we also assume there's one term to address the entire memory, with
    // factor
    // // equal modelPortWidth.

    // uint32_t repeatCount = round_up_div(actualStoresWidth, modelStoresWidth);

    // (conceptually) split memory to legalize.
    // modelStores/loads factors all multiplied, bases
    // become +0, +len, +2len...

    /*
    Legalization options:
      - increase width
        - remaps addresses over multiple memories. be careful if ports have
    different sizes
      - increase depth
      -

    */

    // Duplicate for more store bandwidth -> do get more read bandwidth

    // Loads:
    //  - partly just same sort of covering
    //  - but: can also make more load ports by duplicating everything done up
    //  to now
    //

    // assume canonicalized to remove useless base.
    // assume canon'd to all bits covered
    // assume canon'd factor is multiple of size
    // assume canon'd power of 2 factors into concat (x * 16 + y[3:0])
    // assume all loads in model same size

    // smallest base first, most factors first, biggest factors first
    // constexpr auto sort = [](MemStoreIRef a, MemStoreIRef b) {
    //   if (a.base() != b.base())
    //     return a.base() < b.base();

    //   auto aT = a.terms();
    //   auto bT = b.terms();
    //   if (aT.size() == bT.size()) {
    //     if (aT.empty())
    //       return false;
    //     if (aT.front().getFact() > bT.front().getFact())
    //       return aT.front().getFact() > bT.front().getFact();
    //   }
    //   return aT.size() > bT.size();
    // };

    // Range{actualStores}.sort(sort);
    // Range{modelStores}.sort(sort);
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
