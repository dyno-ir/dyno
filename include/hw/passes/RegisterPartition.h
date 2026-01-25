#pragma once

#include "dyno/Context.h"
#include "dyno/Pass.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/analysis/BitAliasAnalysis.h"
#include "hw/analysis/RegisterValue.h"
#include "support/Utility.h"
#include <algorithm>
namespace dyno {

// attempt to partition registers on all bounds not crossed by any access
class RegisterPartitionPass : public Pass<RegisterPartitionPass> {
  Context &ctx;
  BitAliasAnalysis bitAlias;

  void getStoreRegions(RegisterPartitions &part, StoreIRef store) {
    if (!store.isConstantOffs() || store.value().is<ConstantRef>()) {
      auto addr = store.getBase();
      Optional<uint64_t> len = store.getLen();
      for (auto term : store.terms()) {
        if (!term.getMax()) {
          len = nullopt;
          break;
        }
        *len += term.getFact() * *term.getMax();
      }

      if (!len)
        len = *store.reg().getNumBits() - addr;
      part.addPartition(addr, *len);
      return;
    }

    auto wire = store.value().as<WireRef>();
    auto [val, change] = bitAlias.getReprAliases(wire);
    for (auto frag : val.frags) {
      part.addPartition(frag.dstAddr + store.getBase(), frag.len);
    }
  }

  void buildSplitStore(RegisterPartitions &part, ArrayRef<RegisterRef> regs,
                       StoreIRef store) {
    HWInstrBuilder build{ctx, store};
    for (auto [i, frag] : Range{part.frags}.enumerate()) {
      if (frag.dstAddr < store.getBase())
        continue;
      if (frag.dstAddr + frag.len - store.getBase() > store.getLen())
        break;
      auto newReg = regs[i];
      assert(frag.len != 0);
      auto val = build.buildSplice(store.value(), frag.len,
                                   frag.dstAddr - store.getBase());
      build.buildStore(newReg, val, store.isOpc(HW_STORE_DEFER),
                       store.trigger(), 0, store.terms());
    }
  }

  void buildSplitLoad(RegisterPartitions &part, ArrayRef<RegisterRef> regs,
                      LoadIRef load) {
    SmallVec<HWValue, 4> frags;
    HWInstrBuilder build{ctx, load};
    auto it = part.getInsertIt(load.getBase());
    for (; it != part.frags.end(); ++it) {
      auto &frag = *it;
      auto i = it - part.frags.begin();

      if (frag.dstAddr >= load.getBase() + load.getLen())
        break;
      auto start = std::max(frag.dstAddr, load.getBase());
      auto end =
          std::min(frag.dstAddr + frag.len, load.getBase() + load.getLen());
      assert(end > start);
      auto val = build.buildLoad(regs[i], end - start, start - frag.dstAddr);
      frags.emplace_back(val);
    }
    assert(!frags.empty());
    std::reverse(frags.begin(), frags.end());
    auto val = build.buildConcat(ArrayRef{frags});
    load.def(0)->as<WireRef>().replaceAllUsesWith(val);
  }

  void runOnRegister(RegisterIRef reg) {
    RegisterPartitions part;
    for (auto use : reg.oref().uses()) {
      auto instr = use.instr();
      if (instr.isOpc(HW_STORE, HW_STORE_DEFER))
        getStoreRegions(part, instr.as<StoreIRef>());
    }

    DYNO_DBG("RegisterPartition", {
      dumpInstr(reg, ctx);
      dbgs() << "found partitions:\n";
      for (auto [back, frag] : Range{part.frags}.mark_back()) {
        dbgs() << "[" << frag.dstAddr << "+:" << frag.len << "]";
        dbgs() << (back ? "\n" : ", ");
      }
    });

    auto cbuild = ConstantBuilder{ctx.getStore<Constant>()};

    ModuleIRef mod = HWInstrRef{reg}.parentMod(ctx);
    HWInstrBuilder build{ctx, mod.regs_end()};

    if (part.frags.size() <= 1)
      return;

    if (part.getLen() != reg.getNumBits())
      part.addPartition(part.getLen(), *reg.getNumBits() - part.getLen());

    SmallVec<RegisterRef, 4> regs;
    regs.reserve(part.frags.size());
    for (auto frag : part.frags)
      regs.emplace_back(build.buildRegister(frag.len));

    SmallVec<InstrRef, 4> destroyList;
    for (auto use : reg.oref().uses()) {
      auto instr = use.instr();
      if (instr.isOpc(HW_STORE, HW_STORE_DEFER)) {
        buildSplitStore(part, regs, instr.as<StoreIRef>());
        destroyList.emplace_back(instr);
      } else if (instr.isOpc(HW_LOAD)) {
        buildSplitLoad(part, regs, instr.as<LoadIRef>());
        destroyList.emplace_back(instr);
      } else
        dyno_unreachable("unsupported");
    }

    build.setInsertPoint(mod.block().end());
    build.setInsertPoint(build.buildProcess().block().end());
    for (auto [idx, frag] : Range{part.frags}.enumerate()) {
      build.buildStore(reg.oref(), build.buildLoad(regs[idx]), false, nullref,
                       frag.dstAddr);
    }

    for (auto instr : destroyList)
      build.destroyInstr(instr);
    bitAlias.clearCache();
  }

  void runOnModule(ModuleIRef mod) {
    bitAlias.clearCache();
    for (auto reg : mod.regs())
      runOnRegister(reg);
  }

public:
  auto make(Context &ctx) { return RegisterPartitionPass(ctx); }
  explicit RegisterPartitionPass(Context &ctx) : ctx(ctx), bitAlias(ctx) {}
  void run() {
    for (auto mod : ctx.getCtx<HWDialectContext>().activeModules()) {
      runOnModule(mod.iref());
    }
  }
};
}; // namespace dyno
