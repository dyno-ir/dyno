#pragma once

#include "hw/HWContext.h"
#include "hw/HWValue.h"
#include "hw/LoadStore.h"
#include "hw/analysis/BitAliasAnalysis.h"
namespace dyno {

// attempt to partition registers on all bounds not crossed by any access
class RegisterPartitionPass {
  HWContext &ctx;
  BitAliasAnalysis bitAlias;

  struct Regions {
    struct Fragment {
      uint32_t addr;
      uint32_t len;
    };
    SmallVec<Fragment, 4> frags;

    void addPartition(uint32_t addr, uint32_t len) {
      // todo
    }
  };

  auto getStoreRegions(StoreIRef store) {

    bitAlias.getReprAliases(store.value().as<HWValue>());
  }

  void runOnRegister(RegisterIRef reg) {
    for (auto instr : reg.oref().uses()) {
    }
  }

  void runOnModule(ModuleIRef mod) {
    for (auto reg : mod.regs())
      runOnRegister(reg);
  }

public:
  explicit RegisterPartitionPass(HWContext &ctx) : ctx(ctx) {}
  void run() {
    for (auto mod : ctx.activeModules()) {
      runOnModule(mod.iref());
    }
  }
};
}; // namespace dyno
