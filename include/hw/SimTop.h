#pragma once
#include "dyno/Context.h"
#include "hw/HWContext.h"
#include "hw/Module.h"
#include "hw/Register.h"
#include "hw/SimHeader.h"
#include "hw/run/HWInterpreter.h"
#include "support/Debug.h"
#include "support/SlabAllocator.h"
#include "support/TwoLevelSet.h"
#include <iostream>
#include <memory>
#include <tuple>
template <typename Top> class SimulationTop {
public:
  dyno::Context &ctx;
  std::unique_ptr<Top> top;
  dyno::HWInterpreter interp;

private:
  TwoLevelMap<StringRef, SimRegisterHandle *> map;
  MixedSizeSlabAllocator<> stringAlloc;

  void builRegMapRecursive(std::string &prefix, auto &node) {
    auto sz = prefix.size();
    std::apply(
        [&](auto &&...subNode) {
          (
              [&]() {
                if (sz != 0)
                  prefix += ".";
                prefix += subNode.name;
                builRegMapRecursive(prefix, subNode);
                prefix.resize(sz);
              }(),
              ...);
        },
        node.getAllSubModules());

    node.forAllRegs([&](const char *nm, SimRegisterHandle &handle) {
      if (sz != 0)
        prefix += '.';
      prefix += nm;

      auto ptr = (char *)stringAlloc.resolve(
          stringAlloc.allocate_nbytes(prefix.size()));
      memcpy(ptr, prefix.data(), prefix.size());
      map.insert(StringRef(ptr, prefix.size()), &handle);

      prefix.resize(sz);
    });
  }
  void buildRegMap() {
    std::string prefix;
    builRegMapRecursive(prefix, *top);
  }

public:
  void linkRegisters() {
    auto &regNameInfo = ctx.getCtx<dyno::HWDialectContext>().regNameInfo;
    uint32_t linked = 0;
    for (auto reg : ctx.getStore<dyno::Register>()) {
      for (auto name : regNameInfo.getNames(reg)) {
        if (auto it = map.find(name); it != map.end()) {
          (*it.val()) = SimRegisterHandle{&interp, reg, 0, *reg.getNumBits()};
          linked++;
        }
      }
    }
    std::print(dyno::dbgs(), "linked {} of {} regs\n", linked, map.size());
  }
  SimulationTop(dyno::Context &ctx)
      : ctx(ctx), top(std::make_unique<Top>()),
        interp(ctx, (*ctx.getStore<dyno::Module>().begin()).iref(), std::cout,
               std::cerr) {
    assert(ctx.getStore<dyno::Module>().numIDs() == 1);
    buildRegMap();
    linkRegisters();
    interp.setup();

#ifdef ENABLE_FST
    interp.fstWriter.emplace(ctx, "out.fst");
    interp.fstInitHierarchy();
#endif

    interp.initialEval();
    interp.initialCombEval();

    interp.trace = false;
  }

  void eval() { interp.eval(); }
  void forwardTime(uint64_t delta) { interp.forwardTime(delta); }
};
