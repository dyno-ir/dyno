#pragma once

#include "dyno/Context.h"
#include "dyno/Pass.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/Module.h"
#include "hw/run/HWInterpreter.h"
#include "support/ErrorRecovery.h"
#include <random>
namespace dyno {

class RandomEquivalenceCheckPass : public Pass<RandomEquivalenceCheckPass> {
  Context &ctx;

public:
#define CONFIG_STRUCT_LAMBDA(FIELD, ENUM)                                      \
  FIELD(uint64_t, cycles, 1000)                                                \
  ENUM(clockMode, COMB, COMB, CLOCK)                                           \
  FIELD(bool, exhaustive, false)                                               \
  FIELD(bool, relaxXZ, false)                                                  \
  FIELD(bool, swapTestAndModel, false)                                         \
  FIELD(bool, trace, false)
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
  Config config;

private:
  ModuleIRef getModelModule(ModuleIRef testModule) {
    SmallVec<ModuleRef, 4> mods(Range{ctx.getStore<Module>()}.filter(
        [](auto mod) { return mod.iref().isOpc(HW_MODULE_DEF); }));
    if (mods.size() != 2)
      report_fatal_error("expected two modules (model and test)");
    auto it = Range{mods}.find(testModule.mod());
    if (it == mods.end())
      report_fatal_error("test module not active?");
    return mods[!(it - mods.begin())].iref();
  }

  bool runOnModule(ModuleIRef test) {
    auto model = getModelModule(test);
    if (config.swapTestAndModel)
      std::swap(test, model);

    HWInterpreter modelInterp{ctx, test, std::cout, std::cerr};
    modelInterp.setup();
    HWInterpreter testInterp{ctx, test, std::cout, std::cerr};
    testInterp.setup();

    testInterp.trace = config.trace;

    auto testClk = test.ports().front();
    auto modelClk = model.ports().front();

    auto filterInputs = [](RegisterIRef reg) {
      return reg.isOpc(HW_INPUT_REGISTER_DEF);
    };
    auto filterOutputs = [](RegisterIRef reg) {
      return reg.isOpc(HW_OUTPUT_REGISTER_DEF);
    };
    std::mt19937 rand(42);
    BigInt clk = "1'b0"_bv;
    // set these to avoid initial x -> 0 transition
    testInterp.getReg(testClk) = clk;
    modelInterp.getReg(modelClk) = clk;
    for (uint64_t i = 0; i < config.cycles || config.exhaustive; i++) {
      bool carry = true;
      for (auto [testIn, modelIn] :
           Range{test.ports().filter(filterInputs)}.zip(
               model.ports().filter(filterInputs))) {
        if (config.clockMode == Config::CLOCK && testIn == testClk) {
          assert(modelIn == modelClk);
          continue;
        }
        if (config.exhaustive) {
          auto &val = (modelInterp.getReg(modelIn) += "1'b1"_bv);
          testInterp.getReg(testIn) = val;
          modelInterp.regValueChanged(modelIn);
          testInterp.regValueChanged(testIn);
          if (val.valueEquals(0))
            carry = true;
          else {
            carry = false;
            break;
          }
        } else {
          auto &val = modelInterp.getReg(modelIn);
          val.randomize(rand);
          if (config.trace) {
            val.toStream(dbgs());
            dbgs() << " -> ";
            dumpInstr(testIn, ctx);
          }
          modelInterp.regValueChanged(modelIn);
          testInterp.getReg(testIn) = val;
          testInterp.regValueChanged(testIn);
        }
      }

      if (config.exhaustive && carry)
        break;

      if (config.clockMode == Config::COMB) {
        testInterp.eval();
        modelInterp.eval();
      }

      for (auto [testOut, modelOut] :
           Range{test.ports().filter(filterOutputs)}.zip(
               model.ports().filter(filterOutputs))) {

        bool r = BigInt::icmpOp4S(
            modelInterp.getReg(modelOut), testInterp.getReg(testOut),
            config.relaxXZ ? BigInt::ICMP_CXEQ : BigInt::ICMP_CEQ);
        if (!r) {
          std::stringstream regS;
          HWPrinter print{regS};
          auto t = print.bindCtx(ctx);
          print.printInstr(testOut);

          std::stringstream inpS;
          print.str = inpS;
          for (auto inp : test.ports().filter(filterInputs)) {
            print.printInstr(inp, false);
            inpS << " : ";
            testInterp.getReg(inp).toStream(inpS);
            inpS << "\n";
          }
          for (auto wire : ctx.getStore<Wire>()) {
            auto &val = testInterp.getWire(wire);
            if (val.getNumBits() == 0)
              continue;
            print.introduceAndPrintDef(wire);
            inpS << " : ";
            val.toStream(inpS);
            inpS << "\n";
          }

          std::print(
              dbgs(),
              "mismatch (cycle {}) {}actual: {}\nexpected: {}\n\ninputs:\n{}",
              i, regS.str(), testInterp.getReg(testOut).toString(),
              modelInterp.getReg(modelOut).toString(), std::move(inpS).str());

          return false;
        }
      }

      if (config.clockMode == Config::CLOCK) {
        clk = ~clk;
        testInterp.setReg(testClk, clk);
        modelInterp.setReg(modelClk, clk);

        testInterp.eval();
        modelInterp.eval();

        clk = ~clk;
        testInterp.setReg(testClk, clk);
        modelInterp.setReg(modelClk, clk);

        testInterp.eval();
        modelInterp.eval();
      }
    }
    return true;
  }

public:
  bool runModule(ModuleIRef mod) { return runOnModule(mod); }

  bool run() {
    for (auto mod : ctx.getCtx<HWDialectContext>().activeModules())
      if (!runOnModule(mod.iref()))
        return false;
    return true;
  }

  static constexpr auto runFuncs = mk_tuple(
      &RandomEquivalenceCheckPass::run, &RandomEquivalenceCheckPass::runModule);

  explicit RandomEquivalenceCheckPass(Context &ctx) : ctx(ctx) {}
  auto make(Context &ctx) { return RandomEquivalenceCheckPass{ctx}; }
};
}; // namespace dyno
