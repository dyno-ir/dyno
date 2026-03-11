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
  FIELD(bool, relaxXZ, false)
  CONFIG_STRUCT(CONFIG_STRUCT_LAMBDA)
#undef CONFIG_STRUCT_LAMBDA
  Config config;

private:
  ModuleIRef getModelModule(ModuleIRef testModule) {
    assert(ctx.getStore<Module>().numIDs() == 2);
    return ctx.getStore<Module>()
        .resolve(ObjRef<Module>{ObjID{1 - testModule.mod().getObjID()}})
        .iref();
  }

  void runOnModule(ModuleIRef test) {
    auto model = getModelModule(test);

    HWInterpreter modelInterp{ctx, test, std::cout, std::cerr};
    modelInterp.setup();
    HWInterpreter testInterp{ctx, test, std::cout, std::cerr};
    testInterp.setup();

    auto testClk = test.ports().front();
    auto modelClk = model.ports().front();

    auto filterInputs = [](RegisterIRef reg) {
      return reg.isOpc(HW_INPUT_REGISTER_DEF);
    };
    auto filterOutputs = [](RegisterIRef reg) {
      return reg.isOpc(HW_OUTPUT_REGISTER_DEF);
    };
    std::mt19937 rand(42);
    BigInt clk = "1'b1"_bv;
    for (uint64_t i = 0; i < config.cycles || config.exhaustive; i++) {
      bool carry = true;
      for (auto [testIn, modelIn] :
           Range{test.ports().filter(filterInputs)}.zip(
               model.ports().filter(filterInputs))) {
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
          modelInterp.regValueChanged(modelIn);
          testInterp.getReg(testIn) = val;
          testInterp.regValueChanged(testIn);
        }
      }

      if (config.exhaustive && carry)
        break;

      if (config.clockMode == Config::CLOCK) {
        testInterp.setReg(testClk, clk);
        modelInterp.setReg(modelClk, clk);
        clk = ~clk;
      } else {
        testInterp.eval();
        modelInterp.eval();
      }

      for (auto [testOut, modelOut] :
           Range{test.ports().filter(filterOutputs)}.zip(
               model.ports().filter(filterOutputs))) {

        bool r = BigInt::icmpOp4S(
            modelInterp.getReg(modelOut), testInterp.getReg(testOut),
            config.relaxXZ ? BigInt::ICMP_CXEQ : BigInt::ICMP_EQ);
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
            print.introduceAndPrintDef(wire);
            inpS << " : ";
            testInterp.getWire(wire).toStream(inpS);
            inpS << "\n";
          }

          report_fatal_error(
              "mismatch {}actual: {}\nexpected: {}\n\ninputs:\n{}", regS.str(),
              testInterp.getReg(testOut).toString(),
              modelInterp.getReg(modelOut).toString(), std::move(inpS).str());
        }
      }

      if (config.clockMode == Config::CLOCK) {
        testInterp.eval();
        modelInterp.eval();

        testInterp.setReg(testClk, clk);
        modelInterp.setReg(modelClk, clk);

        testInterp.eval();
        modelInterp.eval();

        clk = ~clk;
      }
    }
  }

public:
  void runWrapper(auto &&runFunc) { runFunc(); }

  void runModule(ModuleIRef mod) {
    runWrapper([&] { runOnModule(mod); });
  }

  void run() {
    runWrapper([&] {
      for (auto mod : ctx.getCtx<HWDialectContext>().activeModules())
        runOnModule(mod.iref());
    });
  }

  static constexpr auto runFuncs = mk_tuple(
      &RandomEquivalenceCheckPass::run, &RandomEquivalenceCheckPass::runModule);

  explicit RandomEquivalenceCheckPass(Context &ctx) : ctx(ctx) {}
  auto make(Context &ctx) { return RandomEquivalenceCheckPass{ctx}; }
};
}; // namespace dyno
