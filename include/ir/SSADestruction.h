#pragma once

#include "ir/IRPass.h"
#include "ir/SSAIRBuilder.h"

#include <ranges>

class PhiIsolationPass : public IRPass<Function> {
  const char *name() override { return "PhiIsolationPass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    for (auto &block : func) {
      for (auto &instr : block) {
        if (!instr.isPhi()) {
          break;
        }
        auto phi = PhiInstrPtr(&instr);
        for (unsigned i = 0, end = phi.getNumPredecessors(); i < end; ++i) {
          auto &pred = phi.getPredecessorBlock(i);
          auto builder = SSAIRBuilder(pred.getLast());
          builder.emitCopy(phi.getPredecessorDef(i));
          phi.setPredecessor(i, builder.getDef(), pred);
        }
        auto builder = SSAIRBuilder(block.getFirstNonPhiSentry());
        auto &copyInstr = builder.emitCopy(phi->getDef());
        phi->getDef().ssaDef().replaceAllUses(copyInstr.getDef());
        copyInstr.getOperand(1).ssaUse().replace(phi->getDef());
      }
    }
  }
};

class PhiDestructionPass : public IRPass<Function> {
  const char *name() override { return "PhiDestructionPass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    RegInfo &regInfo = func.getRegInfo();
    for (auto &block : func) {
      for (auto it = block.begin(), itEnd = block.end(); it != itEnd;) {
        auto &instr = *it;
        ++it;
        if (!instr.isPhi()) {
          break;
        }
        auto phi = PhiInstrPtr(&instr);
        Reg reg = regInfo.createVReg(instr.getDef().ssaDef().type());
        for (unsigned i = 0, end = phi.getNumPredecessors(); i < end; ++i) {
          auto &def = phi.getPredecessorDef(i);
          def.ssaDef().replace(reg);
        }
        instr.getDef().ssaDef().replace(reg);
        instr.deleteThis();
      }
    }
  }
};

class SSADestructionPass : public IRPass<Function> {
  const char *name() override { return "SSADestructionPass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    RegInfo &regInfo = func.getRegInfo();
    for (auto &block : func) {
      for (auto it = block.begin(), itEnd = block.end(); it != itEnd;) {
        auto &instr = *it;
        ++it;
        for (auto &op :
             std::ranges::subrange(instr.def_begin(), instr.def_end())) {
          if (!op.isSSARegDef())
            continue;
          Reg reg = regInfo.createVReg(op.ssaDef().type());
          op.ssaDef().replace(reg);
        }
      }
    }
  }
};
