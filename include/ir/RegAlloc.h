#pragma once

#include "ir/ArchIRBuilder.h"
#include "ir/IR.h"
#include "ir/IRPass.h"
#include "ir/RegLiveness.h"
#include "support/Ranges.h"
#include <cassert>

class RegAssignment {
public:
  static const IRInfoID ID;

  struct Assignment {
    enum Kind {
      NONE,
      PHYS_REG,
      NO_REG,
    };

    Kind kind = NONE;
    Reg reg;
    size_t spillCost;

    void assignPhysReg(Reg r) {
      assert(r.isPhysReg());
      kind = PHYS_REG;
      reg = r;
    }

    void assignNoReg() { kind = NO_REG; }
  };

  RegAssignment() {}
  RegAssignment(size_t numVRegs) : vRegs(numVRegs) {}

  VRegMap<Assignment> vRegs;
};

class RegClobber {
public:
  static const IRInfoID ID;

  RegClobber() {}
  RegClobber(size_t numPhysRegs) : phys(numPhysRegs) {}

  PhysLiveSet phys;
};

class RegRewritePass : public IRPass<Function> {
  const char *name() override { return "RegRewritePass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    clobber = RegClobber(info.getArch().getArchRegs().size());
    RegAssignment &assignment = info.query<RegAssignment>();

    for (auto &block : func) {
      for (auto &instr : make_earlyincr_range(block)) {
        // Apply RegAlloc assignments
        for (auto &op : instr) {
          if (!op.isReg() || !op.reg().isVReg())
            continue;
          auto &regAssign = assignment.vRegs[op.reg()];
          assert(regAssign.kind == RegAssignment::Assignment::PHYS_REG);
          op.regDefUse().replace(regAssign.reg);
        }
        // Remove no-op copies
        if (instr.isCopy()) {
          if (instr.getOperand(0).reg() == instr.getOperand(1).reg()) {
            instr.deleteThis();
            continue;
          }
        }
        // Track clobbered registers
        for (auto &op : instr.defs()) {
          if (!op.isRegDef())
            continue;
          assert(op.reg().isPhysReg());
          clobber.phys.insert(op.reg());
        }
      }
    }

    info.publish(clobber);
  }

  RegClobber clobber;
};

class ColoringRegAllocPass : public IRPass<Function> {

  const char *name() override { return "ColoringRegAllocPass"; }

  void run(Function &func, IRInfo<Function> &info) override {
    this->func = &func;
    printer = &info.query<PrintIRVisitor>();
    arch = &info.getArch();
    regInfo = &func.getRegInfo();

    const ArchRegClass &regClass = *arch->getArchRegClass(1);

    bool failed;
    do {
      graph = &info.query<LiveGraph>();
      assignment = RegAssignment(graph->getNumVRegs());
      virtSet = VLiveSet(graph->getNumVRegs());
      physSet = PhysLiveSet(graph->getNumPhysRegs());
      simplify(regClass.regs.size());
      selectAll();
      spillAll();
      failed = false;
      for (auto [reg, regAssign] : assignment.vRegs) {
        if (regAssign.kind != RegAssignment::Assignment::PHYS_REG) {
          failed = true;
          break;
        }
      }
      info.invalidate<LiveFlow>();
      info.invalidate<LiveGraph>();
    } while (failed);

    info.publish(assignment);
  }

  void simplify(size_t k) {
    std::cout << "simplify\n";
    eliminationOrder.clear();
    graph->resetIgnore();
    virtSet.clear();

    // Prune trivially colorable nodes
    for (auto [reg, vLive] : graph->vRegs) {
      if (vLive.degree < k) {
        eliminate(reg);
      } else {
        virtSet.insert(reg);
      }
    }

    // Eliminate other nodes driven by spill-cost
    calcSpillCost();
    while (!virtSet.empty()) {
      Reg spillReg = Reg();
      for (auto reg : virtSet) {
        if (graph->vRegs[reg].degree < k) {
          eliminate(reg);
          virtSet.erase(reg);
          spillReg = Reg();
          break;
        }
        // Minimize spillCost/degree
        if (!spillReg ||
            assignment.vRegs[reg].spillCost * graph->vRegs[spillReg].degree <
                assignment.vRegs[spillReg].spillCost *
                    graph->vRegs[reg].degree) {
          spillReg = reg;
        }
      }
      if (!spillReg)
        continue;
      eliminate(spillReg);
      virtSet.erase(spillReg);
      std::cout << "  might spill " << spillReg.getIdx() << " with cost "
                << assignment.vRegs[spillReg].spillCost << "/"
                << graph->vRegs[spillReg].degree << "\n";
    }
  }

  void eliminate(Reg reg) {
    eliminationOrder.push_back(reg);
    graph->ignore(reg);
  }

  void calcSpillCost() {
    for (auto reg : virtSet) {
      // TODO: better heuristic
      assignment.vRegs[reg].spillCost = regInfo->defUseRoot(reg).count();
    }
  }

  void selectAll() {
    for (auto reg : eliminationOrder | std::views::reverse) {
      select(reg);
    }
  }

  void spillAll() {
    for (auto [reg, regAssign] : assignment.vRegs) {
      if (regAssign.kind != RegAssignment::Assignment::NO_REG)
        continue;
      spillGlobal(reg);
    }
  }

  void select(Reg reg) {
    std::cout << "select " << reg.getIdx() << ": ";
    physSet.clear();
    auto &regLive = graph->vRegs[reg];
    auto &regAssign = assignment.vRegs[reg];

    // Accumulate all interfering registers
    physSet.insert(regLive.phys);
    for (auto oReg : regLive.virt) {
      auto &oRegAssign = assignment.vRegs[oReg];
      if (oRegAssign.kind != RegAssignment::Assignment::PHYS_REG)
        continue;
      physSet.insert(oRegAssign.reg);
    }

    // FIXME: cache this
    const ArchRegClass &regClass = *arch->getArchRegClass(1);
    PhysLiveSet regClassSet(graph->getNumPhysRegs());
    for (auto *archReg : regClass.regs) {
      regClassSet.insert(archReg->reg);
    }

    // Try to respect a register hint
    for (auto hintReg : regLive.coloringHints) {
      std::cout << "try hint " << hintReg.getNum() << ", ";
      if (hintReg.isVReg()) {
        auto &hintRegAssign = assignment.vRegs[hintReg];
        if (hintRegAssign.kind != RegAssignment::Assignment::PHYS_REG) {
          continue;
        }
        hintReg = hintRegAssign.reg;
        std::cout << "hint is " << hintReg.getNum() << ", ";
      }
      assert(hintReg.isPhysReg());
      if (!regClassSet.contains(hintReg))
        continue;
      if (physSet.contains(hintReg))
        continue;
      std::cout << "assign hint " << hintReg.getNum() << "\n";
      regAssign.assignPhysReg(hintReg);
      return;
    }

    // Try to assign any other register in the register class
    // This respects the register order of the DSL RegClass
    for (auto *archReg : regClass.regs) {
      if (physSet.contains(archReg->reg)) {
        continue;
      }
      std::cout << "assign " << archReg->name << "\n";
      regAssign.assignPhysReg(archReg->reg);
      return;
    }

    std::cout << "no phys reg available\n";
    regAssign.assignNoReg();
  }

  void spillGlobal(Reg reg) {
    std::cout << "global spill " << reg.getIdx() << "\n";
    auto &archIB = arch->getArchIRBuilder();
    // FIXME: proper size calculation
    auto &frameDef = func->getFrameLayout().createFrameEntry(4, Alignment(2));
    for (auto &op : make_earlyincr_range(regInfo->defUseRoot(reg))) {
      Reg newReg = regInfo->cloneVReg(reg);
      op.regDefUse().replace(newReg);
      if (op.isRegDef()) {
        IRBuilder IB(op.getParent().getNextNode());
        archIB.frameStoreReg(IB, newReg, frameDef);
      } else {
        IRBuilder IB(op.getParent());
        archIB.frameLoadReg(IB, newReg, frameDef);
      }
    }
  }

  void invalidate(IRInfo<Function> &info) override {
    info.retract<RegAssignment>();
  }

private:
  RegAssignment assignment;
  PhysLiveSet physSet;
  VLiveSet virtSet;
  std::vector<Reg> eliminationOrder;
  Function *func;
  RegInfo *regInfo;
  Arch *arch;
  LiveGraph *graph;
  PrintIRVisitor *printer;
};
