#pragma once

#include "dyno/Constant.h"
#include "dyno/CustomInstr.h"
#include "dyno/HierBlockIterator.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWPrinter.h"
#include "hw/HWValue.h"
#include "hw/Wire.h"
#include "op/IDs.h"
#include "op/StructuredControlFlow.h"
#include "support/Debug.h"
#include "support/Utility.h"
#include <algorithm>

namespace dyno {

bool generated(HWContext &ctx, HWInstrRef);

class InstCombinePass {
  HWContext &ctx;
  SmallVec<InstrRef, 128> worklist;

  ConstantBuilder cbuild;

public:
  using TaggedIRef = CustomInstrRef<InstrRef, uint64_t>;

private:
  bool simplifyAndCanonicalizeCommOps(InstrRef root) {
    SmallVec<WireRef, 8> operands;
    SmallVec<ConstantRef, 8> constants;
    SmallVec<InstrRef, 4> stack{root};
    auto opc = root.getDialectOpcode();

    auto oldDefW = root.def(0)->as<WireRef>();

    bool change = false;
    while (!stack.empty()) {
      auto instr = stack.pop_back_val();
      for (auto operand : instr.others()) {
        if (auto asWire = operand->dyn_as<WireRef>()) {
          auto defI = asWire.getSingleDef()->instr();
          if (defI.isOpc(opc) && asWire.hasSingleUse()) {
            stack.emplace_back(defI);
            // mark to delete
            TaggedIRef{defI}.get() = 1;
            change = true;
            continue;
          }
          operands.emplace_back(operand->as<WireRef>());
          continue;
        }
        constants.emplace_back(operand->as<ConstantRef>());
        continue;
      }
    }
    change |= constants.size() > 1;
    if (constants.size() > 1) {
      cbuild.val(constants.front());
      switch (*root.getDialectOpcode()) {
#define FUNC(hb, opc, cb)                                                      \
  case *opc:                                                                   \
    for (auto val : Range{constants}.drop_front())                             \
      cbuild.cb(val);                                                          \
    break;
        FOR_HW_COMM_OPS(FUNC)
#undef FUNC
      }
      constants.front() = cbuild.get();
    }
    if (!constants.empty()) {
      switch (*root.getDialectOpcode()) {
      case *OP_OR:
        if (constants[0].valueEqualsS(-1)) {
          TaggedIRef{root}.get() = 1;
          oldDefW.replaceAllUsesWith(cbuild.ones(*oldDefW->numBits).get());
          return true;
        }
        [[fallthrough]];
      case *OP_ADD:
        if (constants[0].valueEquals(0)) {
          change |= 1;
          constants.clear();
        }
        break;
      case *OP_AND:
        if (constants[0].valueEquals(0)) {
          TaggedIRef{root}.get() = 1;
          oldDefW.replaceAllUsesWith(cbuild.zero(*oldDefW->numBits).get());
          return true;
        }
        if (constants[0].valueEqualsS(-1)) {
          change |= 1;
          constants.clear();
        }
        break;
      default:;
      }
    }

    TaggedIRef{root}.get() = 1;
    if (operands.size() + constants.size() <= 1) {
      if (operands.size() + constants.size() == 0) {
        switch (*root.getDialectOpcode()) {
        case *OP_XOR:
        case *OP_ADD:
        case *OP_OR: {
          oldDefW.replaceAllUsesWith(cbuild.zero(*oldDefW->numBits).get());
          return true;
        }
        case *OP_MUL: {
          oldDefW.replaceAllUsesWith(cbuild.one(*oldDefW->numBits).get());
          return true;
        }
        case *OP_XNOR:
        case *OP_AND: {
          oldDefW.replaceAllUsesWith(cbuild.ones(*oldDefW->numBits).get());
          return true;
        }
        default:
          break;
        }
        dyno_unreachable("no neutral element");
      }
      switch (*root.getDialectOpcode()) {
      case *OP_ADD:
      case *OP_AND:
      case *OP_OR:
      case *OP_XOR:
      case *OP_MUL:
        if (constants.size() == 1)
          oldDefW.replaceAllUsesWith(constants[0]);
        else
          oldDefW.replaceAllUsesWith(operands[0]);
        return true;
      case *OP_XNOR:
        if (constants.size() == 1)
          oldDefW.replaceAllUsesWith(cbuild.val(constants[0]).bitNOT().get());
        else {
          // while weird this is actually the best way to represent the this
          // currently.
          TaggedIRef{root}.get() = 0;
          return false;
        }
        return true;
      }
      dyno_unreachable("no 1-ary output value");
    }

    if (!change) {
      TaggedIRef{root}.get() = 0;
      return false;
    }
    HWInstrBuilder build{ctx};
    build.setInsertPoint(ctx.getCFG()[root]);

    std::stable_sort(operands.begin(), operands.end(),
                     [](WireRef lhs, WireRef rhs) {
                       return lhs.getSingleDef()->instr().getDialectOpcode() <
                              rhs.getSingleDef()->instr().getDialectOpcode();
                     });

    ++build.insert;
    auto ibuild = build.buildInstrRaw(opc, 1 + operands.size() +
                                               (constants.empty() ? 0 : 1));

    // todo: steal slot mechanism
    ibuild.addRef(oldDefW);
    ibuild.other();
    for (auto operand : operands)
      ibuild.addRef(operand);
    if (!constants.empty())
      ibuild.addRef(constants[0]);

    // todo: delete non-root
    for (auto op : root)
      op.replace(FatDynObjRef<>{nullref});
    return true;
  }

  // bool simplifyWhile(InstrRef instr) {
  //   auto asWhile = instr.as<WhileInstrRef>();
  //   auto cond = asWhile.getCondBlock().as<BlockRef>();
  //   assert(!cond.empty());
  //   auto yield = cond.end().pred().instr();
  //   assert(yield.isOpc(OP_YIELD));

  //   auto boolCond = yield.operand(0)->as<HWValue>();
  //   if (auto wire = boolCond.dyn_as<WireRef>()) {
  //     auto defI = wire.getDefI();
  //     if (defI.getNumOperands() != 3)
  //       return false;

  //     for (uint i = 1; i < 2; i++) {
  //       auto op = defI.operand(i);
  //       if (auto asWire = op->dyn_as<WireRef>();
  //           asWire && asWire.getDefI().isOpc(OP_UNYIELD)) {

  //       }
  //     }
  //   }

  //   auto body = asWhile.getBodyBlock().as<BlockRef>();
  // }

  bool manual(InstrRef instr) {
    switch (*instr.getDialectOpcode()) {
#define LAMBDA(ib, opc, cb) case *opc:
      FOR_HW_COMM_OPS(LAMBDA)
#undef LAMBDA
      return simplifyAndCanonicalizeCommOps(instr);
    default:
    }

    return false;
  }

  bool runOnInstr(InstrRef instr) {
    if (instr.getNumDefs() > 1)
      return false;
    if (instr.getNumDefs() == 1) {
      if (!instr.def(0)->is<WireRef>())
        return false;
      if (instr.def(0)->as<WireRef>().getNumUses() == 0)
        return false;
    }

    if (manual(instr))
      return true;
    return generated(ctx, instr);
  }

  void newInstrHook(InstrRef ref) {
    TaggedIRef{ref}.get() = 0;
    for (auto def : ref.defs()) {
      switch (*def->fat().getType()) {
      case *HW_WIRE: {
        auto asWire = def->as<WireRef>();
        for (auto use : asWire.uses())
          worklist.emplace_back(use.instr());
        break;
      }
      default:
        break;
      }
    }
  }

  void runOnProcess(ProcessIRef proc) {
    worklist.clear();
    for (auto instr : HierBlockRange{proc.block()})
      worklist.emplace_back(instr);
    while (!worklist.empty()) {
      auto instr = worklist.pop_back_val();
      if (TaggedIRef{instr}.get())
        continue;
      auto lastWorklistSize = worklist.size();
      DEBUG("instcombine", { dumpInstr(instr); })
      if (!runOnInstr(instr))
        continue;
      DEBUG("instcombine", {
        dbgs() << "replaced with:\n";
        for (size_t i = lastWorklistSize, sz = worklist.size(); i < sz; i++)
          dumpInstr(worklist[i]);
        if (lastWorklistSize == worklist.size())
          dbgs() << "<none>\n";
      })
      for (size_t i = lastWorklistSize, sz = worklist.size(); i < sz; i++)
        newInstrHook(worklist[i]);
    }
  }

  void runOnModule(ModuleIRef mod) {
    for (auto proc : mod.procs())
      runOnProcess(proc);
  }

  void destroyMarkedInstrs() {
    HWInstrBuilder build{ctx};
    for (auto instr : ctx.getInstrs()) {
      if (TaggedIRef{instr}.get())
        build.destroyInstr(instr);
    }
  }

public:
  void run() {
    for (auto instr : ctx.getInstrs())
      TaggedIRef{instr}.get() = 0;

    ctx.getInstrs().createHooks.emplace_back(
        [&](InstrRef ref) { worklist.emplace_back(ref); });

    for (auto mod : ctx.getModules()) {
      runOnModule(mod.iref());
    }

    ctx.getInstrs().createHooks.pop_back();

    destroyMarkedInstrs();
  }

public:
  explicit InstCombinePass(HWContext &ctx)
      : ctx(ctx), cbuild(ctx.constBuild()) {}
};

}; // namespace dyno
