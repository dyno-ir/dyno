#pragma once

#include "aig/AIG.h"
#include "aig/IDs.h"
#include "dyno/ObjMap.h"
#include "hw/FlipFlop.h"
#include "hw/HWAbstraction.h"
#include "hw/HWContext.h"
#include "hw/HWInstr.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/LoadStore.h"
#include "hw/Register.h"
#include "hw/SensList.h"
#include "op/IDs.h"
#include "op/StructuredControlFlow.h"
#include "support/Utility.h"
namespace dyno {

// todo: function support
class AggressiveDeadCodeEliminationPass {
  HWContext &ctx;
  HWInstrBuilder build;

  // for objects with 1:1 mapping to instrs we don't need a separate map.
  ObjMapVec<Instr, bool> instrMap;
  ObjMapVec<Wire, bool> wireMap;
  ObjMapVec<Constant, bool> constantMap;

  SmallVec<InstrRef, 256> worklist;

  void visitRegister(RegisterIRef reg) {
    for (auto use : reg.oref().uses()) {
      auto instr = use.instr();
      // none of our sources want to re-inspect so this is fine.
      if (instrMap[instr])
        continue;

      switch (*instr.getDialectOpcode()) {
      case *HW_STORE_DEFER:
      case *HW_STORE:
        worklist.emplace_back(instr);
        break;

      case *HW_INSTANCE: {
        ModuleRef mod = instr.other(0)->as<ModuleRef>();
        uint idx = (use - instr.other_begin()) - 1;
        if (mod->ports[idx].portType == HW_INPUT_REGISTER_DEF)
          break;
        worklist.emplace_back(instr);
        break;
      }

      case *HW_LOAD:
        break;

      case *HW_TRIGGER_DEF:
        break;

      case *HW_FLIP_FLOP: {
        if (use - instr.begin() != 3)
          break;
        worklist.emplace_back(instr);
        break;
      }

      default:
        dyno_unreachable("unknown access instr");
      }
    }
  }

  void visitHWValue(HWValue value) {
    if (auto asConst = value.dyn_as<ConstantRef>()) {
      if (asConst.isInline())
        return;
      constantMap[asConst] = 1;
    } else {
      auto asWire = value.as<WireRef>();
      for (auto def : asWire.defs()) {
        worklist.emplace_back(def.instr());
      }
      wireMap[asWire] = 1;
    }
  }

  void markParentProc(InstrRef instr) {
    auto block = HWInstrRef{instr}.parentBlock(ctx);
    if (block.defI().isOpc(HW_INIT_PROCESS_DEF, HW_COMB_PROCESS_DEF,
                           HW_SEQ_PROCESS_DEF, HW_LATCH_PROCESS_DEF,
                           HW_FINAL_PROCESS_DEF)) {
      instrMap[block.defI()] = 1;
    }
  }

  void visitInstr(InstrRef instr) {
    markParentProc(instr);
    switch (*instr.getDialectOpcode()) {
    case *HW_REGISTER_DEF:
    case *HW_INPUT_REGISTER_DEF:
    case *HW_OUTPUT_REGISTER_DEF:
    case *HW_INOUT_REGISTER_DEF:
    case *HW_REF_REGISTER_DEF: {
      visitRegister(instr.as<RegisterIRef>());
      break;
    }

    case *OP_IF: {
      if (instrMap[instr])
        break;
      auto asIf = instr.as<IfInstrRef>();
      // if (!instrMap[instr])
      visitHWValue(asIf.getCondValue()->as<WireRef>());
      InstrRef yieldInstrT = asIf.getInnerYieldTrue();
      InstrRef yieldInstrF = asIf.getInnerYieldFalse();
      for (uint i = 0; i < asIf.getNumYieldValues(); i++) {
        // if (!wireMap[asIf.getYieldValue(i)->as<WireRef>()])
        //   continue;
        wireMap[asIf.getYieldValue(i)->as<WireRef>()] = 1;

        if (yieldInstrT)
          visitHWValue(yieldInstrT.operand(i)->as<HWValue>());
        if (yieldInstrF)
          visitHWValue(yieldInstrF.operand(i)->as<HWValue>());
      }
      instrMap[yieldInstrT] = 1;
      instrMap[yieldInstrF] = 1;
      break;
    }

    case *OP_SWITCH: {
      auto asSwitch = instr.as<SwitchInstrRef>();
      if (!instrMap[instr])
        visitHWValue(asSwitch.cond()->as<WireRef>());

      for (uint i = 0; i < asSwitch.getNumYieldValues(); i++) {
        if (!wireMap[asSwitch.getYieldValue(i)->as<WireRef>()])
          continue;
        for (auto yieldInstr : asSwitch.caseYields()) {
          visitHWValue(yieldInstr.operand(i)->as<HWValue>());
        }
      }
      for (auto yieldInstr : asSwitch.caseYields())
        instrMap[yieldInstr] = 1;
      for (auto instr : asSwitch.block()) {
        for (auto use : instr.others())
          visitHWValue(use->as<HWValue>());
        instrMap[instr] = 1;
      }
      break;
    }

    case *OP_WHILE: {
      if (instrMap[instr])
        break;
      auto asWhileLoop = instr.as<WhileInstrRef>();
      for (auto input : asWhileLoop.inputValues())
        visitHWValue(input->as<HWValue>());
      for (auto yield : asWhileLoop.yieldValues())
        wireMap[yield->as<WireRef>()] = 1;
      worklist.emplace_back(asWhileLoop.getCondYield());
      worklist.emplace_back(asWhileLoop.getCondUnyield());
      worklist.emplace_back(asWhileLoop.getBodyYield());
      worklist.emplace_back(asWhileLoop.getBodyUnyield());
      break;
    }

    case *OP_DO_WHILE: {
      if (instrMap[instr])
        break;
      auto asDoWhile = instr.as<DoWhileInstrRef>();
      for (auto input : asDoWhile.inputValues())
        visitHWValue(input->as<HWValue>());
      for (auto yield : asDoWhile.yieldValues())
        wireMap[yield->as<WireRef>()] = 1;
      worklist.emplace_back(asDoWhile.getYield());
      worklist.emplace_back(asDoWhile.getUnyield());
      break;
    }

    case *OP_FOR: {
      if (instrMap[instr])
        break;
      auto asFor = instr.as<ForInstrRef>();
      for (auto input : asFor.inputValues())
        visitHWValue(input->as<HWValue>());
      for (auto yield : asFor.yieldValues())
        wireMap[yield->as<WireRef>()] = 1;
      visitHWValue(asFor.getLower()->as<HWValue>());
      visitHWValue(asFor.getUpper()->as<HWValue>());
      visitHWValue(asFor.getStep()->as<HWValue>());
      worklist.emplace_back(asFor.getYield());
      worklist.emplace_back(asFor.getUnyield());
      break;
    }

    case *HW_LOAD: {
      if (instrMap[instr])
        break;
      auto asLoad = instr.as<LoadIRef>();
      auto reg = asLoad.reg().iref();
      if (!instrMap[reg])
        worklist.emplace_back(reg);
      if (auto range = asLoad.range()) {
        visitHWValue(range->getAddr());
        visitHWValue(range->getLen());
      }
      break;
    }

    case *HW_STORE_DEFER:
    case *HW_STORE: {
      if (instrMap[instr])
        break;
      auto asStore = instr.as<StoreIRef>();
      visitHWValue(asStore.value());
      auto reg = asStore.reg().iref();
      if (!instrMap[reg])
        worklist.emplace_back(reg);
      if (auto range = asStore.range()) {
        visitHWValue(range->getAddr());
        visitHWValue(range->getLen());
      }
      break;
    }

    case *HW_INSTANCE: {
      if (instrMap[instr])
        break;
      for (auto op : instr.others().drop_front()) {
        auto reg = op->as<RegisterRef>().iref();
        if (!instrMap[reg])
          worklist.emplace_back(reg);
      }
      break;
    }

    case *OP_UNYIELD: {
      // we don't do fine-grained deletion of single yield values here,
      // so using one means all are alive.
      for (auto def : instr.defs())
        wireMap[def->as<WireRef>()] = 1;
      break;
    }

    case *HW_TRIGGER_DEF: {
      auto asTrigger = instr.as<TriggerIRef>();
      for (auto reg : asTrigger.others()) {
        auto iref = reg->as<RegisterRef>().iref();
        if (instrMap[iref])
          continue;
        worklist.emplace_back(iref);
      }
      break;
    }

    case *HW_ADD_COMPRESS:
    case *HW_ADD_CARRY: {
      if (instrMap[instr])
        break;
      for (auto def : instr.defs())
        wireMap[def->as<WireRef>()] = 1;
      for (auto use : instr.others()) {
        visitHWValue(use->as<HWValue>());
      }
      break;
    }

    case *AIG_OUTPUT: {
      worklist.emplace_back(
          instr.other(0)->as<AIGObjRef>()->defUse.getSingleDef()->instr());
      break;
    }

    case *AIG_INPUT: {
      visitHWValue(instr.other(0)->as<HWValue>());
      break;
    }

    case *AIG_GRAPH: {
      if (instrMap[instr])
        break;
      auto asAIGObj = instr.def(0)->as<AIGObjRef>();
      // AIG depends on all its inputs
      for (auto use : asAIGObj->defUse.uses())
        if (use.instr().isOpc(AIG_INPUT))
          worklist.emplace_back(use.instr());
      break;
    }

    case *HW_STDCELL_INSTANCE: {
      if (instrMap[instr])
        break;
      for (auto def : instr.defs())
        visitHWValue(def->as<HWValue>());
      for (auto use : instr.others().drop_front()) {
        visitHWValue(use->as<HWValue>());
      }
      break;
    }

    case *HW_FLIP_FLOP: {
      if (instrMap[instr])
        break;
      auto asFF = instr.as<FlipFlopIRef>();
      worklist.emplace_back(asFF.clk().iref());
      worklist.emplace_back(asFF.d().iref());
      if (asFF.hasClkEn()) {
        worklist.emplace_back(asFF.clkEn().iref());
      }
      if (asFF.hasRst()) {
        worklist.emplace_back(asFF.rst().iref());
        visitHWValue(asFF.rstVal());
      }
      break;
    }

    default: {
      if (instrMap[instr])
        break;
      assert(instr.getNumDefs() <= 1);
      for (auto use : instr.others()) {
        visitHWValue(use->as<HWValue>());
      }
      break;
    }
    }
    instrMap[instr] = 1;
  }

  void initialWorklist(ModuleIRef module) {
    // initially, only i/os are live
    for (auto instr : module.block()) {
      if (instr.isOpc(HW_INPUT_REGISTER_DEF, HW_OUTPUT_REGISTER_DEF,
                      HW_INOUT_REGISTER_DEF, HW_REF_REGISTER_DEF)) {
        worklist.emplace_back(instr);
        continue;
      }
      // if (instr.isOpc(HW_INSTANCE)) {
      //   worklist.emplace_back(instr);
      // }
    }

    for (auto instr : module.triggers()) {
      worklist.emplace_back(instr);
    }
  }

  void destroyDeadInstrs() {
    ObjMapVec<Block, bool> blockDestroyMap;
    blockDestroyMap.resize(ctx.getCFG().blocks.numIDs());

    for (auto [obj, live] : instrMap) {
      if (!ctx.getInstrs().exists(obj) || live)
        continue;
      auto instr = ctx.getInstrs().resolve(obj);

      switch (*instr.getDialectOpcode()) {
      case *HW_INIT_PROCESS_DEF:
      case *HW_COMB_PROCESS_DEF:
      case *HW_SEQ_PROCESS_DEF:
      case *HW_LATCH_PROCESS_DEF:
      case *HW_FINAL_PROCESS_DEF: {
        blockDestroyMap[instr.as<ProcessIRef>().block()] = 1;
        break;
      }
      case *OP_IF: {
        blockDestroyMap[instr.as<IfInstrRef>().getTrueBlock()] = 1;
        if (instr.as<IfInstrRef>().hasFalseBlock())
          blockDestroyMap[instr.as<IfInstrRef>().getFalseBlock()] = 1;
        break;
      }
      case *OP_WHILE: {
        blockDestroyMap[instr.as<WhileInstrRef>().getCondBlock()] = 1;
        blockDestroyMap[instr.as<WhileInstrRef>().getBodyBlock()] = 1;
        break;
      }
      case *OP_DO_WHILE: {
        blockDestroyMap[instr.as<DoWhileInstrRef>().getBlock()] = 1;
        break;
      }
      case *OP_FOR: {
        blockDestroyMap[instr.as<ForInstrRef>().getBlock()] = 1;
        break;
      }
      case *OP_CASE:
      case *OP_CASE_DEFAULT: {
        blockDestroyMap[instr.as<CaseInstrRef>().block()] = 1;
        break;
      }
      case *OP_SWITCH: {
        blockDestroyMap[instr.as<SwitchInstrRef>().block()] = 1;
        break;
      }
      }

      if (ctx.getCFG().contains(instr))
        ctx.getCFG()[instr].erase();
      instr->destroyOperands();
      ctx.getInstrs().destroy(instr);
    }

    for (auto [obj, destroy] : blockDestroyMap) {
      if (!ctx.getCFG().blocks.exists(obj) || !destroy)
        continue;
      auto block = ctx.getCFG().blocks.resolve(obj);
      ctx.getCFG().blocks.destroy(block);
    }
  }

  void destroyDeadWires() {
    for (auto [obj, live] : wireMap) {
      if (!ctx.getWires().exists(obj) || live)
        continue;
      auto wire = ctx.getWires().resolve(obj);
      ctx.getWires().destroy(wire);
    }
  }

  void destroyDeadConstants() {
    for (auto [obj, live] : constantMap) {
      if (!ctx.getConstants().exists(obj) || live)
        continue;
      auto constant = ctx.getConstants().resolve(obj);
      ctx.getConstants().destroy(constant);
    }
  }

  void runOnModule(ModuleIRef module) {
    instrMap[module] = 1;

    initialWorklist(module);
    while (!worklist.empty()) {
      visitInstr(worklist.pop_back_val());
    }
  }

public:
  void run() {
    instrMap.clear();
    wireMap.clear();
    constantMap.clear();

    instrMap.resize(ctx.getInstrs().numIDs());
    wireMap.resize(ctx.getWires().numIDs());
    constantMap.resize(ctx.getConstants().numIDs());
    for (auto mod : ctx.getModules()) {
      runOnModule(mod.iref());
    }
    destroyDeadInstrs();
    destroyDeadWires();
    destroyDeadConstants();
  }
  AggressiveDeadCodeEliminationPass(HWContext &ctx) : ctx(ctx), build(ctx) {}
};
}; // namespace dyno
