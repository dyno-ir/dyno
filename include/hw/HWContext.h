#pragma once
#include "dyno/Constant.h"
#include "dyno/IDs.h"
#include "hw/DebugInfo.h"
#include "hw/Module.h"
#include "hw/SensList.h"
#include "hw/Wire.h"

namespace dyno {

class HWContext {

  ConstantStore constants;
  NewDeleteObjStore<Module> modules;
  NewDeleteObjStore<Register> regs;
  NewDeleteObjStore<Wire> wires;
  NewDeleteObjStore<Function> funcs;
  NewDeleteObjStore<Process> procs;
  NewDeleteObjStore<Trigger> triggers;
  CFG cfg;
  NewDeleteObjStore<Instr> instrs;

public:
  auto &getConstants() { return constants; }
  auto &getModules() { return modules; }
  auto &getRegs() { return regs; }
  auto &getWires() { return wires; }
  auto &getFuncs() { return funcs; }
  auto &getProcs() { return procs; }
  auto &getInstrs() { return instrs; }
  auto &getCFG() { return cfg; }
  auto &getTriggers() { return triggers; }
  DebugInfo dbgInfo;

  ModuleIRef createModule(std::string_view name) {
    auto moduleRef = modules.create(std::string(name));
    auto moduleInstr = InstrRef{instrs.create(2, HW_MODULE_INSTR)};

    InstrBuilder{moduleInstr}.addRef(moduleRef).addRef(createBlock());
    return moduleInstr;
  }

  BlockRef createBlock() {
    auto blockRef = cfg.blocks.create(cfg);
    // auto blockInstrRef =
    //     InstrRef{instrs.create(1 + sizeof...(parents),
    //     DialectID{DIALECT_HW},
    //                            OpcodeID{HW_BLOCK_INSTR})};
    // InstrBuilder build{blockInstrRef};
    // build.addRef(blockRef).other();
    //(([&]() { build.addRef(parents); })(), ...);
    return blockRef;
  }

  ConstantBuilder constBuild() { return ConstantBuilder{constants}; }

  FatDynObjRef<> resolveObj(DynObjRef obj) {
    switch (*obj.getType()) {
    case *CORE_INSTR: {
      return getInstrs().resolve(obj.as<ObjRef<Instr>>());
      break;
    }
    case *CORE_BLOCK: {
      return getCFG().blocks.resolve(obj.as<ObjRef<Block>>());
      break;
    }
    case *CORE_CONSTANT: {
      return getConstants().resolve(obj);
    }
    case *OP_FUNC: {
      return getFuncs().resolve(obj.as<ObjRef<Function>>());
      break;
    }
    case *HW_REGISTER: {
      return getRegs().resolve(obj.as<ObjRef<Register>>());
      break;
    }
    case *HW_WIRE: {
      return getWires().resolve(obj.as<ObjRef<Wire>>());
      break;
    }
    case *HW_PROCESS: {
      return getProcs().resolve(obj.as<ObjRef<Process>>());
      break;
    }
    case *HW_TRIGGER: {
      return getTriggers().resolve(obj.as<ObjRef<Trigger>>());
      break;
    }
    case *HW_MODULE: {
      return getModules().resolve(obj.as<ObjRef<Module>>());
      break;
    }
    default:
      dyno_unreachable("resolving unknown object");
    }
  }

  HWContext() {
    instrs.destroyHooks.emplace_back(
        [&](InstrRef instr) { dbgInfo.resetDebugInfo(instr); });
  }
};

}; // namespace dyno
