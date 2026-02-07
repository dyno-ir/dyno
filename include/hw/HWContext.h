#pragma once
#include "aig/AIG.h"
#include "dyno/Constant.h"
#include "dyno/Context.h"
#include "dyno/DialectInfo.h"
#include "dyno/FixedFlatObjStore.h"
#include "dyno/IDs.h"
#include "dyno/Interface.h"
#include "dyno/NewDeleteObjStore.h"
#include "dyno/Obj.h"
#include "hw/DebugInfo.h"
#include "hw/HWValue.h"
#include "hw/IDs.h"
#include "hw/Module.h"
#include "hw/Process.h"
#include "hw/SensList.h"
#include "hw/StdCellInfo.h"
#include "hw/Wire.h"
#include "op/MapObj.h"
#include "support/CallableRef.h"
#include "support/TemplateUtil.h"
#include <array>
namespace dyno {

class HWDialectContext : public ContextMixin<HWDialectContext> {
public:
  template <typename T> struct StoreType {
    using t = NewDeleteObjStore<T>;
  };
  template <typename T> using StoreType_t = StoreType<T>::t;

  static constexpr DialectID dialect{DIALECT_HW};

  std::tuple<StoreType_t<Wire>, StoreType_t<Register>, StoreType_t<Process>,
             StoreType_t<Module>, StoreType_t<Trigger>,
             StoreType_t<StdCellInfo>>
      stores;

  ValueNameInfo<Register> regNameInfo;
  template <typename T> T &get() { return std::get<T>(stores); }

  template <typename T> StoreType_t<T> &getStore() {
    return get<StoreType_t<T>>();
  }

  auto activeModules() {
    return Range{getStore<Module>()}.filter(
        [](ModuleRef ref) { return !ref->ignore; });
  }

  HWDialectContext() {
    getStore<Register>().destroyHooks.emplace_back(
        [&](RegisterRef ref) { regNameInfo.clearNames(ref); });
  }

  void reset() {
    ContextMixin::reset();
    regNameInfo.reset();
  }
};

template <> struct DialectContext<DialectID{DIALECT_HW}> {
  using t = HWDialectContext;
};

class HWContext {

  ConstantStore constants;
  NewDeleteObjStore<Module> modules;
  NewDeleteObjStore<Register> regs;
  FixedFlatObjStore<Wire> wires;
  NewDeleteObjStore<Function> funcs;
  NewDeleteObjStore<Process> procs;
  NewDeleteObjStore<Trigger> triggers;
  CFG cfg;
  NewDeleteObjStore<Instr> instrs;
  NewDeleteObjStore<AIGObj> aigObjs;
  NewDeleteObjStore<StdCellInfo> stdCellInfos;
  NewDeleteObjStore<MapObj> mapObjs;

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
  auto &getAIGs() { return aigObjs; }
  auto &getStdCellInfos() { return stdCellInfos; }
  auto &getMaps() { return mapObjs; }
  SourceLocInfo<Instr> sourceLocInfo;
  ValueNameInfo<Register> regNameInfo;

  ModuleIRef createModule(std::string_view name,
                          DialectOpcode defOpc = HW_MODULE_DEF) {
    auto moduleRef = modules.create(std::string(name));
    auto moduleInstr = InstrRef{instrs.create(2, defOpc)};

    InstrBuilder{moduleInstr}.addRef(moduleRef).addRef(createBlock());
    return moduleInstr;
  }

  ModuleIRef createStdCell(std::string_view name, StdCellInfoRef info) {
    auto moduleRef = modules.create(std::string(name));
    auto moduleInstr = InstrRef{instrs.create(3, HW_STDCELL_DEF)};

    InstrBuilder{moduleInstr}
        .addRef(moduleRef)
        .addRef(createBlock())
        .addRef(info);
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

  auto activeModules() {
    return Range{modules}.filter([](ModuleRef ref) { return !ref->ignore; });
  }

  HWContext() {
    instrs.destroyHooks.emplace_back(
        [&](InstrRef instr) { sourceLocInfo.resetDebugInfo(instr); });
    regs.destroyHooks.emplace_back(
        [&](RegisterRef ref) { regNameInfo.clearNames(ref); });
  }
};

}; // namespace dyno
