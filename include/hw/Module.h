#pragma once

#include "dyno/CFG.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/IDs.h"
#include "hw/Process.h"
#include "hw/Register.h"
#include "hw/SensList.h"
#include "op/Function.h"
#include "support/SmallVec.h"

namespace dyno {

class Context;

class Module {

public:
  InstrDefUse defUse;
  std::string name;
  bool ignore = false;

  struct Port {
    RegisterRef reg;
    DialectOpcode portType;

    Port(RegisterRef reg, DialectOpcode portType)
        : reg(reg), portType(portType) {}
  };

  SmallVec<Port, 8> ports;

  Module(DynObjRef, std::string name) : name(name) {}
};

class ModuleIRef;

class ModuleRef : public FatObjRef<Module>, public InstrDefUseMixin<ModuleRef> {
public:
  using FatObjRef::FatObjRef;
  ModuleRef(const FatObjRef<Module> ref) : FatObjRef<Module>(ref) {}

  ModuleIRef iref();
};

template <> struct ObjTraits<Module> {
  // static constexpr DialectID dialect{DIALECT_HW};
  static constexpr DialectType ty{HW_MODULE};
  using FatRefT = ModuleRef;
};

class ModuleIRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  ModuleIRef(InstrRef instr) : InstrRef(instr) {}

  ModuleRef mod() { return def(0)->as<ModuleRef>(); }
  BlockRef block() { return def(1)->as<BlockRef>(); }

  BlockRef_iterator<true> regs_end() {
    auto it = block().begin();
    // todo: decent impl via block defrag
    while (it != block().end()) {
      switch (it.instr().getDialectOpcode().raw()) {
      case HW_INPUT_REGISTER_DEF.raw():
      case HW_OUTPUT_REGISTER_DEF.raw():
      case HW_INOUT_REGISTER_DEF.raw():
      case HW_REF_REGISTER_DEF.raw():
      case HW_REGISTER_DEF.raw():
        it++;
        continue;
      default:
        break;
      }
      break;
    }
    return it;
  }

  BlockRef_iterator<true> ports_end() {
    auto it = block().begin();
    while (it != block().end()) {
      switch (it.instr().getDialectOpcode().raw()) {
      case HW_INPUT_REGISTER_DEF.raw():
      case HW_OUTPUT_REGISTER_DEF.raw():
      case HW_INOUT_REGISTER_DEF.raw():
      case HW_REF_REGISTER_DEF.raw():
        it++;
        continue;
      default:
        break;
      }
      break;
    }
    return it;
  }

  auto regs() { return Range{block().begin(), regs_end()}.as<RegisterIRef>(); }
  auto ports() {
    return Range{block().begin(), ports_end()}.as<RegisterIRef>();
  }

  auto procs() {
    return Range{regs_end(), block().end()}
        .filter([](InstrRef instr) {
          return instr.getNumDefs() > 0 && instr.def(0)->is<ProcessRef>();
        })
        .as<ProcessIRef>();
  }
  auto comb_procs() {
    return Range{regs_end(), block().end()}
        .filter([](InstrRef instr) { return instr.isOpc(HW_COMB_PROCESS_DEF); })
        .as<ProcessIRef>();
  }

  auto funcs() {
    return Range{regs_end(), block().end()}
        .filter([](InstrRef instr) { return instr.is<FunctionIRef>(); })
        .as<FunctionIRef>();
  }

  auto triggers() {
    return Range{regs_end(), block().end()}
        .filter([](InstrRef instr) { return instr.is<TriggerIRef>(); })
        .as<TriggerIRef>();
  }

  ProcessIRef getSingleProcess() {
    auto it = block().end();
    if (it == block().begin())
      return nullref;
    auto proc = std::prev(it);
    if (!proc->is<ProcessIRef>())
      return nullref;
    if (proc != block().begin() && std::prev(proc)->is<ProcessIRef>())
      return nullref;
    return proc->as<ProcessIRef>();
  }

  static bool is_impl(FatDynObjRef<> ref) {
    if (!ref.is<InstrRef>())
      return false;
    return ref.as<InstrRef>().isOpc(HW_MODULE_DEF, HW_STDCELL_DEF);
  }

  // todo: signature caching should be more explicit
  void rebuildSignature() {
    mod()->ports.clear();
    for (auto port : ports()) {
      mod()->ports.emplace_back(port.as<RegisterIRef>().oref(),
                                port.getDialectOpcode());
    }
  }
};

inline ModuleIRef ModuleRef::iref() {
  return getSingleDef()->instr().as<ModuleIRef>();
}

}; // namespace dyno
