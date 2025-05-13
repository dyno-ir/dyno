#pragma once

#include "dyno/DialectInfo.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/IDs.h"
#include "hw/Process.h"
#include "hw/Register.h"
#include "op/Function.h"
#include "op/IDs.h"
#include "support/SmallVec.h"
#include "support/Utility.h"

namespace dyno {

class HWContext;

class Module {

  // category order in this enum is maintained in defUse via manual hooking.
public:
  enum UseClass { UC_DEF, UC_REG, UC_PROC, UC_FUNC, UC_COUNT };

private:
  static uint classifyUse(OperandRef ref) {

    auto instrRef = ref.instr();

    uint32_t type = (instrRef.getDialect() << 16) | instrRef.getOpcode();
    switch (type) {
    case (DIALECT_HW << 16 | HW_MODULE_INSTR):
      return UC_DEF;
    case (DIALECT_HW << 16 | HW_PROCESS_INSTR):
      return UC_PROC;
    case (DIALECT_HW << 16 | HW_REGISTER_INSTR):
      return UC_REG;
    case (DIALECT_OP << 16 | OP_FUNC_INSTR):
      return UC_FUNC;
    default:
      dyno_unreachable("type cannot use module");
    }
  }

public:
  CategoricalDefUse<InstrDefUse, UC_COUNT, classifyUse> defUse;
  std::string name;

  SmallVec<FatObjRef<Register>, 8> ports;

  Module(DynObjRef, std::string name) : name(name) {}
};

class ModuleRef : public FatObjRef<Module> {
public:
  using FatObjRef<Module>::FatObjRef;
  ModuleRef(const FatObjRef<Module> ref) : FatObjRef<Module>(ref) {}
};

template <> struct ObjTraits<Module> {
  static constexpr DialectID dialect{DIALECT_HW};
  static constexpr TyID ty{HW_MODULE};
  using FatRefT = ModuleRef;
};

class ModuleIRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  ModuleIRef(InstrRef instr) : InstrRef(instr) {}

  ModuleRef mod() { return def(0)->as<ModuleRef>(); }
  BlockRef block() { return def(1)->as<BlockRef>(); }

  void addPort(RegisterRef ref, Register::PortType portType) {
    ref.getPtr()->portIndex = mod()->ports.size();
    ref.getPtr()->portType = portType;
    mod()->ports.emplace_back(ref);
  }

  auto procs() {
    return mod()
        ->defUse.usesOfCategory(Module::UC_PROC)
        .transform([](size_t i, const OperandRef &OpRef) {
          // this kind of cast should maybe check dialect/opcode.
          return OpRef.instr().as<ProcessIRef>();
        });
  }
  auto regs() {
    return mod()
        ->defUse.usesOfCategory(Module::UC_REG)
        .transform([](size_t i, const OperandRef &OpRef) {
          return OpRef.instr().def()->as<RegisterRef>();
        });
  }
  auto funcs() {
    return mod()
        ->defUse.usesOfCategory(Module::UC_FUNC)
        .transform([](size_t i, const OperandRef &OpRef) {
          return FuncInstrRef{OpRef.instr()};
        });
    ;
  }
};

}; // namespace dyno
