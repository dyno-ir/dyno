#pragma once

#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/IDs.h"
#include "hw/Register.h"
#include "scf/IDs.h"
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
    case (DIALECT_RTL << 16 | HW_MODULE_INSTR):
      return UC_DEF;
    case (DIALECT_RTL << 16 | HW_PROCESS_INSTR):
      return UC_PROC;
    case (DIALECT_RTL << 16 | HW_REGISTER_INSTR):
      return UC_REG;
    case (DIALECT_SCF << 16 | SCF_FUNC_INSTR):
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

private:
  auto usesOfCategory(Module::UseClass uc) {
    return Range{ptr->defUse.begin() +
                     ((uc == 0) ? 0 : ptr->defUse.catBounds[uc - 1]),
                 ptr->defUse.begin() + ptr->defUse.catBounds[uc]};
  }

public:
  auto procs() {
    return usesOfCategory(Module::UC_PROC);
  }
  auto regs() {
    return usesOfCategory(Module::UC_REG);
  }
  auto funcs() {
    return usesOfCategory(Module::UC_FUNC);
  }

  void addPort(RegisterRef ref, Register::PortType portType) {
    ref.getPtr()->portIndex = ptr->ports.size();
    ref.getPtr()->portType = portType;
    ptr->ports.emplace_back(ref);
  }
};

template <> struct ObjTraits<Module> {
  static constexpr DialectID dialect{DIALECT_RTL};
  static constexpr TyID ty{RTL_MODULE};
  using FatRefT = ModuleRef;
};

}; // namespace dyno
