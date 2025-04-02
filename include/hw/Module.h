#pragma once

#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/IDs.h"
#include "hw/Register.h"
#include "scf/IDs.h"
#include "support/SmallVec.h"

namespace dyno {

class HWContext;

class Module {

public:
  InstrDefUse defUse;
  std::string name;

  // todo: fast ordered (inline linked list) smallvec wrapper?
  SmallVec<FatObjRef<Register>, 8> ports;

  Module(DynObjRef, std::string name) : name(name) {}
};

class ModuleRef : public FatObjRef<Module> {
public:
  using FatObjRef<Module>::FatObjRef;
  ModuleRef(const FatObjRef<Module> ref) : FatObjRef<Module>(ref) {}
  InstrRef getModuleInstr() { return ptr->defUse.getSingleDef()->instr(); }

  // FIXME: stand-in for better solution.
  // maybe just own register in the module & have no register instruction at all?
  auto procs() {
    return ptr->defUse.uses().filter([](OperandRef ref) {
      return ref.instr().getDialect() == DIALECT_RTL && ref.instr().getOpcode() == HW_PROCESS_INSTR;
    });
  }
  auto regs() {
    return ptr->defUse.uses().filter([](OperandRef ref) {
      return ref.instr().getDialect() == DIALECT_RTL && ref.instr().getOpcode() == HW_REGISTER_INSTR;
    });
  }
  auto funcs() {
    return ptr->defUse.uses().filter([](OperandRef ref) {
      return ref.instr().getDialect() == DIALECT_SCF && ref.instr().getOpcode() == SCF_FUNC_INSTR;
    });
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
