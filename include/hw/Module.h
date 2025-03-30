#pragma once

#include "dyno/CFG.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/IDs.h"
#include "support/SmallVec.h"

namespace dyno {

class HWContext;

class Module {

public:
  InstrDefUse defUse;
  std::string name;

  Module(DynObjRef, std::string name) : name(name) {}
};

class ModuleRef : public FatObjRef<Module> {
public:
  using FatObjRef<Module>::FatObjRef;
  ModuleRef(const FatObjRef<Module> ref) : FatObjRef<Module>(ref) {}
  InstrRef getModuleInstr() { return ptr->defUse.getSingleDef()->instr(); }

  auto procs() { return ptr->defUse.uses(); }
};

template <> struct ObjTraits<Module> {
  static constexpr DialectID dialect{DIALECT_RTL};
  static constexpr TyID ty{RTL_MODULE};
  using FatRefT = ModuleRef;
};

}; // namespace dyno
