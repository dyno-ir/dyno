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

public:
  InstrDefUse defUse;
  std::string name;

  SmallVec<FatObjRef<Register>, 8> ports;

  Module(DynObjRef, std::string name) : name(name) {}
};

class ModuleRef : public FatObjRef<Module>, public InstrDefUseMixin<ModuleRef> {
public:
  using FatObjRef::FatObjRef;
  //ModuleRef(const FatObjRef<Module> ref) : FatObjRef<Module>(ref) {}
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
};

}; // namespace dyno
