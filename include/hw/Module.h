#pragma once

#include "dyno/CFG.h"
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

  SmallVec<RegisterRef, 8> ports;

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

  BlockRef_iterator<true> regs_end() {
    auto it = block().begin();
    // todo: decent impl via block defrag
    while (it != block().end()) {
      switch (it.instr().getDialectOpcode().raw()) {
      case HW_INPUT_REGISTER_INSTR.raw():
      case HW_OUTPUT_REGISTER_INSTR.raw():
      case HW_INOUT_REGISTER_INSTR.raw():
      case HW_REF_REGISTER_INSTR.raw():
      case HW_REGISTER_INSTR.raw():
        it++;
        continue;
      default:
      }
      break;
    }
    return it;
  }

  Range<BlockRef_iterator<true>> regs() {
    return Range{block().begin(), regs_end()};
  }
};

inline ModuleIRef ModuleRef::iref() {
  return getSingleDef()->instr().as<ModuleIRef>();
}

}; // namespace dyno
