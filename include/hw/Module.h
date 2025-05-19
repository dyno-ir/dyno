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
  // ModuleRef(const FatObjRef<Module> ref) : FatObjRef<Module>(ref) {}

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
      switch (it.instr().getDialect() << 16 | it.instr().getOpcode()) {
      case DIALECT_HW << 16 | HW_INPUT_REGISTER_INSTR:
      case DIALECT_HW << 16 | HW_OUTPUT_REGISTER_INSTR:
      case DIALECT_HW << 16 | HW_INOUT_REGISTER_INSTR:
      case DIALECT_HW << 16 | HW_REF_REGISTER_INSTR:
      case DIALECT_HW << 16 | HW_REGISTER_INSTR:
        it++;
        continue;
      default:
      }
      break;
    }
    return it;
  }
};

inline ModuleIRef ModuleRef::iref() {
  return getSingleDef()->instr().as<ModuleIRef>();
}

}; // namespace dyno
