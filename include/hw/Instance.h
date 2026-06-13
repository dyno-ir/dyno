#pragma once

#include "dyno/Instr.h"
#include "dyno/MutInstr.h"
#include "dyno/Obj.h"
#include "dyno/TypeHint.h"
#include "hw/HWInstr.h"
#include "hw/Module.h"
#include "op/StringObj.h"
namespace dyno {

class InstanceIRef : public OpcodeInstrRef<HWInstrRef, HW_INSTANCE> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  auto ports() { return others().subrange(1).as<RegisterRef>(); }
  ModuleRef mod() { return other(0)->as<ModuleRef>(); }
  StringRef name() {
    return getNumDefs() == 0 ? StringRef::emptyRef()
                             : StringRef(def().as<StringObjRef>()->data);
  }
  std::string *nameRaw() {
    return getNumDefs() == 0 ? nullptr : &def().as<StringObjRef>()->data;
  }
};

class InstanceMutInstr : public MutInstr<FatDynObjRef<>> {
public:
  auto ports() { return others().subrange(1).cast<TypeHint<StringObjRef>>(); }
  TypeHint<ModuleRef> mod() { return TypeHint<ModuleRef>{other(0)}; }
  TypeHint<StringObjRef> name() {
    assert(getNumDefs() != 0 &&
           "name operand must be reserved at construct time");
    return TypeHint<StringObjRef>{def()};
  }

  InstanceMutInstr(Context &ctx, uint32_t numPorts, StringObjRef name = nullref)
      : MutInstr(ctx, HW_INSTANCE, !!name, 1 + numPorts) {
    if (name)
      this->name() = name;
  }
};

}; // namespace dyno
