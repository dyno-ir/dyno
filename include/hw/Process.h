#pragma once

#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/IDs.h"

namespace dyno {

class Process {
public:
  GenericDefUse defUse;
  // todo: add stuff like edge-triggered, comb, ...
  Process(DynObjRef) {}
};

class ProcessRef : public FatObjRef<Process> {
public:
  using FatObjRef<Process>::FatObjRef;
  ProcessRef(const FatObjRef<Process> ref) : FatObjRef<Process>(ref) {}

  auto blocks() { return ptr->defUse.uses(); }
};

template <> struct ObjTraits<Process> {
  static constexpr DialectID dialect{DIALECT_RTL};
  static constexpr TyID ty{RTL_PROCESS};
  using FatRefT = ProcessRef;
};

}; // namespace dyno
