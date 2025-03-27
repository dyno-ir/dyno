#pragma once

#include "dyno/CFG.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/IDs.h"

namespace dyno {

class Process {
public:
  InstrDefUse defUse;
  // todo: add stuff like edge-triggered, comb, ...
  Process(DynObjRef) {}

  auto blocks() { return defUse.uses(); }
};

using ProcessRef = FatObjRef<Process>;

template <> struct ObjTraits<Process> {
  static constexpr DialectID dialect{DIALECT_RTL};
  static constexpr TyID ty{RTL_PROCESS};
  using FatRefT = ProcessRef;
};

}; // namespace dyno
