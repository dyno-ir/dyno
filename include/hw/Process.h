#pragma once

#include "dyno/CFG.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/IDs.h"

namespace dyno {

/*
class Process {
public:
  InstrDefUse defUse;
  Process(DynObjRef) {}
};

using ProcessRef = FatDynObjRef<Process>;

template <> struct ObjTraits<Process> {
  static constexpr DialectID dialect{DIALECT_RTL};
  static constexpr TyID ty{RTL_WIRE};
  using FatRefT = ProcessRef;
};*/


class ProcessInstrRef : InstrRef
{

};

}; // namespace dyno
