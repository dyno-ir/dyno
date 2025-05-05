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
};

class ProcessRef : public FatObjRef<Process> {
public:
  using FatObjRef<Process>::FatObjRef;
  ProcessRef(const FatObjRef<Process> ref) : FatObjRef<Process>(ref) {}

  // BlockRef block() {
  //   return ptr->defUse.getSingleDef()->instr().def(1)->as<BlockRef>();
  // }
};

template <> struct ObjTraits<Process> {
  static constexpr DialectID dialect{DIALECT_RTL};
  static constexpr TyID ty{RTL_PROCESS};
  using FatRefT = ProcessRef;
};

class ProcessIRef : public InstrRef {
public:
  using InstrRef::InstrRef;

  ProcessRef proc() { return def(0)->as<ProcessRef>(); }
  BlockRef block() { return def(1)->as<BlockRef>(); }
};

}; // namespace dyno
