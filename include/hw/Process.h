#pragma once

#include "dyno/CFG.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/IDs.h"

namespace dyno {

class Process {
public:
  InstrDefUse defUse;

  Process(DynObjRef) {}
};

class ProcessIRef;

class ProcessRef : public FatObjRef<Process> {
public:
  using FatObjRef<Process>::FatObjRef;
  ProcessRef(const FatObjRef<Process> ref) : FatObjRef<Process>(ref) {}

  ProcessIRef iref() const;
};

template <> struct ObjTraits<Process> {
  // static constexpr DialectID dialect{DIALECT_HW};
  static constexpr DialectType ty{HW_PROCESS};
  using FatRefT = ProcessRef;
};

class ProcessIRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  constexpr ProcessIRef(InstrRef ref) : InstrRef(ref) {}

  ProcessRef proc() const { return def(0)->as<ProcessRef>(); }
  BlockRef block() const { return def(1)->as<BlockRef>(); }

  static bool is_impl(const FatObjRef<Instr> &instr) {
    return InstrRef{instr}.isOpc(HW_COMB_PROCESS_DEF, HW_INIT_PROCESS_DEF,
                                 HW_SEQ_PROCESS_DEF, HW_FINAL_PROCESS_DEF,
                                 HW_LATCH_PROCESS_DEF);
  }
  static bool is_impl(const FatDynObjRef<> &ref) {
    if (auto asInstr = ref.dyn_as<InstrRef>())
      return is_impl(asInstr);
    return false;
  }
};

inline ProcessIRef ProcessRef::iref() const {
  return ptr->defUse.getSingleDef()->instr().as<ProcessIRef>();
}

}; // namespace dyno
