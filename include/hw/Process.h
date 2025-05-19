#pragma once

#include "dyno/CFG.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/IDs.h"
#include "hw/Register.h"

namespace dyno {

struct ProcSenstv {
  enum Mode : uint8_t { POSEDGE, NEGEDGE, ANYEDGE, IFF };
  SmallVec<std::pair<RegisterRef, Mode>, 2> signals;

  static ProcSenstv empty() { return ProcSenstv{}; }
  explicit operator bool() const { return !signals.empty(); }
};

class Process {
public:
  InstrDefUse defUse;
  SmallVec<ProcSenstv::Mode, 2> modes;

  Process(DynObjRef, const ProcSenstv &sens) : modes(sens.signals.size()) {
    // we're storing mode here but register refs in the instr. maybe
    // we can somehow spare a few bits in the DynObjRef?
    // -> multiple register types all mapping to Register
    // -> 2 high bits in obj id
    for (size_t i = 0; i < sens.signals.size(); i++)
      modes[i] = sens.signals[i].second;
  }
};

class ProcessRef : public FatObjRef<Process> {
public:
  using FatObjRef<Process>::FatObjRef;
  ProcessRef(const FatObjRef<Process> ref) : FatObjRef<Process>(ref) {}
};

template <> struct ObjTraits<Process> {
  static constexpr DialectID dialect{DIALECT_HW};
  static constexpr TyID ty{HW_PROCESS};
  using FatRefT = ProcessRef;
};

class ProcessIRef : public InstrRef {
public:
  using InstrRef::InstrRef;

  ProcessRef proc() { return def(0)->as<ProcessRef>(); }
  BlockRef block() { return def(1)->as<BlockRef>(); }
};

}; // namespace dyno
