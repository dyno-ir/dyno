#pragma once
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/DefUseMixin.h"
#include "hw/IDs.h"

namespace dyno {

class MemoryPort : public InstrDefUseMixin<MemoryPort> {
public:
  InstrDefUse defUse;
  MemoryPort(DynObjRef) {}
  MemoryPort(DynObjRef, FatObjRef<MemoryPort>) {}
};

template <> struct ObjTraits<MemoryPort> {
  using FatRefT = FatObjRef<MemoryPort>;
  static constexpr DialectType ty{HW_MEM_PORT};
};

} // namespace dyno
