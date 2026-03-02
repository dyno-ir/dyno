#pragma once
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/DefUseMixin.h"
#include "hw/IDs.h"

namespace dyno {

class MemoryPort : public InstrDefUseMixin<MemoryPort> {
public:
  InstrDefUse defUse;
  uint32_t delay;

  struct WriteForwardMeta {
    uint32_t oldTime;
    uint32_t unkTime;
  };
  SmallVec<WriteForwardMeta, 1> writeForwardMeta;

  MemoryPort(DynObjRef, uint32_t delay = 0) : delay(delay) {}
  MemoryPort(DynObjRef, FatObjRef<MemoryPort> o) : delay(o->delay) {}
};

using MemoryPortRef = FatObjRef<MemoryPort>;

template <> struct ObjTraits<MemoryPort> {
  using FatRefT = MemoryPortRef;
  static constexpr DialectType ty{HW_MEM_PORT};
};

struct MemoryWriteForward {
  MemoryPortRef port;
  uint32_t oldTime;
  uint32_t unkTime;
};

} // namespace dyno
