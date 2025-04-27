#pragma once

#include <dyno/Instr.h>
#include <dyno/Obj.h>
#include <hw/DefUseMixin.h>
#include <hw/IDs.h>
#include <optional>

namespace dyno {
class Wire {
public:
  InstrDefUse defUse;
  std::optional<uint32_t> bitSize;

  Wire(DynObjRef) {}
};

class WireRef : public FatObjRef<Wire>, public InstrDefUseMixin<WireRef> {
public:
  using FatObjRef<Wire>::FatObjRef;
  WireRef(FatObjRef<Wire> ref) : FatObjRef<Wire>(ref) {}

  std::optional<uint32_t> getBitSize() {
    return ptr->bitSize;
  }

  auto getDefI() { return getDef().instr(); }
};

template <> struct ObjTraits<Wire> {
  static constexpr DialectID dialect{DIALECT_RTL};
  static constexpr TyID ty{RTL_WIRE};
  using FatRefT = WireRef;
};

} // namespace dyno
