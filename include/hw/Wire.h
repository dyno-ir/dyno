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
  Wire(DynObjRef, std::optional<uint32_t> bitSize = std::nullopt)
      : bitSize(bitSize) {}
};

class WireRef : public FatObjRef<Wire>, public InstrDefUseMixin<WireRef> {
public:
  using FatObjRef<Wire>::FatObjRef;
  WireRef(FatObjRef<Wire> ref) : FatObjRef<Wire>(ref) {}

  std::optional<uint32_t> getBitSize() { return ptr->bitSize; }
  void setBitSize(uint32_t bitSize) { ptr->bitSize = bitSize; }

  auto getDefI() { return getDef().instr(); }
};

template <> struct ObjTraits<Wire> {
  static constexpr DialectID dialect{DIALECT_HW};
  static constexpr TyID ty{HW_WIRE};
  using FatRefT = WireRef;
};

} // namespace dyno
