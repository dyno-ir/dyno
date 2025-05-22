#pragma once

#include "dyno/Constant.h"
#include "dyno/RefUnion.h"
#include "hw/IDs.h"
#include "hw/Register.h"
#include "hw/Wire.h"
#include <cstdint>
#include <type_traits>

namespace dyno {

class HWValue : public FatRefUnion<WireRef, ConstantRef> {
public:
  using FatRefUnion::FatRefUnion;

  Optional<uint32_t> getNumBits() const {
    switch (getDialectID() << 8 | getTyID()) {
    case (DIALECT_CORE << 8) | CORE_CONSTANT:
      return this->as<ConstantRef>().getNumBits();
    case (DIALECT_HW << 8) | HW_WIRE:
      return this->as<WireRef>().getNumBits();
    default:
      dyno_unreachable("invalid value");
    }
  }

  using FatRefUnion::operator=;
};

template <typename T>
concept IsAnyHWValue =
    std::is_same_v<HWValue, T> || std::is_same_v<WireRef, T> ||
    std::is_same_v<ConstantRef, T>;

template <typename T>
concept IsHWValue = std::is_same_v<HWValue, T>;

// in particular for function calls, we need to pass regs for ref/output args
class HWValueOrReg : public FatRefUnion<WireRef, ConstantRef, RegisterRef> {
public:
  using FatRefUnion::FatRefUnion;

  Optional<uint32_t> getNumBits() const {
    switch (getDialectID() << 8 | getTyID()) {
    case (DIALECT_CORE << 8) | CORE_CONSTANT:
      return this->as<ConstantRef>().getNumBits();
    case (DIALECT_HW << 8) | HW_WIRE:
      return this->as<WireRef>().getNumBits();
    case (DIALECT_HW << 8) | HW_REGISTER:
      return this->as<RegisterRef>().getNumBits();
    default:
      dyno_unreachable("invalid value");
    }
  }
  using FatRefUnion::operator=;
};

template <typename T>
concept IsHWValueOrReg = std::is_same_v<HWValueOrReg, T>;

}; // namespace dyno
