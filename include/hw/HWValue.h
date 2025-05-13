#pragma once

#include "dyno/Constant.h"
#include "dyno/RefUnion.h"
#include "hw/Wire.h"
#include <cstdint>
#include <optional>
#include <type_traits>

namespace dyno {

class HWValue : public FatRefUnion<WireRef, ConstantRef> {
public:
  using FatRefUnion::FatRefUnion;

  std::optional<uint32_t> numBits() const {
    switch (getDialectID() << 8 | getTyID()) {
    case (DIALECT_CORE << 8) | CORE_CONSTANT:
      return this->as<ConstantRef>().getNumBits();
    case (DIALECT_HW << 8) | HW_WIRE:
      return this->as<WireRef>().getBitSize();
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
concept IsHWValue =
    std::is_same_v<HWValue, T>;

}; // namespace dyno
