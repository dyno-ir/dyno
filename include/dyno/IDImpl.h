#pragma once

#include "support/Bits.h"
#include <cstdint>

namespace dyno {

template <typename NumT> class IDImpl {
public:
  static constexpr IDImpl invalid() { return IDImpl{bit_mask_ones<NumT>()}; }
  static const inline IDImpl INVALID = invalid();

  using num_t = NumT;
  num_t num;

  constexpr IDImpl() = default;
  constexpr explicit IDImpl(num_t num) : num(num) {}
  constexpr operator num_t() const { return num; }
  constexpr explicit operator bool() const { return *this != invalid(); }
  constexpr bool operator==(IDImpl o) const { return num == o.num; }

  template <typename... T> bool anyOf(T... ids) {
    for (auto id : {ids...}) {
      if (id == num)
        return true;
    }
    return false;
  }
};

using DialectID = IDImpl<uint8_t>;
using TyID = IDImpl<uint8_t>;
using ObjID = IDImpl<uint32_t>;
using InterfaceID = IDImpl<uint16_t>;

const inline TyID::num_t TY_DEF_USE_START = bit_mask_msb<TyID::num_t>();

}; // namespace dyno
