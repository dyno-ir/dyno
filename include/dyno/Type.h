#pragma once
#include "dyno/IDImpl.h"
#include "support/Bits.h"
#include <compare>

namespace dyno {

class DialectType;

template <typename T>
concept IsDialectType = std::is_base_of_v<DialectType, T>;

class DialectType {
public:
  // like this so dialect is more significant when ordering
  TyID type;
  DialectID dialect;

protected:
  explicit DialectType(unsigned combined)
      : type(combined & bit_mask_ones<TyID::num_t>()),
        dialect(combined >> bit_mask_sz<TyID::num_t>) {
    assert(combined <= bit_mask_ones<uint16_t>(bit_mask_sz<TyID::num_t> +
                                               bit_mask_sz<DialectID::num_t>));
  }

public:
  constexpr DialectType(const DialectType &) = default;
  constexpr DialectType(DialectType &&) = default;
  constexpr DialectType &operator=(const DialectType &) = default;
  constexpr DialectType &operator=(DialectType &&) = default;
  constexpr DialectType(DialectID dialect, TyID type)
      : type(type), dialect(dialect) {}
  constexpr DialectType(DialectID::num_t dialect, TyID::num_t type)
      : type(type), dialect(dialect) {}

  constexpr TyID getTypeID() const { return type; }
  constexpr DialectID getDialectID() const { return dialect; }

  constexpr explicit operator uint16_t() const {
    return (dialect.num << bit_mask_sz<TyID::num_t> | type.num);
  }
  constexpr uint16_t raw() const { return uint16_t(*this); }
  constexpr uint16_t operator*() const { return raw(); }

  constexpr friend auto operator<=>(DialectType lhs, DialectType rhs) {
    return uint16_t(lhs) <=> uint16_t(rhs);
  }
  constexpr friend auto operator==(DialectType lhs, DialectType rhs) {
    return lhs.dialect == rhs.dialect && lhs.type == rhs.type;
  }

  constexpr DialectType indexAdd(unsigned i, DialectType max) const {
    DialectType tmp = (*this);
    tmp = DialectType{tmp.raw() + i};
    assert(tmp <= max && "index oob");
    return tmp;
  }

  constexpr DialectType() = default;

  template <IsDialectType... Ts> bool is(Ts... ts) {
    return ((ts == *this) || ...);
  }
};

template <DialectID D> class SpecificDialectType : public DialectType {
public:
  constexpr SpecificDialectType(const SpecificDialectType &) = default;
  constexpr SpecificDialectType(SpecificDialectType &&) = default;
  constexpr SpecificDialectType &
  operator=(const SpecificDialectType &) = default;
  constexpr SpecificDialectType &operator=(SpecificDialectType &&) = default;
  constexpr SpecificDialectType(TyID type) : DialectType(D, type) {}
  constexpr SpecificDialectType(TyID::num_t type) : DialectType(D.num, type) {}
  constexpr SpecificDialectType() = default;
};

}; // namespace dyno
