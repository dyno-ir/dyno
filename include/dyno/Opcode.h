#pragma once
#include "dyno/IDs.h"
#include "support/Bits.h"

namespace dyno {

using OpcodeID = IDImpl<uint16_t>;
class DialectOpcode;

template <typename T>
concept IsDialectOpcode = std::is_base_of_v<DialectOpcode, T>;

class DialectOpcode {
public:
  // like this so dialect is more significant when ordering
  OpcodeID opc;
  DialectID dialect;

protected:
  explicit DialectOpcode(uint32_t combined)
      : opc(combined & bit_mask_ones<OpcodeID::num_t>()),
        dialect(combined >> bit_mask_sz<OpcodeID::num_t>) {
    assert(combined <= bit_mask_ones<uint32_t>(bit_mask_sz<OpcodeID::num_t> +
                                               bit_mask_sz<DialectID::num_t>));
  }

public:
  constexpr DialectOpcode(const DialectOpcode &) = default;
  constexpr DialectOpcode(DialectOpcode &&) = default;
  constexpr DialectOpcode &operator=(const DialectOpcode &) = default;
  constexpr DialectOpcode &operator=(DialectOpcode &&) = default;
  constexpr DialectOpcode(DialectID dialect, OpcodeID opc)
      : opc(opc), dialect(dialect) {}
  constexpr DialectOpcode(DialectID::num_t dialect, OpcodeID::num_t opc)
      : opc(opc), dialect(dialect) {}

  constexpr OpcodeID getOpcodeID() const { return opc; }
  constexpr DialectID getDialectID() const { return dialect; }

  constexpr explicit operator uint32_t() const {
    return (dialect.num << bit_mask_sz<OpcodeID::num_t> | opc.num);
  }
  constexpr uint32_t raw() const { return uint32_t(*this); }
  constexpr uint32_t operator*() const { return raw(); }

  constexpr friend auto operator<=>(DialectOpcode lhs, DialectOpcode rhs) {
    return uint32_t(lhs) <=> uint32_t(rhs);
  }
  constexpr friend auto operator==(DialectOpcode lhs, DialectOpcode rhs) {
    return lhs.dialect == rhs.dialect && lhs.opc == rhs.opc;
  }

  constexpr DialectOpcode indexAdd(unsigned i, DialectOpcode max) const {
    DialectOpcode tmp = (*this);
    tmp = DialectOpcode{tmp.raw() + i};
    assert(tmp <= max && "index oob");
    return tmp;
  }

  constexpr DialectOpcode() = default;

  template <IsDialectOpcode... Ts> bool is(Ts... ts) {
    return ((ts == *this) || ...);
  }
};

template <DialectID D> class SpecificDialectOpcode : public DialectOpcode {
public:
  constexpr SpecificDialectOpcode(const SpecificDialectOpcode &) = default;
  constexpr SpecificDialectOpcode(SpecificDialectOpcode &&) = default;
  constexpr SpecificDialectOpcode &
  operator=(const SpecificDialectOpcode &) = default;
  constexpr SpecificDialectOpcode &
  operator=(SpecificDialectOpcode &&) = default;
  constexpr SpecificDialectOpcode(OpcodeID opc) : DialectOpcode(D, opc) {}
  constexpr SpecificDialectOpcode(OpcodeID::num_t opc)
      : DialectOpcode(D.num, opc) {}
  constexpr SpecificDialectOpcode() = default;
};

template <size_t NUM_DIALECTS, typename T> class DialectOpcodeInterface {
  Interfaces<NUM_DIALECTS, std::vector<T>> interfaces;

public:
  constexpr T &operator[](DialectOpcode d) {
    auto &vec =
        interfaces.template getVal<std::vector<T>>(d.getDialectID().num);
    if (vec.size() <= d.getOpcodeID().num)
      vec.resize(d.getOpcodeID().num + 1);
    return vec[d.getOpcodeID().num];
  }

  constexpr void set(DialectOpcode d, const T &t) {
    auto &vec =
        interfaces.template getVal<std::vector<T>>(d.getDialectID().num);
    if (vec.size() <= d.getOpcodeID().num)
      vec.resize(d.getOpcodeID().num + 1);
    vec[d.getOpcodeID().num] = t;
  }
};

}; // namespace dyno
