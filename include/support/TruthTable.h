#pragma once

#include "support/ArrayRef.h"
#include "support/Bits.h"
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <print>

namespace dyno {

template <typename T>
constexpr unsigned tt_increment_plane_num =
    std::bit_width(unsigned(std::numeric_limits<T>::digits)) - 1;

template <typename T> constexpr T tt_increment_plane(unsigned k) {
  unsigned halfPeriod = 1 << k;
  return repeatBits<T>(bit_mask_ones<T>(halfPeriod, halfPeriod),
                       2 * halfPeriod);
}

template <typename T> consteval auto tt_increment_planes_eval() {
  std::array<T, tt_increment_plane_num<T>> arr;
  for (unsigned i = 0; i < arr.size(); ++i) {
    arr[i] = tt_increment_plane<T>(i);
  }
  return arr;
}

template <typename T>
constexpr auto tt_increment_planes = tt_increment_planes_eval<T>();

class TruthTable {
public:
  using truth_t = uint64_t;
  static constexpr unsigned MaxVars = 6;

private:
  truth_t truth;

public:
  static constexpr unsigned size(unsigned numVars) {
    return bit_mask<unsigned>(numVars);
  }
  constexpr TruthTable() : truth(0) {}
  constexpr explicit TruthTable(truth_t t) : truth(t) {}

  constexpr truth_t raw() const { return truth; }
  constexpr truth_t raw(unsigned numVars) const {
    return truth & bit_mask_ones<truth_t>(size(numVars));
  }

  std::string format(unsigned numVars) {
    auto res = std::format("{:0{}b}", truth, size(numVars));
    std::reverse(res.begin(), res.end());
    return res;
  }

  static constexpr TruthTable identity(unsigned var) {
    return TruthTable(tt_increment_planes<truth_t>[var]);
  }

  static constexpr TruthTable zero() { return TruthTable(0); }
  static constexpr TruthTable one() { return TruthTable(~truth_t(0)); }

  constexpr auto term(unsigned i) { return DynBoolField(truth, i); };
  constexpr auto term(unsigned i) const { return DynBoolField(truth, i); };

  TruthTable expandSlow(ArrayRef<uint8_t> varMap, unsigned numNewVars) const {
    assert(numNewVars <= MaxVars);
    assert(varMap.size() < numNewVars);
    TruthTable res;
    unsigned numTerms = size(numNewVars);
    // Calculate each minterm for the expanded TruthTable
    for (unsigned iTerm = 0; iTerm < numTerms; ++iTerm) {
      unsigned oldTerm = 0;
      for (unsigned i = 0; i < varMap.size(); ++i) {
        if (bit_select(iTerm, varMap[i]))
          oldTerm |= bit_mask<unsigned>(i);
      }
      if (term(oldTerm))
        res.term(iTerm).set();
    }
    return res;
  }

  TruthTable expand(ArrayRef<uint8_t> varMap, unsigned numNewVars) const {
    return expandSlow(varMap, numNewVars);
  }

  constexpr TruthTable operator&(TruthTable other) const {
    return TruthTable(truth & other.truth);
  }
  constexpr TruthTable operator|(TruthTable other) const {
    return TruthTable(truth | other.truth);
  }
  constexpr TruthTable operator^(TruthTable other) const {
    return TruthTable(truth ^ other.truth);
  }
  constexpr TruthTable operator~() const { return TruthTable(~truth); }

  constexpr bool operator==(TruthTable other) const {
    return truth == other.truth;
  }
  constexpr bool operator!=(TruthTable other) const {
    return truth != other.truth;
  }

  constexpr TruthTable inverted() const { return ~(*this); }
};

} // namespace dyno
