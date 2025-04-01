#pragma once

#include <bit>
#include <cassert>
#include <climits>
#include <concepts>
#include <cstdint>
#include <type_traits>
#include <bit>

template <std::unsigned_integral T>
constexpr unsigned bit_mask_sz = sizeof(T) * CHAR_BIT;

template <typename T> constexpr T bit_mask(unsigned pos = 0) {
  assert(pos < bit_mask_sz<T>);
  return T(1) << pos;
}

template <typename T> constexpr T bit_mask_msb(unsigned pos = 0) {
  assert(pos < bit_mask_sz<T>);
  return T(1) << (bit_mask_sz<T> - 1 - pos);
}

template <typename T>
constexpr T bit_mask_ones(unsigned n = bit_mask_sz<T>, unsigned pos = 0) {
  assert(pos + n <= bit_mask_sz<T>);
  if (n == bit_mask_sz<T>) {
    return ~T(0);
  }
  return ((T(1) << n) - T(1)) << pos;
}

template <typename T> constexpr T bit_mask_zeros(unsigned n, unsigned pos = 0) {
  return ~bit_mask_ones<T>(n, pos);
}


template <typename T> constexpr T bit_mask_nbits(unsigned nbits)
{
  return (T(1) << nbits) - 1;
}


template <typename T> constexpr unsigned clog2(T val) {
  if (val == 0) return 0;
  return bit_mask_sz<T> - std::countl_zero(val - 1);

}

template <typename NumT, unsigned N, unsigned Pos> class BitField {
  NumT &num;

public:
  using num_t = NumT;
  using num_signed_t = std::make_signed_t<NumT>;

  static constexpr num_t mask_ones = bit_mask_ones<num_t>(N, Pos);
  static constexpr num_t mask_zeros = bit_mask_zeros<num_t>(N, Pos);

  static constexpr num_t mask_ones_noshift = bit_mask_ones<num_t>(N);
  static constexpr num_t mask_zeros_noshift = bit_mask_zeros<num_t>(N);

  static constexpr unsigned size = N;
  static constexpr unsigned pos = Pos;

  explicit BitField(num_t &v) : num(v) {}

  BitField &operator=(num_t v) {
    set(v);
    return *this;
  }

  operator num_t() { return get(); }

  num_t get() { return (num & mask_ones) >> pos; }

  void clr() { num &= mask_zeros; }

  void set() { num |= mask_ones; }

  void set(num_t v) {
    assert((v & mask_ones_noshift) == v);
    clr();
    num |= v << pos;
  }

  void flip() { num ^= mask_ones; }

  unsigned count() { return std::popcount(num & mask_ones); }
};
