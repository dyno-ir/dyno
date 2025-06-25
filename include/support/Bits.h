#pragma once
#include <bit>
#include <cassert>
#include <climits>
#include <concepts>
#include <cstdint>
#include <type_traits>

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

template <typename T> constexpr T bit_mask_ms_nbits(unsigned nbits) {
  return bit_mask_ones<T>(nbits, (bit_mask_sz<T> - nbits));
}

template <typename T> constexpr unsigned clog2(T val) {
  if (val == 0)
    return 0;
  if (val == 1)
    return 1;
  return std::bit_width(val - 1);
  // if (val == 0 || val == 1)
  //   return 0;
  // return bit_mask_sz<T> - std::countl_zero(val - 1);
}

template <typename T> constexpr T round_up_div(T dividend, T divisor) {
  return (dividend + divisor - 1) / divisor;
}

template <typename T>
static constexpr unsigned repeatBits(T x, unsigned xBits) {
  unsigned fact = xBits;
  assert(!(x & bit_mask_zeros<unsigned>(xBits)));
  while (fact != bit_mask_sz<T>) {
    x |= (x << fact);
    fact <<= 1;
  }
  return x;
}

static constexpr uint16_t pack_bits(uint32_t x) {
  x &= 0x55555555;

  x = (x | (x >> 1)) & 0x33333333;
  x = (x | (x >> 2)) & 0x0F0F0F0F;
  x = (x | (x >> 4)) & 0x00FF00FF;
  x = (x | (x >> 8)) & 0x0000FFFF;

  return x;
}

static constexpr uint32_t unpack_bits(uint16_t x) {
  uint32_t xx = x;
  xx = (xx | (xx << 8)) & 0x00FF00FF;
  xx = (xx | (xx << 4)) & 0x0F0F0F0F;
  xx = (xx | (xx << 2)) & 0x33333333;
  xx = (xx | (xx << 1)) & 0x55555555;

  return xx;
}

// split integer into regions of N bits, return 1000... for each region if equal
template <int N, std::integral T>
static constexpr T n_equal_mask(T lhs, T rhs) {
  constexpr T REP01 = repeatBits((T(1) << (N - 1)) - T(1), N);
  lhs ^= rhs;
  T lhsSC = ~(((lhs & REP01) + REP01) | lhs | REP01);
  return lhsSC;
}

// fixme: these should use a shared base but then template param deduction
// fails.
template <std::integral NumT, unsigned N, unsigned Pos> class BitField {
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

  explicit constexpr BitField(num_t &v) : num(v) {}

  constexpr BitField &operator=(num_t v) {
    set(v);
    return *this;
  }

  constexpr BitField &operator+=(num_t v) { return (*this) = (*this) + v; }
  constexpr BitField &operator-=(num_t v) { return (*this) = (*this) - v; }

  constexpr operator num_t() const { return get(); }

  constexpr num_t get() const { return (num & mask_ones) >> pos; }

  constexpr void clr() { num &= mask_zeros; }

  constexpr void set() { num |= mask_ones; }

  constexpr void set(num_t v) {
    assert((v & mask_ones_noshift) == v);
    clr();
    num |= v << pos;
  }

  constexpr void flip() { num ^= mask_ones; }

  constexpr unsigned count() { return std::popcount(num & mask_ones); }
};

template <std::integral NumT, unsigned N, unsigned Pos>
class BitField<const NumT, N, Pos> {
  const NumT &num;

public:
  using num_t = const NumT;
  using num_signed_t = std::make_signed_t<const NumT>;

  static constexpr num_t mask_ones = bit_mask_ones<num_t>(N, Pos);
  static constexpr num_t mask_zeros = bit_mask_zeros<num_t>(N, Pos);

  static constexpr num_t mask_ones_noshift = bit_mask_ones<num_t>(N);
  static constexpr num_t mask_zeros_noshift = bit_mask_zeros<num_t>(N);

  static constexpr unsigned size = N;
  static constexpr unsigned pos = Pos;

  explicit constexpr BitField(num_t &v) : num(v) {}

  constexpr operator num_t() const { return get(); }

  constexpr num_t get() const { return (num & mask_ones) >> pos; }
  constexpr void flip() { num ^= mask_ones; }

  constexpr unsigned count() { return std::popcount(num & mask_ones); }
};
