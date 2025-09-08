#pragma once
#include "support/ErrorRecovery.h"
#include <bit>
#include <cassert>
#include <climits>
#include <concepts>
#include <cstddef>
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

template <typename T> constexpr T sign_extend(T num, unsigned bits) {
  assert(bits != 0);
  using S = std::make_signed_t<T>;
  unsigned shamt = (bit_mask_sz<T> - bits);
  return S(num << shamt) >> shamt;
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
constexpr T checked_mul(T multiplicand, T multiplier,
                        const char *message = "checked multiply overflow") {
  T out;
  if (__builtin_mul_overflow(multiplicand, multiplier, &out)) [[unlikely]]
    report_fatal_error(message);
  return out;
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

static constexpr uint32_t hash_combine(uint32_t a, uint32_t b) {
  return a ^ (b + 0x9e3779b9 + (a << 6) + (a >> 2));
}

static constexpr uint64_t hash_combine(uint64_t a, uint64_t b) {
  return a ^ (b + 0x9e3779b97f4a7c15ull + (a << 6) + (a >> 2));
}

static constexpr uint32_t hash_u32(uint32_t a) {
  a = (a ^ 61) ^ (a >> 16);
  a = a + (a << 3);
  a = a ^ (a >> 4);
  a = a * 0x27d4eb2d;
  a = a ^ (a >> 15);
  return a;
}

static constexpr uint64_t hash_u64(uint64_t a) {
  a ^= a >> 33;
  a *= 0xff51afd7ed558ccdull;
  return a ^ (a >> 32);
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

template <std::integral NumT> class DynBitField {
public:
  using num_t = NumT;
  using size_type = uint8_t;

private:
  num_t &num;
  const size_type pos;
  const size_type n;

  constexpr num_t mask_ones() const { return bit_mask_ones<num_t>(n, pos); }
  constexpr num_t mask_zeros() const { return bit_mask_zeros<num_t>(n, pos); }
  constexpr num_t mask_ones_noshift() const { return bit_mask_ones<num_t>(n); }
  constexpr num_t mask_zeros_noshift() const {
    return bit_mask_zeros<num_t>(n);
  }

public:
  explicit constexpr DynBitField(num_t &v, size_t pos, size_t n)
      : num(v), pos(uint8_t(pos)), n(uint8_t(n)) {
    assert(pos < 256 && n < 256);
  }

  constexpr DynBitField &operator=(num_t v) {
    set(v);
    return *this;
  }

  constexpr DynBitField &operator+=(num_t v) { return (*this) = (*this) + v; }
  constexpr DynBitField &operator-=(num_t v) { return (*this) = (*this) - v; }
  constexpr DynBitField &operator|=(num_t v) { return (*this) = (*this) | v; }
  constexpr DynBitField &operator&=(num_t v) { return (*this) = (*this) & v; }
  constexpr DynBitField &operator^=(num_t v) { return (*this) = (*this) ^ v; }
  constexpr operator num_t() const { return get(); }

  constexpr num_t get() const { return (num & mask_ones()) >> pos; }
  constexpr void clr() { num &= mask_zeros(); }
  constexpr void set() { num |= mask_ones(); }

  constexpr void set(num_t v) {
    assert((v & mask_ones_noshift()) == v);
    clr();
    num |= v << pos;
  }

  constexpr void flip() { num ^= mask_ones(); }
  constexpr unsigned count() { return std::popcount(num & mask_ones()); }

  constexpr DynBitField at(size_type offs, size_type len = 1) {
    assert(offs + len <= pos + n);
    return DynBitField{num, size_type(pos + offs), len};
  }
};
