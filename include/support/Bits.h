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

template <typename T>
constexpr T bit_mask_wrap(unsigned pos = 0, unsigned wrapSz = bit_mask_sz<T>) {
  return bit_mask<T>(pos % wrapSz);
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

template <typename T> constexpr T ceil_to_pow2(T x) {
  if (x <= 1)
    return 1;
  return T(1) << (std::bit_width(x - 1));
}
template <typename T> constexpr T floor_to_pow2(T x) {
  return std::bit_floor(x);
}

template <typename T> constexpr T repeatBits(T x, unsigned xBits) {
  unsigned fact = xBits;
  assert(!(x & bit_mask_zeros<T>(xBits)));
  while (fact != bit_mask_sz<T>) {
    x |= (x << fact);
    fact <<= 1;
  }
  return x;
}

constexpr uint16_t pack_bits(uint32_t x) {
  x &= 0x55555555;

  x = (x | (x >> 1)) & 0x33333333;
  x = (x | (x >> 2)) & 0x0F0F0F0F;
  x = (x | (x >> 4)) & 0x00FF00FF;
  x = (x | (x >> 8)) & 0x0000FFFF;

  return x;
}

constexpr uint32_t unpack_bits(uint16_t x) {
  uint32_t xx = x;
  xx = (xx | (xx << 8)) & 0x00FF00FF;
  xx = (xx | (xx << 4)) & 0x0F0F0F0F;
  xx = (xx | (xx << 2)) & 0x33333333;
  xx = (xx | (xx << 1)) & 0x55555555;

  return xx;
}

constexpr uint32_t hash_combine(uint32_t a, uint32_t b) {
  return a ^ (b + 0x9e3779b9 + (a << 6) + (a >> 2));
}

constexpr uint64_t hash_combine64(uint64_t a, uint64_t b) {
  return a ^ (b + 0x9e3779b97f4a7c15ull + (a << 6) + (a >> 2));
}

constexpr uint32_t hash_u32(uint32_t a) {
  a = (a ^ 61) ^ (a >> 16);
  a = a + (a << 3);
  a = a ^ (a >> 4);
  a = a * 0x27d4eb2d;
  a = a ^ (a >> 15);
  return a;
}

constexpr uint64_t hash_u64(uint64_t a) {
  a ^= a >> 33;
  a *= 0xff51afd7ed558ccdull;
  return a ^ (a >> 32);
}

// split integer into regions of N bits, return 1000... for each region if equal
template <int N, std::integral T> constexpr T n_equal_mask(T lhs, T rhs) {
  constexpr T REP01 = repeatBits((T(1) << (N - 1)) - T(1), N);
  lhs ^= rhs;
  T lhsSC = ~(((lhs & REP01) + REP01) | lhs | REP01);
  return lhsSC;
}

template <typename T>
constexpr T bit_select(T val, unsigned i, unsigned n = 1) {
  unsigned pos = i * n;
  return (bit_mask_ones<T>(n, pos) & val) >> pos;
}

// fixme: these should use a shared base but then template param deduction
// fails.
template <std::unsigned_integral NumT, unsigned N, unsigned Pos>
class BitField {
  NumT &num;

public:
  using num_t = NumT;
  using num_signed_t = std::make_signed_t<NumT>;

  static constexpr num_t mask_sz = bit_mask_sz<num_t>;
  static constexpr num_t mask_ones = bit_mask_ones<num_t>(N, Pos);
  static constexpr num_t mask_zeros = bit_mask_zeros<num_t>(N, Pos);

  static constexpr num_t mask_ones_noshift = bit_mask_ones<num_t>(N);
  static constexpr num_t mask_zeros_noshift = bit_mask_zeros<num_t>(N);

  static constexpr unsigned size = N;
  static constexpr unsigned pos = Pos;

private:
  template <unsigned SubN, unsigned SubPos> struct make_SubField {
    static_assert(SubPos + SubN <= N);
    using type = BitField<NumT, SubN, SubPos + Pos>;
  };

public:
  template <unsigned SubN, unsigned SubPos>
  using SubField = make_SubField<SubN, SubPos>::type;

  template <unsigned HiN = mask_sz - (Pos + N)>
  using HiField = BitField<NumT, HiN, Pos + N>;
  ;

  static constexpr num_t getClr(num_t num) { return num & mask_zeros; }
  static constexpr num_t getSet(num_t num) { return num | mask_ones; }
  static constexpr num_t getSet(num_t num, num_t v) {
    assert((v & mask_ones_noshift) == v);
    return getClr(num) | (v << pos);
  }
  static constexpr num_t getVal(num_t num) { return (num & mask_ones) >> pos; }

  constexpr explicit BitField(num_t &v) : num(v) {}

  constexpr BitField &operator=(num_t v) {
    set(v);
    return *this;
  }

  constexpr BitField &operator+=(num_t v) { return (*this) = (*this) + v; }
  constexpr BitField &operator-=(num_t v) { return (*this) = (*this) - v; }

  constexpr operator num_t() const { return get(); }

  constexpr num_t get() const { return getVal(num); }

  constexpr void clr() { num &= mask_zeros; }

  constexpr void set() { num |= mask_ones; }

  constexpr void set(num_t v) { num = getSet(num, v); }

  constexpr void flip() { num ^= mask_ones; }

  constexpr unsigned count() const { return std::popcount(num & mask_ones); }

  template <unsigned SubN, unsigned SubPos> auto subField() {
    return SubField<SubN, SubPos>(num);
  }

  template <typename T> auto subField() {
    return SubField<T::size, T::pos>(num);
  }

  num_t getRawClr() { return getClr(num); }
  num_t getRawSet() { return getSet(num); }
  num_t getRawSet(num_t v) { return getSet(num, v); }
};

template <std::unsigned_integral NumT, unsigned N, unsigned Pos>
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

  constexpr unsigned count() const { return std::popcount(num & mask_ones); }
};

template <std::unsigned_integral NumT> class DynBitField {
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

template <std::unsigned_integral NumT> class DynBoolField {
public:
  using num_t = NumT;
  using size_type = uint8_t;

private:
  num_t &num;
  const size_type pos;

  constexpr num_t mask_one() const { return bit_mask<num_t>(pos); }
  constexpr num_t mask_zero() const { return ~bit_mask<num_t>(pos); }
  constexpr num_t mask(bool v) const { return num_t(v) << pos; }

public:
  explicit constexpr DynBoolField(num_t &v, size_t pos) : num(v), pos(pos) {
    assert(pos < bit_mask_sz<num_t>);
  }

  constexpr bool get() const { return num & mask_one(); }
  constexpr void clr() { num &= mask_zero(); }
  constexpr void set() { num |= mask_one(); }
  constexpr void flip() { num ^= mask_one(); }

  constexpr void set(bool v) {
    clr();
    num |= mask(v);
  }

  constexpr DynBoolField &operator=(bool v) {
    set(v);
    return *this;
  }

  constexpr DynBoolField &operator|=(bool v) {
    num |= mask(v);
    return *this;
  }
  constexpr DynBoolField &operator^=(bool v) {
    num ^= mask(v);
    return *this;
  }
  constexpr DynBoolField &operator&=(bool v) {
    num &= ~mask(!v);
    return *this;
  }

  constexpr operator bool() const { return get(); }
};

template <typename Field, typename Get, typename Set> class GetSetField {
  Field field;

public:
  using V = std::invoke_result_t<Get, Field &>;

  GetSetField(Field &field) : field(field) {}

  GetSetField &operator=(const V &newVal) { return *this; }

  void set(const V &newVal) { Set(field, newVal); }

  V get() { return Get(field); }

  operator V() { return get(); }
};
