#pragma once

#include <bit>
#include <cassert>
#include <climits>
#include <cstddef>
#include <cstdint>
#include <type_traits>

class TCInt {
public:
  template <unsigned N> static bool canTrunc(int32_t v) {
    return (v < (INT32_C(1) << (N - 1))) && (v >= -(INT32_C(1) << (N - 1)));
  }
};

// Machine Integer
// Two's complement arbitrary width integer used to model calculations performed
// by the target machine.
//
// All operations should behave like a two's complement machine under all
// circumstances, no operation should be able to invoke undefined behaviour in
// the host compiler. Therefore, care must be taken when using signed arithmetic
// in this class.
template <typename T = std::uint64_t> class MInt {
public:
  // Unsigned word type used for calculations and storage
  using MWord = T;
  // Signed version of word type used for sign extension and sign-dependent
  // operations.
  // intXX_t is guaranteed to be two's complement (even before C++20)
  using MWordSigned = std::make_signed_t<MWord>;

  constexpr static unsigned MWord_n = sizeof(MWord) * CHAR_BIT;

  static constexpr MWordSigned signedMWord(MWord word) {
    return std::bit_cast<MWordSigned>(word);
  }
  static constexpr MWord unsignedMWord(MWordSigned word) {
    return std::bit_cast<MWord>(word);
  }

  static constexpr MWord maskMWord(unsigned nSet) {
    return (MWord(1) << nSet) - 1;
  }
  static constexpr MWord zextMWord(unsigned n, MWord word) {
    if (n == MWord_n) {
      return word;
    }
    return word & maskMWord(n);
  }
  static constexpr MWordSigned sextMWord(unsigned n, MWord word) {
    if (n == MWord_n) {
      return signedMWord(word);
    }
    unsigned shamt = MWord_n - n;
    return signedMWord(word << shamt) >> shamt;
  }

  MWord getMWordZext() const { return zextMWord(n, word); }
  MWordSigned getMWordSignedSext() const { return sextMWord(n, word); }
  MWord getMWordSext() const { return unsignedMWord(sextMWord(n, word)); }

  MInt(unsigned n, MWord word) : n(n), word(word) { assert(n <= MWord_n); }

public:
  explicit MInt(int32_t imm) : MInt(32, unsignedMWord(imm)) {}

  explicit MInt(uint32_t imm) : MInt(32, imm) {}

  unsigned getBits() { return n; }

  MInt operator~() const { return {n, ~word}; }

  MInt operator-() const { return zero(n) - *this; }

  explicit operator bool() { return getMWordZext(); }

  static bool isBinopCompatible(const MInt &a, const MInt &b) {
    return a.n == b.n;
  }

  friend bool operator==(const MInt &a, const MInt &b) {
    assert(isBinopCompatible(a, b));
    return a.getMWordZext() == b.getMWordZext();
  }

  friend bool operator!=(const MInt &a, const MInt &b) {
    assert(isBinopCompatible(a, b));
    return a.getMWordZext() != b.getMWordZext();
  }

  bool lt(const MInt &o) const {
    assert(isBinopCompatible(*this, o));
    return getMWordSignedSext() < o.getMWordSignedSext();
  }
  bool ltu(const MInt &o) const {
    assert(isBinopCompatible(*this, o));
    return getMWordZext() < o.getMWordZext();
  }
  bool gt(const MInt &o) const {
    assert(isBinopCompatible(*this, o));
    return getMWordSignedSext() > o.getMWordSignedSext();
  }
  bool gtu(const MInt &o) const {
    assert(isBinopCompatible(*this, o));
    return getMWordZext() > o.getMWordZext();
  }
  bool le(const MInt &o) const {
    assert(isBinopCompatible(*this, o));
    return getMWordSignedSext() <= o.getMWordSignedSext();
  }
  bool leu(const MInt &o) const {
    assert(isBinopCompatible(*this, o));
    return getMWordZext() <= o.getMWordZext();
  }
  bool ge(const MInt &o) const {
    assert(isBinopCompatible(*this, o));
    return getMWordSignedSext() >= o.getMWordSignedSext();
  }
  bool geu(const MInt &o) const {
    assert(isBinopCompatible(*this, o));
    return getMWordZext() >= o.getMWordZext();
  }

  friend MInt operator+(const MInt &a, const MInt &b) {
    assert(isBinopCompatible(a, b));
    return {a.n, a.word + b.word};
  }
  friend MInt operator-(const MInt &a, const MInt &b) {
    assert(isBinopCompatible(a, b));
    return {a.n, a.word - b.word};
  }
  friend MInt operator*(const MInt &a, const MInt &b) {
    assert(isBinopCompatible(a, b));
    return {a.n, a.word * b.word};
  }
  friend MInt operator&(const MInt &a, const MInt &b) {
    assert(isBinopCompatible(a, b));
    return {a.n, a.word & b.word};
  }
  friend MInt operator|(const MInt &a, const MInt &b) {
    assert(isBinopCompatible(a, b));
    return {a.n, a.word | b.word};
  }
  friend MInt operator^(const MInt &a, const MInt &b) {
    assert(isBinopCompatible(a, b));
    return {a.n, a.word ^ b.word};
  }

  MInt udiv(const MInt &o) {
    assert(isBinopCompatible(*this, o));
    return {n, getMWordZext() / o.getMWordZext()};
  }
  // FIXME: might cause undefined behaviour on host compiler
  MInt sdiv(const MInt &o) {
    assert(isBinopCompatible(*this, o));
    return {n, unsignedMWord(getMWordSignedSext() / o.getMWordSignedSext())};
  }
  MInt urem(const MInt &o) {
    assert(isBinopCompatible(*this, o));
    return {n, getMWordZext() % o.getMWordZext()};
  }
  // FIXME: might cause undefined behaviour on host compiler
  MInt srem(const MInt &o) {
    assert(isBinopCompatible(*this, o));
    return {n, unsignedMWord(getMWordSignedSext() % o.getMWordSignedSext())};
  }

  friend MInt operator<<(const MInt &a, const MInt &b) {
    assert(isBinopCompatible(a, b));
    assert(b.getMWordZext() < a.n);
    return {a.n, a.word << b.getMWordZext()};
  }
  friend MInt operator<<(const MInt &a, unsigned shamt) {
    return a << MInt{a.n, shamt};
  }
  MInt srl(const MInt &o) const {
    assert(isBinopCompatible(*this, o));
    assert(o.getMWordZext() < n);
    return {n, getMWordZext() >> o.getMWordZext()};
  }
  MInt srl(unsigned shamt) const { return srl(MInt{n, shamt}); }
  MInt sra(const MInt &o) const {
    assert(isBinopCompatible(*this, o));
    assert(o.getMWordZext() < n);
    return {n, unsignedMWord(getMWordSignedSext() >> o.getMWordZext())};
  }
  MInt sra(unsigned shamt) const { return sra(MInt{n, shamt}); }

  MInt zext(unsigned newN) const {
    assert(newN > n);
    return {newN, getMWordZext()};
  }
  MInt sext(unsigned newN) const {
    assert(newN > n);
    return {newN, getMWordSext()};
  }
  MInt trunc(unsigned newN) const {
    assert(newN < n);
    return {newN, word};
  }

  bool canSTrunc(unsigned newN) const { return trunc(newN).sext(n) == *this; }
  bool canZTrunc(unsigned newN) const { return trunc(newN).zext(n) == *this; }

  bool isZero() const { return zero(n) == *this; }
  bool isOne() const { return one(n) == *this; }
  bool isOnes() const { return ones(n) == *this; }
  bool isNegOne() const { return negOne(n) == *this; }

  int32_t toInt32() { return getMWordSignedSext(); }

  static MInt one(unsigned n) { return {n, 1}; }
  static MInt zero(unsigned n) { return {n, 0}; }
  static MInt ones(unsigned n) { return ~MInt{n, 0}; }
  static MInt negOne(unsigned n) { return -one(n); }

  static MInt lowerOnes(unsigned n, unsigned setN) {
    assert(setN <= n);
    if (setN == n) [[unlikely]] {
      return ones(n);
    } else {
      return MInt{n, maskMWord(setN)};
    }
  }

  static MInt upperOnes(unsigned n, unsigned setN) {
    assert(setN <= n);
    return ~lowerOnes(n, n - setN);
  }

  static MInt shiftedOne(unsigned n, unsigned shamt) {
    return MInt{n, MWord(1) << shamt};
  }

private:
  unsigned n;
  MWord word; // unused upper bits of word are undefined
};
