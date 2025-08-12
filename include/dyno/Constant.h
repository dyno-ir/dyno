#pragma once

#include "support/ArrayRef.h"
#include "support/Bits.h"
#include "support/DenseMultimap.h"
#include "support/Optional.h"
#include "support/SmallVec.h"
#include "support/Utility.h"
#include <algorithm>
#include <bit>
#include <concepts>
#include <cstdint>
#include <dyno/IDs.h>
#include <dyno/NewDeleteObjStore.h>
#include <dyno/Obj.h>
#include <expected>
#include <iomanip>
#include <ios>
#include <iostream>
#include <iterator>
#include <ostream>
#include <span>
#include <string>
#include <type_traits>

namespace dyno {

template <typename T, size_t NumInline> class CexprVec {
  using value_type = T;
  using size_type = uint32_t;
  using iterator = T *;
  using const_iterator = const T *;
  using param_type = T &;

  std::array<T, NumInline> arr = {};
  uint32_t sz;

public:
  constexpr iterator begin() { return arr.begin(); }
  constexpr iterator end() { return arr.begin() + sz; }
  constexpr const_iterator begin() const { return arr.begin(); }
  constexpr const_iterator end() const { return arr.begin() + sz; }

  constexpr T &back() {
    assert(!empty());
    return *(end() - 1);
  }
  constexpr const T &back() const {
    assert(!empty());
    return *(end() - 1);
  }
  constexpr T &front() {
    assert(!empty());
    return *(begin());
  }
  constexpr const T &front() const {
    assert(!empty());
    return *(begin());
  }

  constexpr void pop_back() {
    assert(!empty());
    sz--;
  }

  constexpr T &operator[](size_type i) {
    assert(i < sz);
    return arr[i];
  }
  constexpr const T &operator[](size_type i) const {
    assert(i < sz);
    return arr[i];
  }

  constexpr void resize(size_type newSize) {
    assert(newSize <= NumInline);
    sz = newSize;
  }
  constexpr size_t size() const { return sz; }
  constexpr bool empty() const { return size() == 0; }

  constexpr CexprVec() = default;
  constexpr CexprVec(size_type sz) : sz(sz) {}
};

class ConstantRef;
template <typename T> class ConstantBuilderBase;

// only need one bit for sext/zext, two bits also allows extending with x or z
// for hardware.
constexpr size_t BigIntExtendBits = 2;

// custom bits are stored in all BigIntAPI containers. currently used to
// distinguish 2 state/4 state storage.
constexpr size_t BigIntCustomBits = 1;

constexpr uint32_t WordBits = sizeof(uint32_t) * 8;
constexpr uint32_t WordBitsM1 = WordBits - 1;

template <typename Container> class BigIntBase;
template <typename Derived> class BigIntMixin;

using BigInt = BigIntBase<SmallVec<uint32_t, 4>>;
using CBigInt = BigIntBase<CexprVec<uint32_t, 16>>;

template <typename T>
concept BigIntAPI = std::is_base_of<BigIntMixin<T>, T>::value;

struct FourState {
  enum {
    S0,
    S1,
    SZ,
    SX,
  };
  uint8_t val;
  FourState(uint8_t i) : val(i) { assert(i <= 4); }
  operator uint8_t() { return val; }

  bool isUnk() { return val == SZ || val == SX; }

  FourState operator!() { return isUnk() ? SX : !val; }
  FourState operator~() { return !*this; }
};

template <typename Derived> class BigIntMixin {
  friend BigInt;
  friend CBigInt;

  // Base API for BigInt is getRawNumBits, getNumWords, getExtend, getIs4S and
  // getWords. This is for utility functions using that API that may be shared
  // by all implementing BigInt API.
  constexpr Derived &self() { return *static_cast<Derived *>(this); }
  constexpr const Derived &self() const {
    return *static_cast<const Derived *>(this);
  }

protected:
  constexpr static uint32_t bitsToWords(uint32_t bits) {
    return round_up_div(bits, WordBits);
  }
  constexpr static unsigned repeatExtend(uint32_t num) {
    return repeatBits(num, BigIntExtendBits);
  }
  constexpr uint32_t getExtNumWords() const {
    return bitsToWords(self().getRawNumBits());
  }
  constexpr uint32_t __attribute__((always_inline)) getWord(uint32_t i) const {
    if (i >= bitsToWords(self().getRawNumBits()))
      dyno_unreachable("out of bounds");

    if (i >= self().getNumWords())
      return repeatExtend(self().getExtend());
    return self().getWords()[i];
  }
  constexpr uint32_t __attribute__((always_inline))
  getWord4S(uint32_t i) const {
    return self().getIs4S() ? getWord(i)
                            : unpack_bits(getWord(i / 2) >> ((i % 2) * 16));
  }
  constexpr uint8_t getRawBit(uint32_t i) const {
    assert(i <= self().getRawNumBits());
    return !!(getWord(i / WordBits) & (1 << (i % WordBits)));
  }

  constexpr uint8_t getRawSignBit() const {
    return getRawBit(self().getRawNumBits() - 1);
  }
  constexpr bool isExtended() const {
    return self().getNumWords() != getExtNumWords();
  }

public:
  uint8_t getExtendPatFromSignBit() const {
    if (self().getIs4S()) {
      return getSignBit();
    } else
      return getSignBit() ? 0b11 : 0;
  }

  constexpr FourState getBit(uint32_t i) const {
    assert(i <= getNumBits());
    if (!self().getIs4S())
      return !!(getWord(i / WordBits) & (1 << (i % WordBits)));
    i *= 2;
    assert(i <= self().getRawNumBits());
    return (getWord(i / WordBits) >> (i % WordBits)) &
           bit_mask_ones<uint32_t>(BigIntExtendBits);
  }
  constexpr uint8_t getSignBit() const {
    return getBit(self().getNumBits() - 1);
  }
  constexpr uint32_t getNumBits() const {
    return self().getIs4S() ? self().getRawNumBits() / 2
                            : self().getRawNumBits();
  }

  void toStream(std::ostream &os, int base = 16, bool unsized = false) const;
  friend std::ostream &operator<<(std::ostream &os, const Derived &self) {
    int base = 16;

    if (self.getIs4S() && self.getRawNumBits() < 16)
      base = 2;
    else if (!self.getIs4S() && self.getLimitedVal() &&
             (*self.getLimitedVal() <= 255 ||
              ((*self.getLimitedVal() + 1) % 100) <= 1)) {
      base = 10;
    }

    self.toStream(os, base);
    return os;
  }

  std::string toString(int base = 16, bool unsized = false) {
    std::stringstream str;
    toStream(str, base, unsized);
    return str.str();
  }

  template <BigIntAPI T>
  constexpr friend bool operator==(const Derived &lhs, const T &rhs) {
    if (!(lhs.getRawNumBits() == rhs.getRawNumBits() &&
          lhs.getNumWords() == rhs.getNumWords() &&
          lhs.getExtend() == rhs.getExtend() && lhs.getIs4S() == rhs.getIs4S()))
      return false;

    for (size_t i = 0; i < lhs.getNumWords(); i++)
      if (lhs.getWords()[i] != rhs.getWords()[i])
        return false;
    return true;
  }

  constexpr bool valueEquals(uint32_t val) const {
    if (auto val2 = getLimitedVal())
      return val == *val2;
    return false;
  }
  constexpr bool valueEqualsS(int32_t val) const {
    if (auto val2 = getLimitedValS())
      return val == *val2;
    return false;
  }

  // ext:11 0000 0001 bits: 1

  // ext:11 ffff ffff bits: 64
  // ext:00 ffff ffff bits: 64

  template <BigIntAPI T> constexpr bool valueEquals(const T &other) const {
    if (other.getNumWords() != self().getNumWords())
      return false;
    if (other.getExtend() != self().getExtend())
      return false;

    for (size_t i = 0; i < self().getNumWords(); i++)
      if (self().getWords()[i] != other.getWords()[i])
        return false;

    return true;
  }

  uint32_t getLastWordExtended(FourState extend) const {
    uint32_t word = self().getWords().back();

    if ((self().getRawNumBits() % WordBits) == 0)
      return word;

    word &= bit_mask_ones<uint32_t>(self().getRawNumBits() % WordBits);
    word |= bit_mask_zeros<uint32_t>(self().getRawNumBits() % WordBits) &
            repeatExtend(extend);
    return word;
  }

  template <BigIntAPI T> constexpr bool valueEqualsS(const T &other) const {
    if (other.getNumWords() != self().getNumWords())
      return false;
    if (other.getExtend() != self().getExtend())
      return false;

    for (size_t i = 0; i < self().getNumWords() - 1; i++)
      if (self().getWords()[i] != other.getWords()[i])
        return false;

    if (self().getLastWordExtended(self().getExtendPatFromSignBit()) !=
        other.getLastWordExtended(other.getExtendPatFromSignBit()))
      return false;

    return true;
  }

  constexpr std::optional<uint32_t> getRawLimitedVal() const {
    if (self().getNumWords() > 1)
      return std::nullopt;
    if (self().getExtNumWords() > 1 && self().getExtend() != 0)
      return std::nullopt;
    return self().getWords()[0];
  }

  constexpr std::optional<uint32_t> getLimitedVal() const {
    assert(!self().getIs4S());
    return getRawLimitedVal();
  }
  constexpr std::optional<int32_t> getLimitedValS() const {
    assert(!self().getIs4S());
    if (self().getNumWords() > 1)
      return std::nullopt;
    if (self().getExtNumWords() > 1 &&
        !(self().getExtend() == 0 ||
          (self().getExtend() == 0b11 &&
           self().getWords()[0] & bit_mask_msb<uint32_t>())))
      return std::nullopt;
    if (self().getNumBits() < WordBits)
      return sign_extend(self().getWords()[0], self().getNumBits());
    return self().getWords()[0];
  }
  constexpr uint32_t getExactVal() const {
    auto rv = getLimitedVal();
    assert(rv && "would truncate");
    return *rv;
  }
  constexpr uint32_t getExactValS() const {
    auto rv = getLimitedValS();
    assert(rv && "would truncate");
    return *rv;
  }

  constexpr bool allBitsUndef() const {
    if (!self().getIs4S())
      return false;
    if (self().isExtended() && !FourState{self().getExtend()}.isUnk())
      return false;
    for (auto word : self().getWords()) {
      if ((word & repeatBits(0b10U, 2)) != repeatBits(0b10U, 2))
        return false;
    }
    return true;
  }
};

class PatBigInt : public BigIntMixin<PatBigInt> {
  friend class Constant;
  friend class ConstantStore;
  friend class BigIntMixin<PatBigInt>;
  friend class BigIntBase<SmallVec<uint32_t, 4>>;
  friend class BigIntBase<CexprVec<uint32_t, 16>>;

  uint32_t bits;
  uint8_t field;

  template <typename T> using Extend = BitField<T, BigIntExtendBits, 0>;
  template <typename T>
  using Custom = BitField<T, BigIntCustomBits, BigIntExtendBits>;

public:
  uint32_t getNumWords() const { return 0; }
  uint32_t getRawNumBits() const { return bits; }
  uint8_t getExtend() const { return Extend{field}; }
  uint8_t getIs4S() const { return Custom{field}; }
  MutArrayRef<uint32_t> getWords() const {
    return MutArrayRef<uint32_t>::emptyRef();
  };
  PatBigInt(uint32_t bits, uint8_t pattern, uint8_t custom = 0)
      : bits(bits), field(0) {
    Extend{field} = pattern;
    Custom{field} = custom;
  }

  static constexpr PatBigInt undef(uint bits) {
    return PatBigInt{2 * bits, FourState::SX, 1};
  }
  static constexpr PatBigInt undef2(uint bits) {
    return PatBigInt{2 * bits, FourState::SZ, 1};
  }
  static constexpr PatBigInt fromFourState(FourState state, uint bits) {
    return PatBigInt{state.isUnk() ? bits * 2 : bits,
                     state == FourState::S1 ? FourState::SX : state,
                     state.isUnk()};
  }
  template <BigIntAPI T>
  static constexpr PatBigInt fromSign(const T &lhs, uint bits) {
    return PatBigInt{lhs.getIs4S() ? 2u * bits : bits,
                     lhs.getExtendPatFromSignBit(), lhs.getIs4S()};
  }
};

template <typename Container>
class BigIntBase : public BigIntMixin<BigIntBase<Container>> {
  friend CBigInt;
  friend BigInt;
  friend class Constant;
  friend class ConstantStore;
  friend class BigIntMixin<BigIntBase>;
  using BigIntMixin<BigIntBase<Container>>::repeatExtend;
  using BigIntMixin<BigIntBase<Container>>::isExtended;
  using BigIntMixin<BigIntBase<Container>>::getExtNumWords;

public:
  using BigIntMixin<BigIntBase<Container>>::getNumBits;

private:
  Container words;
  uint32_t numBits;
  uint8_t field;

  template <typename T> using Extend = BitField<T, BigIntExtendBits, 0>;
  template <typename T>
  using Custom = BitField<T, BigIntCustomBits, BigIntExtendBits>;

public:
  // big int API
  constexpr unsigned getNumWords() const { return words.size(); }
  constexpr uint32_t getRawNumBits() const { return numBits; }
  constexpr uint8_t getExtend() const { return Extend{field}; }
  constexpr uint8_t getIs4S() const { return Custom{field}; }
  constexpr MutArrayRef<uint32_t> getWords() { return words; }
  constexpr ArrayRef<uint32_t> getWords() const { return words; }

private:
  constexpr BigIntBase(uint32_t numBits, uint32_t numWords, uint8_t extend,
                       uint8_t custom)
      : words(numWords), numBits(numBits), field(0) {
    Extend<uint8_t>{field} = extend;
    Custom<uint8_t>{field} = custom;
  }

public:
  constexpr void setExtend(uint8_t val) {
    Extend{field} = val & bit_mask_ones<uint8_t>(BigIntExtendBits);
    normalizeExtend();
  }
  constexpr void setCustom(uint8_t val) {
    Custom{field} = val & bit_mask_ones<uint8_t>(BigIntCustomBits);
  }

  template <BigIntAPI T> constexpr BigIntBase &operator=(const T &other) {
    if constexpr (std::is_same_v<T, BigIntBase>)
      if (&other == this)
        return *this;

    this->numBits = other.getRawNumBits();
    Extend{field} = other.getExtend();
    Custom{field} = other.getIs4S();
    this->words.resize(std::max(other.getNumWords(), 1U));
    if (other.getNumWords() == 0)
      this->words[0] = repeatExtend(other.getExtend());
    std::copy(other.getWords().begin(), other.getWords().end(),
              this->getWords().begin());
    normalize();
    return *this;
  }

  constexpr BigIntBase &operator=(const BigIntBase &other) {
    return this->BigIntBase::operator= <BigIntBase>(other);
  }
  constexpr BigIntBase(const BigIntBase &other) : BigIntBase() {
    (*this) = other;
  }

  template <BigIntAPI T> constexpr BigIntBase(const T &other) : BigIntBase() {
    (*this) = other;
  }

  constexpr BigIntBase &operator=(BigIntBase &&other) = default;
  constexpr BigIntBase(BigIntBase &&other) = default;

  constexpr BigIntBase() : words(), numBits(0), field(0) {}
  constexpr static BigIntBase fromU64Pruned(uint64_t val) {
    BigIntBase rv;
    rv.setPruned(val);
    return rv;
  }
  constexpr static BigIntBase fromU64(uint64_t val, unsigned bits) {
    BigIntBase rv;
    rv.set(val, bits);
    return rv;
  }
  constexpr static BigIntBase fromI64(int64_t val, unsigned bits) {
    BigIntBase rv;
    rv.set(uint64_t(val), bits);
    rv.setExtend(val < 0 ? 0b11 : 0);
    return rv;
  }

  constexpr static BigIntBase ofLen(uint32_t bits) {
    return BigIntBase{bits, round_up_div(bits, WordBits), 0, 0};
  }

  constexpr static BigIntBase fromRaw(ArrayRef<uint32_t> data, uint32_t bits,
                                      uint8_t extend, uint8_t custom) {
    auto maxWords = round_up_div(bits, WordBits);
    if (data.size() > maxWords)
      data = ArrayRef{data.data(), maxWords};
    auto rv = BigIntBase{bits, (uint32_t)data.size(), extend, custom};
    std::copy(data.begin(), data.end(), rv.words.begin());
    rv.normalize();
    if (rv.getIs4S())
      rv.conv4To2StateIfPossible();
    return rv;
  }
  constexpr static BigIntBase fromRaw(SmallVecImpl<uint32_t> &&data,
                                      uint32_t bits, uint8_t extend,
                                      uint8_t custom) {
    auto maxWords = round_up_div(bits, WordBits);
    if (data.size() > maxWords)
      data.downsize(maxWords);

    auto rv = BigIntBase{};
    rv.words = std::move(data);
    rv.numBits = bits;
    rv.setExtend(extend);
    rv.setCustom(custom);
    rv.normalize();
    if (rv.getIs4S())
      rv.conv4To2StateIfPossible();
    return rv;
  }

#define LINEAR_OP(ident, code)                                                 \
  template <typename T0, typename T1>                                          \
  constexpr static void ident(BigIntBase &out, const T0 &lhs, const T1 &rhs) { \
    return carryPropOp<[](unsigned lhs, unsigned rhs,                          \
                          [[maybe_unused]] unsigned cin,                       \
                          [[maybe_unused]] unsigned *cout) { return code; }>(  \
        out, lhs, rhs);                                                        \
  }
  LINEAR_OP(addOp, __builtin_addc(lhs, rhs, cin, cout))
  LINEAR_OP(subOp, __builtin_subc(lhs, rhs, cin, cout))
  LINEAR_OP(andOp, lhs &rhs)
  LINEAR_OP(orOp, lhs | rhs)
  LINEAR_OP(xorOp, lhs ^ rhs)
  LINEAR_OP(xnorOp, ~(lhs ^ rhs))

  template <typename T>
  constexpr static void negateOp(BigIntBase &out, const T &lhs) {
    subOp(out, PatBigInt(lhs.getRawNumBits(), 0), lhs);
  }

  constexpr void setRepeating(uint32_t val, unsigned bits, uint8_t custom = 0) {
    words.resize(1);
    words[0] = val;
    numBits = bits;
    Extend{field} = val & bit_mask_ones<uint32_t>(BigIntExtendBits);
    Custom{field} = custom;
  }
  constexpr void set(uint64_t val, unsigned bits) {
    if (bits <= WordBits)
      return set((uint32_t)val, bits);
    words.resize(2);
    words[0] = val;
    words[1] = val >> WordBits;
    numBits = bits;
    normalize();
  }
  constexpr void set(uint32_t val, unsigned bits) {
    words.resize(1);
    words[0] = val;
    numBits = bits;
  }
  constexpr void setPruned(uint64_t val) {
    unsigned bits = clog2(val);
    numBits = bits;
    words.resize(bits <= WordBits ? 1 : 2);
    words[0] = val;
    if (words.size() == 2) {
      words[1] = val >> WordBits;
      normalize();
    }
  }

  template <bool Left, bool Arith = false, typename T0>
  static void shiftOp(BigIntBase &out, const T0 &lhs, uint32_t rhs) {
    uint32_t shamtWords = rhs / WordBits;
    uint32_t shamtRem = rhs % WordBits;
    uint8_t oldExtend = lhs.getExtend();
    out.numBits = lhs.getRawNumBits();
    if ((rhs & 1) && (Extend{out.field} == 1 || Extend{out.field} == 2))
      Extend{out.field} =
          (~oldExtend) & bit_mask_ones<uint8_t>(BigIntExtendBits);
    else
      Extend{out.field} = oldExtend & bit_mask_ones<uint8_t>(BigIntExtendBits);

    auto originalNumWords = lhs.getNumWords();

    if constexpr (Left)
      out.words.resize(
          std::min(originalNumWords + (round_up_div(rhs, WordBits)),
                   round_up_div(lhs.getRawNumBits(), WordBits)));
    else {
      if (!Arith && Extend{out.field} != 0) {
        out.words.resize(round_up_div(lhs.getRawNumBits(), WordBits));
        Extend{out.field} = 0;
      }
    }

    ssize_t lower = 0;
    ssize_t upper = out.getNumWords();
    ssize_t step = 1;
    if (Left) {
      lower = upper - 1;
      upper = -1;
      step = -1;
    }

    for (ssize_t i = lower; i != upper; i += step) {
      ssize_t idxHigh;
      ssize_t idxLow;
      if constexpr (Left) {
        idxHigh = i - shamtWords;
        idxLow = i - (shamtWords + 1);
      } else {
        idxHigh = i + shamtWords + 1;
        idxLow = i + shamtWords;
      }

      uint32_t low;
      if (idxLow < 0)
        low = 0;
      else if (idxLow >= originalNumWords) {
        low = repeatExtend(oldExtend);
        if constexpr (!Left && !Arith) {
          if (idxLow >= out.words.size())
            low = 0;
          else if (idxLow == out.words.size() - 1)
            low &= ((1 << (lhs.getRawNumBits()) % 32)) - 1;
        }
      } else
        low = lhs.getWords()[idxLow];

      uint32_t high;
      if (idxHigh < 0)
        high = 0;
      else if (idxHigh >= originalNumWords) {
        high = repeatExtend(oldExtend);
        if constexpr (!Left && !Arith) {
          if (idxHigh >= out.words.size())
            high = 0;
          else if (idxHigh == out.words.size() - 1)
            high &= (1 << (lhs.getRawNumBits() % 32)) - 1;
        }
      } else
        high = lhs.getWords()[idxHigh];

      if constexpr (Left) {
        out.words[i] = (high << shamtRem) |
                       ((shamtRem == 0) ? 0 : (low >> (WordBits - shamtRem)));
      } else {
        out.words[i] = ((shamtRem == 0) ? 0 : (high << (WordBits - shamtRem))) |
                       (low >> shamtRem);
      }
    }
    out.normalize();
  }

  template <int Mode = 0, typename T0, typename T1>
  constexpr static BigIntBase mulOp(const T0 &lhs, const T1 &rhs) {

    size_t resultBits;
    switch (Mode) {
    case 0: // trunc
      resultBits = std::max(lhs.getRawNumBits(), rhs.getRawNumBits());
      break;
    case 1: // extending, unsigned
      [[fallthrough]];
    case 2: // extending, signed
      resultBits = lhs.getRawNumBits() + rhs.getRawNumBits();
      break;
    }
    auto out = BigIntBase::ofLen(resultBits);

    for (size_t i = 0; i < lhs.getExtNumWords(); i++) {

      uint64_t carry = 0;

      for (size_t j = 0;
           j < rhs.getExtNumWords() && (i + j) < out.getNumWords(); j++) {
        uint32_t lhsVal = lhs.getWord(i);
        uint32_t rhsVal = rhs.getWord(j);

        uint64_t result = uint64_t(lhsVal) * rhsVal;

        size_t outIdx = i + j;

        uint64_t temp = result + carry + out.getWords()[outIdx];

        out.getWords()[outIdx] = uint32_t(temp);
        carry = temp >> 32;
      }

      if (size_t idx = i + rhs.getExtNumWords(); idx < out.getNumWords())
        out.getWords()[idx] = carry;
    }

    // correct result for signed multiply
    if constexpr (Mode == 2) {
      if (lhs.getRawSignBit()) {
        uint32_t b = 0;
        for (size_t i = 0; i < rhs.getExtNumWords() &&
                           i + lhs.getExtNumWords() < out.getNumWords();
             i++) {
          out.getWords()[i + lhs.getExtNumWords()] = __builtin_subc(
              out.getWords()[i + lhs.getExtNumWords()], rhs.getWord(i), b, &b);
        }
      }
      if (rhs.getRawSignBit()) {
        uint32_t b = 0;
        for (size_t i = 0; i < lhs.getExtNumWords() &&
                           i + rhs.getExtNumWords() < out.getNumWords();
             i++) {
          out.getWords()[i + rhs.getExtNumWords()] = __builtin_subc(
              out.getWords()[i + rhs.getExtNumWords()], lhs.getWord(i), b, &b);
        }
      }
    }

    out.normalize();
    return out;
  }

  template <typename T0> constexpr static uint32_t leadingZeros(const T0 &val) {
    if (val.isExtended()) {
      if (val.getExtend() == 0b11 || val.getExtend() == 0b10)
        return 0;
      if (val.getExtend() == 0b01)
        return 1;
      assert(val.getExtend() == 0);
    }

    // may be negative.
    int32_t zeroBits = val.getRawNumBits() - (val.getNumWords() * WordBits);
    uint32_t lastWord = val.getWords()[val.getNumWords() - 1];
    if (lastWord == 0)
      return zeroBits + WordBits;
    return zeroBits + __builtin_clz(lastWord);
  }

  template <typename T0>
  constexpr static uint32_t leadingNonUnk(const T0 &val) {
    if (!val.getIs4S())
      return val.getNumBits();
    BigInt copy = unknownMask(val);
    return leadingZeros(copy);
  }

  template <typename T0>
  constexpr static uint32_t leadingZeros4SExact(const T0 &val) {
    return leadingBits4SExact(val, FourState::S0);
  }

  template <typename T0>
  constexpr static uint32_t countBitsExact(const T0 &val, bool bit) {
    uint32_t cnt = 0;
    for (uint i = 0; i < val.getExtNumWords(); i++) {
      cnt += std::popcount(bit ? val.getWord(i) : ~val.getWord(i));
    }
    return cnt;
  }

  template <typename T0>
  constexpr static uint32_t countBits4SExact(const T0 &val, FourState bit) {
    if (!val.getIs4S()) {
      if (bit.isUnk())
        return 0;
      return countBitsExact(val, bit.val == FourState::S1);
    }

    uint32_t cnt = 0;
    for (uint i = 0; i < val.getExtNumWords(); i++) {
      cnt += std::popcount(
          n_equal_mask<2>(val.getWord4S(i), repeatBits(bit.val, 2)));
    }
    return cnt;
  }

  template <typename T0>
  constexpr static uint32_t leadingBits4SExact(const T0 &val, FourState state) {
    BigInt copy;
    bitsExactEqual4S(copy, val,
                     PatBigInt::fromFourState(state, val.getNumBits()));
    notOp4S(copy, copy);
    return leadingZeros(copy);
  }

  template <typename T0, typename T1>
  static BigIntBase upowOp(const T0 &lhs, const T1 &rhs) {
    if (rhs.valueEquals(0))
      return BigIntBase::fromU64(1, lhs.getNumBits());
    if (lhs.valueEquals(0))
      return BigIntBase::fromU64(0, lhs.getNumBits());

    BigIntBase acc = BigIntBase::fromU64(1, lhs.getNumBits());
    BigIntBase pow{lhs};

    uint32_t bits = rhs.getRawNumBits() - leadingZeros(rhs);
    for (uint32_t i = 0; i < bits; i++) {
      if (rhs.getRawBit(i)) {
        acc = BigIntBase::mulOp(acc, pow);
        if (acc.valueEquals(0))
          break;
      }
      pow = BigIntBase::mulOp(pow, pow);
    }
    return acc;
  }

  template <typename T0, typename T1>
  static BigIntBase spowOp(const T0 &lhs, const T1 &rhs) {
    if (!rhs.getRawSignBit())
      return upowOp(lhs, rhs);

    if (lhs.valueEqualsS(-1)) {
      return BigIntBase::fromI64(rhs.getRawBit(0) ? 1 : -1, lhs.getNumBits());
    }
    if (lhs.valueEquals(0)) {
      // in verilog returns x but we check for that in the 4S func
      // instead. just return zero here.
      return BigIntBase::fromU64(0, lhs.getNumBits());
    }
    if (lhs.valueEquals(1)) {
      return BigIntBase::fromU64(1, lhs.getNumBits());
    }
    return BigIntBase::fromU64(0, lhs.getNumBits());
  }

  template <typename T0>
  static Optional<uint32_t> leadingZeros4S(const T0 &val) {
    if (val.getIs4S())
      return nullopt;
    return leadingZeros(val);
  }

  template <typename T0, typename T1>
  static BigIntBase upowOp4S(const T0 &lhs, const T1 &rhs) {
    if (lhs.getIs4S() || rhs.getIs4S())
      return PatBigInt(lhs.getRawNumBits(), 0b11, 1);
    return BigIntBase::upowOp(lhs, rhs);
  }

  template <typename T0, typename T1>
  static BigIntBase spowOp4S(const T0 &lhs, const T1 &rhs) {
    if (lhs.getIs4S() || rhs.getIs4S() ||
        (lhs.valueEquals(0) && rhs.getRawSignBit()))
      return PatBigInt(lhs.getRawNumBits(), 0b11, 1);
    return BigIntBase::spowOp(lhs, rhs);
  }

  template <typename T>
  constexpr static void resizeOp(BigIntBase &out, const T &lhs,
                                 uint32_t newSize, uint8_t extendPat = 0) {

    auto copyIfDifferent = [&]() {
      if (lhs.getWords().begin() != out.getWords().begin())
        std::copy_n(lhs.getWords().begin(),
                    std::min(lhs.getNumWords(), out.getNumWords()),
                    out.getWords().begin());
    };

    // truncate
    if (newSize < lhs.getRawNumBits()) {
      out.words.resize(std::min(lhs.getNumWords(),
                                std::max(1u, round_up_div(newSize, WordBits))));
      copyIfDifferent();
      out.numBits = newSize;

      // cutting of top (bits) might reveal a collapsable section
      out.normalize();
      return;
    }

    if ((extendPat == lhs.getExtend() || !lhs.isExtended())) {

      // as long as extend stays the same (or is freely assignable) we can
      // extend by just changing numBits
      out.words.resize(lhs.getNumWords());
      copyIfDifferent();

      if (extendPat != lhs.getExtend()) {
        out.setLastWordBitsToPattern(extendPat);
      }
      out.numBits = newSize;
      out.setExtend(extendPat);

      // no need to normalize here
      return;
    }

    // else we need to materialize lhs's extend bits
    out.words.resize(lhs.getExtNumWords());
    for (size_t i = lhs.getNumWords(); i < lhs.getExtNumWords(); i++)
      out.getWords()[i] = lhs.getWord(i);

    out.numBits = newSize;
    Extend{out.field} = extendPat;
  }

public:
  template <BigIntAPI T>
  static void rangeSelectOp(BigIntBase &out, const T &src, uint32_t bitOffs,
                            uint32_t bitLen) {
    if (bitLen == 0) {
      out = BigIntBase::fromU64(0, 0);
      return;
    }

    uint32_t outWords = round_up_div(bitLen, WordBits);
    do {
      if constexpr (std::is_same_v<T, BigIntBase>) {
        if (&out == &src)
          break;
      }
      out.words.resize(outWords);
    } while (false);

    uint32_t offs = bitOffs / WordBits;
    uint32_t shamt = bitOffs % WordBits;

    for (uint32_t i = 0; i < std::min(outWords, out.words.size()); i++) {
      size_t lowI = i + offs;
      size_t highI = i + offs + 1;

      out.words[i] = (src.getWord(lowI) >> shamt) |
                     ((highI >= src.getExtNumWords() || shamt == 0)
                          ? 0
                          : (src.getWord(highI) << (32 - shamt)));
    }

    out.words.resize(outWords);
    out.numBits = bitLen;
    out.normalize();
  }

  template <typename T0, typename T1>
  static auto udivmodOp(const T0 &lhs, const T1 &rhs) {
    const uint64_t base = 1UL << 32;

    BigIntBase quot = BigIntBase::ofLen(lhs.getRawNumBits());
    BigIntBase rem = BigIntBase::ofLen(rhs.getRawNumBits());

    // todo: 64 and maybe 128 bit division fast paths.

    size_t highWordIdx;
    if (rhs.getExtend() == 0)
      highWordIdx = rhs.getNumWords() - 1;
    else
      highWordIdx = rhs.getExtNumWords() - 1;

    // divisor is 32 bits or less.
    if (highWordIdx == 0) {
      int32_t k = 0;
      for (ssize_t i = quot.getNumWords() - 1; i >= 0; i--) {
        // pull down one digit
        uint64_t t = (k * base + lhs.getWord(i));
        // do single digit division
        quot.words[i] = t / rhs.getWord(0);
        // correct back
        k = t - quot.words[i] * rhs.getWord(0);
      }
      rem.words[0] = k;

      quot.normalize();
      rem.normalize();

      return std::make_pair(std::move(quot), std::move(rem));
    }

    // Normalize by shifting v left just enough so that
    // its high-order bit is on, and shift u left the
    // same amount. We may have to append a high-order
    // digit on the dividend; we do that unconditionally.
    BigIntBase un;
    BigIntBase vn;

    // Find most significant 1 bit.
    uint32_t shamt = __builtin_clz(rhs.getWord(highWordIdx));
    vn.resizeOp(vn, rhs, (highWordIdx + 1) * 32 - shamt);
    BigIntBase::shlOp(vn, vn, shamt);

    BigIntBase::resizeOp(un, lhs, lhs.getRawNumBits() + shamt);
    BigIntBase::shlOp(un, un, shamt);
    un.expand();

    // std::cout << "un = " << un << "\n";
    // std::cout << "vn = " << vn << "\n";

    size_t n = vn.getExtNumWords();
    for (ssize_t i = un.getNumWords() - 1 - n; i >= 0; i--) {
      int64_t t = (un.getWord(i + n) * base + un.getWord(i + n - 1));

      uint64_t qhat = t / vn.getWord(n - 1);
      uint64_t rhat = t - qhat * vn.getWord(n - 1);

      while (1) {
        if (qhat >= base ||
            qhat * vn.getWord(n - 2) > base * rhat + un.getWord(i + n - 2)) {
          qhat--;
          rhat += vn.getWord(n - 1);
          if (rhat < base) [[unlikely]]
            continue;
        }
        break;
      }

      int64_t k = 0;
      for (size_t j = 0; j < n; j++) {
        auto p = qhat * vn.getWord(j);
        t = un.getWord(i + j) - k - (p & 0xFFFF'FFFF);
        un.words[i + j] = t;
        k = (p >> 32) - (t >> 32);
      }
      t = un.getWord(i + n) - k;
      un.words[i + n] = t;

      quot.words[i] = qhat;
      if (t < 0) {
        quot.words[i] -= 1;
        k = 0;
        for (size_t j = 0; j < n; j++) {
          t = un.getWord(i + j) + vn.getWord(j) + k;
          un.words[i + j] = t;
          k = t >> 32;
        }
        un.words[i + n] += k;
      }
    }

    BigIntBase::lshrOp(rem, un, shamt);
    BigIntBase::resizeOp(rem, rem, rhs.getRawNumBits());

    quot.normalize();
    return std::make_pair(std::move(quot), std::move(rem));
  }

  template <BigIntAPI T0, BigIntAPI T1>
  static bool icmpEqualOp(const T0 &lhs, const T1 &rhs) {
    if (lhs.getNumWords() != rhs.getNumWords())
      return false;
    if (lhs.getRawNumBits() != rhs.getRawNumBits())
      return false;
    if (lhs.getExtend() != rhs.getExtend())
      return false;

    for (size_t i = 0; i < lhs.getNumWords(); i++)
      if (lhs.getWord(i) != rhs.getWord(i))
        return false;

    return true;
  }

  template <BigIntAPI T0, BigIntAPI T1>
  static FourState icmpWildcardEqualOp4S(const T0 &lhs, const T1 &rhs) {

    if (!lhs.getIs4S() && !rhs.getIs4S())
      return icmpEqualOp(lhs, rhs) ? FourState::S1 : FourState::S0;
    if (lhs.getNumBits() != rhs.getNumBits())
      return false;

    size_t n = std::max(lhs.getExtNumWords(), rhs.getExtNumWords());

    bool notEqual = 0;

    for (size_t i = 0; i < n; i++) {
      uint32_t lhsV = lhs.getWord4S(i);
      uint32_t rhsV = rhs.getWord4S(i);

      uint32_t mask = (rhsV & REP10);
      mask |= mask >> 1;

      uint32_t neq = (lhsV ^ rhsV) & ~mask;

      if (neq)
        notEqual = 1;

      if ((neq & REP10) != 0)
        return FourState::SX;
    }

    return (!notEqual) ? FourState::S1 : FourState::S0;
  }

  template <auto MaskFunc, BigIntAPI T0, BigIntAPI T1>
  static FourState icmpCaseXZEqualOp4S(const T0 &lhs, const T1 &rhs) {
    if (!lhs.getIs4S() && !rhs.getIs4S())
      return icmpEqualOp(lhs, rhs) ? FourState::S1 : FourState::S0;
    if (lhs.getNumBits() != rhs.getNumBits())
      return false;

    size_t n = std::max(lhs.getExtNumWords(), rhs.getExtNumWords());

    for (size_t i = 0; i < n; i++) {
      uint32_t lhsV = lhs.getWord4S(i);
      uint32_t rhsV = rhs.getWord4S(i);

      uint32_t mask = MaskFunc(lhsV, rhsV);
      mask |= mask >> 1;

      uint32_t neq = (lhsV ^ rhsV) & ~mask;

      if (neq)
        return FourState::S0;
    }
    return FourState::S1;
  }

  template <BigIntAPI T0, BigIntAPI T1>
  static FourState icmpCaseZEqualOp4S(const T0 &lhs, const T1 &rhs) {
    return icmpCaseXZEqualOp4S<[](uint32_t lhsV, uint32_t rhsV) -> uint32_t {
      return n_equal_mask<2>(rhsV, EXTZ_MASK) |
             n_equal_mask<2>(lhsV, EXTZ_MASK);
    }>(lhs, rhs);
  }
  template <BigIntAPI T0, BigIntAPI T1>
  static FourState icmpCaseXEqualOp4S(const T0 &lhs, const T1 &rhs) {
    return icmpCaseXZEqualOp4S<[](uint32_t lhsV, uint32_t rhsV) -> uint32_t {
      return (rhsV & REP10) | (lhsV & REP10);
    }>(lhs, rhs);
  }

  template <BigIntAPI T0, BigIntAPI T1>
  static bool icmpUnsignedLessOp(const T0 &lhs, const T1 &rhs,
                                 bool onEqual = false) {
    assert(lhs.getNumWords() == rhs.getNumWords());
    assert(lhs.getRawNumBits() == rhs.getRawNumBits());

    if (lhs.getExtend() < rhs.getExtend())
      return true;

    for (size_t i = lhs.getNumWords(); i-- > 0;) {
      auto lw = lhs.getWord(i);
      auto rw = rhs.getWord(i);
      if (lw < rw)
        return true;
      if (lw > rw)
        return false;
    }

    return onEqual;
  }
  template <BigIntAPI T0, BigIntAPI T1>
  static bool icmpSignedLessOp(const T0 &lhs, const T1 &rhs,
                               bool onEqual = false) {
    assert(lhs.getNumWords() == rhs.getNumWords());
    assert(lhs.getRawNumBits() == rhs.getRawNumBits());

    bool sL = lhs.getSignBit();
    bool sR = rhs.getSignBit();

    if (sL != sR)
      return sL;

    for (size_t i = lhs.getNumWords(); i-- > 0;) {
      auto lw = lhs.getWord(i);
      auto rw = rhs.getWord(i);
      if (lw == rw)
        continue;

      if (!sL) // both >= 0
        return lw < rw;
      else // both < 0, more negative means less
        return lw > rw;
    }

    return onEqual;
  }

  enum ICmpPred {
    ICMP_EQ,
    ICMP_NE,
    // case equality, compares x/z by value
    ICMP_CEQ,
    ICMP_CNE,
    // wildcard equality
    ICMP_WEQ,
    ICMP_WNE,

    // casez equality
    ICMP_CZEQ,
    ICMP_CZNE,

    // casex equality
    ICMP_CXEQ,
    ICMP_CXNE,

    ICMP_ULT,
    ICMP_SLT,
    ICMP_ULE,
    ICMP_SLE,
    ICMP_UGT,
    ICMP_SGT,
    ICMP_UGE,
    ICMP_SGE,
  };

  template <BigIntAPI T0, BigIntAPI T1>
  static bool icmpOp(const T0 &lhs, const T1 &rhs, ICmpPred pred) {
    switch (pred) {
    case ICMP_EQ:
      return icmpEqualOp(lhs, rhs);
    case ICMP_NE:
      return !icmpEqualOp(lhs, rhs);
    case ICMP_ULT:
      return icmpUnsignedLessOp(lhs, rhs, false);
    case ICMP_SLT:
      return icmpSignedLessOp(lhs, rhs, false);
    case ICMP_ULE:
      return icmpUnsignedLessOp(lhs, rhs, true);
    case ICMP_SLE:
      return icmpSignedLessOp(lhs, rhs, true);
    case ICMP_UGT:
      return icmpUnsignedLessOp(rhs, lhs, false);
    case ICMP_SGT:
      return icmpSignedLessOp(rhs, lhs, false);
    case ICMP_UGE:
      return icmpUnsignedLessOp(rhs, lhs, true);
    case ICMP_SGE:
      return icmpSignedLessOp(rhs, lhs, true);
    default:
      dyno_unreachable("invalid predicate");
    }
  }

  template <BigIntAPI T0, BigIntAPI T1>
  static FourState icmpOp4S(const T0 &lhs, const T1 &rhs, ICmpPred pred) {
    switch (pred) {
    case ICMP_CEQ:
      return icmpEqualOp(lhs, rhs);
    case ICMP_CNE:
      return !icmpEqualOp(lhs, rhs);
    case ICMP_WEQ:
      return icmpWildcardEqualOp4S(lhs, rhs);
    case ICMP_WNE:
      return !icmpWildcardEqualOp4S(lhs, rhs);
    case ICMP_CZEQ:
      return icmpCaseZEqualOp4S(lhs, rhs);
    case ICMP_CZNE:
      return !icmpCaseZEqualOp4S(lhs, rhs);
    case ICMP_CXEQ:
      return icmpCaseXEqualOp4S(lhs, rhs);
    case ICMP_CXNE:
      return !icmpCaseXEqualOp4S(lhs, rhs);
    default: {
      if (lhs.getIs4S() || rhs.getIs4S())
        return FourState::SX;
      return icmpOp(lhs, rhs, pred);
    }
    }
  }

  template <typename T0, typename T1>
  static auto sdivmodOp(const T0 &lhs, const T1 &rhs) {
    bool lhsSign = lhs.getRawSignBit();
    bool rhsSign = rhs.getRawSignBit();

    BigIntBase lhsAbs = lhs.getRawSignBit() ? -lhs : lhs;
    BigIntBase rhsAbs = rhs.getRawSignBit() ? -rhs : rhs;

    auto [div, mod] = udivmodOp(lhsAbs, rhsAbs);

    if (lhsSign ^ rhsSign)
      negateOp(div, div);
    if (lhsSign)
      negateOp(mod, mod);

    return std::make_pair(std::move(div), std::move(mod));
  }

  template <typename T0>
  static void shlOp(BigIntBase &out, const T0 &lhs, uint32_t rhs) {
    shiftOp<true>(out, lhs, rhs);
  }

  template <typename T0>
  static void lshrOp(BigIntBase &out, const T0 &lhs, uint32_t rhs) {
    shiftOp<false>(out, lhs, rhs);
  }

  template <typename T0>
  static void ashrOp(BigIntBase &out, const T0 &lhs, uint32_t rhs) {
    shiftOp<false, true>(out, lhs, rhs);
  }

  template <typename T0> static bool reductionXOROp(const T0 &val) {
    uint32_t acc = 0;

    ssize_t words = val.getExtNumWords();

    if (val.getRawNumBits() % 32 != 0) {
      acc ^= val.getWord(val.getExtNumWords() - 1) &
             bit_mask_ones<uint32_t>(val.getRawNumBits() % 32);
      words--;
    }

    if (auto extWords = words - ssize_t(val.getNumWords()); extWords > 0) {
      if (extWords & 1)
        acc ^= repeatExtend(val.getExtend());
    }

    for (size_t i = 0; i < std::min(val.getNumWords(), uint32_t(words)); i++) {
      acc ^= val.getWords()[i];
    }

    return __builtin_parity(acc);
  }

  template <BigIntAPI T0, BigIntAPI T1>
  constexpr static void concatOp(BigIntBase &out, const T0 &lhsMayAlias,
                                 const T1 &rhs) {
    size_t rhsWords = rhs.getNumWords();
    size_t rhsExtWords = rhs.getExtNumWords();
    size_t rhsBits = rhs.getRawNumBits();

    const T0 *lhsPtr = &lhsMayAlias;
    BigIntBase copy;
    if constexpr (std::is_same_v<BigIntBase, T0>) {
      if (&out == &lhsMayAlias) {
        copy = lhsMayAlias;
        lhsPtr = &copy;
      }
    }

    auto &lhs = *lhsPtr;

    if (lhs.getNumBits() == 0) {
      out = rhs;
      return;
    }
    if (rhs.getNumBits() == 0) {
      out = lhs;
      return;
    }

    auto lhsBits = lhs.getRawNumBits();
    out.numBits = lhsBits + rhsBits;
    uint32_t outNumWords = rhsExtWords + std::max(1u, lhs.getNumWords());
    if ((rhsBits % 32) && (lhsBits % 32) &&
        (rhsBits % 32) + (lhsBits % 32) <= 32)
      outNumWords--;

    out.words.resize(outNumWords);

    do {
      // edge case rhs == out.
      if constexpr (std::is_same_v<T1, BigIntBase>) {
        if (&rhs == &out)
          break;
      }
      // regular case, copy into out
      std::copy_n(rhs.getWords().begin(), rhsWords, out.words.begin());
    } while (false);

    // copy RHS extend into out
    for (size_t i = rhsWords; i < rhsExtWords; i++)
      out.words[i] = repeatExtend(rhs.getExtend());

    size_t lhsOffs = rhsBits / WordBits;
    size_t lhsShamt = rhsBits % WordBits;

    out.words[lhsOffs] &= bit_mask_ones<uint32_t>(lhsShamt);
    out.words[lhsOffs] |= lhs.getWord(0) << lhsShamt;

    for (size_t i = lhsOffs + 1; i < out.words.size(); i++) {
      size_t lowI = i - lhsOffs - 1;
      size_t highI = i - lhsOffs;

      out.words[i] =
          (lhsShamt == 0 ? 0 : (lhs.getWord(lowI) >> (32 - lhsShamt))) |
          ((highI >= lhs.getExtNumWords()) ? 0
                                           : (lhs.getWord(highI) << lhsShamt));
    }

    Extend{out.field} = lhs.getExtend();
    out.normalize();
  }

  template <BigIntAPI T0>
  static void repeatOp(BigIntBase &out, const T0 &val, uint32_t count) {
    if (count == 0) {
      out.set(0U, 0);
      return;
    } else if (count == 1) {
      out = val;
      return;
    } else if (count == 2) {
      concatOp(out, val, out);
      return;
    }

    // out might alias val so do this first
    BigIntBase pow;
    concatOp(pow, val, val);

    if (!(count & 1))
      out.set(0U, 0);
    count &= bit_mask_zeros<decltype(count)>(1);

    for (size_t i = 1; i < bit_mask_sz<decltype(count)> && count; i++) {
      if (count & (1ULL << i)) {
        concatOp(out, pow, out);
        count &= ~(1ULL << i);
      }

      // todo: edge case concat for repeat
      concatOp(pow, pow, pow);
    }
  }

  // 4 State
  static constexpr uint32_t REP00 = repeatBits(0b00U, 2);
  static constexpr uint32_t REP01 = repeatBits(0b01U, 2);
  static constexpr uint32_t REP10 = repeatBits(0b10U, 2);
  static constexpr uint32_t REP11 = repeatBits(0b11U, 2);

  static constexpr uint32_t EXT0_MASK = REP00;
  static constexpr uint32_t EXT1_MASK = REP01;
  static constexpr uint32_t EXTZ_MASK = REP10;
  static constexpr uint32_t EXTX_MASK = REP11;

  // could specialize these with the fancy AVX512 LUT function
  static constexpr uint32_t and_4state(uint32_t lhs, uint32_t rhs) {
    uint32_t lhsSC = n_equal_mask<2>(lhs, REP00);
    uint32_t rhsSC = n_equal_mask<2>(rhs, REP00);

    lhsSC |= lhsSC >> 1;
    rhsSC |= rhsSC >> 1;

    uint32_t lhsX = (lhs & REP10);
    lhsX |= lhsX >> 1;

    uint32_t rhsX = (rhs & REP10);
    rhsX |= rhsX >> 1;

    return ((lhs & rhs) | lhsX | rhsX) & ~(lhsSC | rhsSC);
  }
  static constexpr uint32_t xor_4state(uint32_t lhs, uint32_t rhs) {
    uint32_t lhsX = (lhs & REP10);
    lhsX |= lhsX >> 1;

    uint32_t rhsX = (rhs & REP10);
    rhsX |= rhsX >> 1;

    return ((lhs ^ rhs) | lhsX | rhsX);
  }
  static constexpr uint32_t xnor_4state(uint32_t lhs, uint32_t rhs) {
    uint32_t val = xor_4state(lhs, rhs);
    val ^= ~(val >> 1);
    return val;
  }
  static constexpr uint32_t or_4state(uint32_t lhs, uint32_t rhs) {
    uint32_t lhsSC = n_equal_mask<2>(lhs, REP01);
    uint32_t rhsSC = n_equal_mask<2>(rhs, REP01);

    uint32_t lhsX = (lhs & REP10);
    lhsX |= lhsX >> 1;

    uint32_t rhsX = (rhs & REP10);
    rhsX |= rhsX >> 1;

    return (((lhs | rhs) | lhsX | rhsX) & ~(lhsSC | rhsSC)) | (lhsSC >> 1) |
           (rhsSC >> 1);
  }
  // static void test() {
  //   static_assert(BigIntBase::and_4state(0b00'00'01'01'00'10'10'11,
  //                                      0b00'01'00'01'10'00'01'01) ==
  //                 0b00'00'00'01'00'00'11'11);
  //   static_assert(BigIntBase::or_4state(0b00'01'10'10'11, 0b01'00'01'00'00)
  //   ==
  //                 0b01'01'01'11'11);
  // }

  // 4 State Ops
  void conv4To2State() {
    Extend{field} = (Extend{field} & 1) * 0b11;
    size_t outNumWords = round_up_div(getNumWords(), 2U);
    for (size_t i = 0; i < getNumWords() / 2; i++) {
      words[i] = (pack_bits(words[2 * i + 1]) << 16) | pack_bits(words[2 * i]);
    }
    if (outNumWords != getNumWords() / 2)
      words[outNumWords - 1] = (repeatExtend(getExtend()) << 16) |
                               pack_bits(words[getNumWords() - 1]);

    words.resize(outNumWords);
    numBits /= 2;
    setCustom(0);
  }
  void conv4To2StateHi() {
    Extend{field} = ((Extend{field} >> 1) & 1) * 0b11;
    size_t outNumWords = round_up_div(getNumWords(), 2U);
    for (size_t i = 0; i < getNumWords() / 2; i++) {
      words[i] = (pack_bits(words[2 * i + 1] >> 1) << 16) |
                 pack_bits(words[2 * i] >> 1);
    }
    if (outNumWords != getNumWords() / 2)
      words[outNumWords - 1] = ((repeatExtend(getExtend()) >> 1) << 16) |
                               pack_bits(words[getNumWords() - 1] >> 1);
    words.resize(outNumWords);
    numBits /= 2;
    setCustom(0);
  }
  template <BigIntAPI T> static constexpr BigIntBase unknownMask(const T &val) {
    BigIntBase copy = val;
    copy.conv4To2StateHi();
    copy.normalize();
    return copy;
  }
  void conv4To2StateIfPossible() {
    if (!getIs4S())
      return;
    uint32_t hasUnk = isExtended() ? (getExtend() & REP10) : 0;
    for (size_t i = 0; i < getNumWords() && !hasUnk; i++) {
      hasUnk |= getWords()[i] & REP10;
    }
    if (hasUnk)
      return;

    conv4To2State();
    normalize();
  }
  void conv2To4State() {
    if (numBits == 0)
      return;
    size_t outNumWords = round_up_div(2 * getNumBits(), WordBits);
    Container buf(outNumWords);

    for (size_t i = 0; i < getNumWords() - 1; i++) {
      buf[2 * i + 0] = unpack_bits(words[i]);
      buf[2 * i + 1] = unpack_bits(words[i] >> 16);
    }
    buf[2 * (getNumWords() - 1) + 0] = unpack_bits(words.back());
    if (auto idx = 2 * (getNumWords() - 1) + 1; idx < buf.size())
      buf[idx] = unpack_bits(words.back() >> 16);

    words = std::move(buf);
    numBits *= 2;
    Extend{field} = (Extend{field} & 1);
    Custom{field} = 1;
  }
  template <auto Func4S, auto Func2S, typename T0, typename T1>
  static void bitwiseOp4S(BigIntBase &out, const T0 &lhsMayAlias,
                          const T1 &rhs) {
    if (!lhsMayAlias.getIs4S() && !rhs.getIs4S()) {
      Func2S(out, lhsMayAlias, rhs);
      return;
    }
    const T0 *lhsPtr = &lhsMayAlias;
    BigIntBase lhsCopy;
    if constexpr (std::is_same_v<BigIntBase, T0>) {
      if (&out == &lhsMayAlias && !lhsMayAlias.getIs4S()) {
        lhsCopy = lhsMayAlias;
        lhsPtr = &lhsCopy;
      }
    }

    const T0 &lhs = *lhsPtr;

    out.words.resize(std::max(lhs.getNumWords(), rhs.getNumWords()));
    out.numBits = std::max(lhs.getRawNumBits(), rhs.getRawNumBits());

    uint32_t hasUnk = 0;

    for (size_t i = 0; i < out.getNumWords(); i++) {
      uint32_t lhsV = lhs.getWord4S(i);
      uint32_t rhsV = rhs.getWord4S(i);
      uint32_t outV = Func4S(lhsV, rhsV);
      out.words[i] = outV;
      hasUnk |= outV & REP10;
    }

    Custom{out.field} = 1;
    Extend{out.field} =
        Func4S(lhs.getIs4S() ? lhs.getExtend() : unpack_bits(lhs.getExtend()),
               rhs.getIs4S() ? rhs.getExtend() : unpack_bits(rhs.getExtend())) &
        0b11;

    if (!hasUnk && (!out.isExtended() || !(Extend{out.field} & 0b10)))
      out.conv4To2State();

    out.normalize();
  }
  template <BigIntAPI T0, BigIntAPI T1>
  static void andOp4S(BigIntBase &out, const T0 &lhs, const T1 &rhs) {
    return bitwiseOp4S<and_4state, andOp<T0, T1>, T0, T1>(out, lhs, rhs);
  }
  template <BigIntAPI T0, BigIntAPI T1>
  static void orOp4S(BigIntBase &out, const T0 &lhs, const T1 &rhs) {
    return bitwiseOp4S<or_4state, orOp<T0, T1>, T0, T1>(out, lhs, rhs);
  }
  template <BigIntAPI T0, BigIntAPI T1>
  static void xorOp4S(BigIntBase &out, const T0 &lhs, const T1 &rhs) {
    return bitwiseOp4S<xor_4state, xorOp<T0, T1>, T0, T1>(out, lhs, rhs);
  }
  template <BigIntAPI T0, BigIntAPI T1>
  static void xnorOp4S(BigIntBase &out, const T0 &lhs, const T1 &rhs) {
    return bitwiseOp4S<xnor_4state, xnorOp<T0, T1>, T0, T1>(out, lhs, rhs);
  }
  template <BigIntAPI T0> static void notOp4S(BigIntBase &out, const T0 &lhs) {
    return xorOp4S(out, lhs, PatBigInt{lhs.getRawNumBits(), 0b11});
  }

  template <typename T0, typename T1>
  static void bitsExactEqual4S(BigIntBase &out, const T0 &lhs, const T1 &rhs) {
    if (!lhs.getIs4S() && !rhs.getIs4S())
      return xnorOp(out, lhs, rhs);
    auto bits = lhs.getNumBits();
    assert(bits == rhs.getNumBits());

    out.numBits = 2 * bits;
    out.words.resize(round_up_div(out.numBits, WordBits));
    Custom{out.field} = 1;

    for (size_t i = 0; i < out.getNumWords(); i++) {
      uint32_t lhsV = lhs.getWord4S(i);
      uint32_t rhsV = rhs.getWord4S(i);
      uint32_t outV = n_equal_mask<2>(lhsV, rhsV) >> 1;
      out.words[i] = outV;
    }
    out.conv4To2State();
    out.normalize();
  }

#define LINEAR_OP_4S(ident, func2s)                                            \
  template <BigIntAPI T0, BigIntAPI T1>                                        \
  static void ident(BigIntBase &out, const T0 &lhs, const T1 &rhs) {           \
    if (lhs.getIs4S() || rhs.getIs4S()) {                                      \
      out.setRepeating(EXTX_MASK,                                              \
                       std::max(lhs.getRawNumBits(), rhs.getRawNumBits()), 1); \
      return;                                                                  \
    }                                                                          \
    func2s(out, lhs, rhs);                                                     \
  }

  LINEAR_OP_4S(addOp4S, addOp)
  LINEAR_OP_4S(subOp4S, subOp)

  template <BigIntAPI T0>
  static void shlOp4S(BigIntBase &out, const T0 &lhs, unsigned rhs) {
    shlOp(out, lhs, lhs.getIs4S() ? 2 * rhs : rhs);
    out.conv4To2StateIfPossible();
  }
  template <BigIntAPI T0>
  static void lshrOp4S(BigIntBase &out, const T0 &lhs, unsigned rhs) {
    lshrOp(out, lhs, lhs.getIs4S() ? 2 * rhs : rhs);
    out.conv4To2StateIfPossible();
  }
  template <BigIntAPI T0>
  static void ashrOp4S(BigIntBase &out, const T0 &lhs, unsigned rhs) {
    ashrOp(out, lhs, lhs.getIs4S() ? 2 * rhs : rhs);
    out.conv4To2StateIfPossible();
  }

  template <BigIntAPI T0, BigIntAPI T1>
  static void shlOp4S(BigIntBase &out, const T0 &lhs, const T1 &rhs) {
    if (rhs.getIs4S()) {
      out = PatBigInt::undef(lhs.getNumBits());
      return;
    }
    if (!rhs.getLimitedVal()) {
      out = PatBigInt::fromFourState(FourState::S0, lhs.getNumBits());
      return;
    }
    return shlOp4S(out, lhs, *rhs.getLimitedVal());
  }
  template <BigIntAPI T0, BigIntAPI T1>
  static void lshrOp4S(BigIntBase &out, const T0 &lhs, const T1 &rhs) {
    if (rhs.getIs4S()) {
      out = PatBigInt::undef(lhs.getNumBits());
      return;
    }
    if (!rhs.getLimitedVal()) {
      out = PatBigInt::fromFourState(FourState::S0, lhs.getNumBits());
      return;
    }
    return lshrOp4S(out, lhs, *rhs.getLimitedVal());
  }
  template <BigIntAPI T0, BigIntAPI T1>
  static void ashrOp4S(BigIntBase &out, const T0 &lhs, const T1 &rhs) {
    if (rhs.getIs4S()) {
      out = PatBigInt::undef(lhs.getNumBits());
      return;
    }
    if (!rhs.getLimitedVal()) {
      out = PatBigInt::fromSign(lhs, lhs.getNumBits());
      return;
    }
    return ashrOp4S(out, lhs, *rhs.getLimitedVal());
  }

  template <BigIntAPI T>
  constexpr static void resizeOp4S(BigIntBase &out, const T &lhs,
                                   uint32_t newSize, bool sign = false) {
    if (lhs.getIs4S()) {
      BigIntBase::resizeOp(out, lhs, 2 * newSize,
                           sign ? lhs.getExtendPatFromSignBit() : 0);
      out.conv4To2StateIfPossible();
    } else
      BigIntBase::resizeOp(out, lhs, newSize,
                           sign ? lhs.getExtendPatFromSignBit() : 0);
  }
  template <BigIntAPI T>
  static void rangeSelectOp4S(BigIntBase &out, const T &src, uint32_t bitOffs,
                              uint32_t bitLen) {
    if (src.getIs4S()) {
      BigIntBase::rangeSelectOp(out, src, bitOffs * 2, bitLen * 2);
      out.setCustom(1);
      out.conv4To2StateIfPossible();
    } else {
      BigIntBase::rangeSelectOp(out, src, bitOffs, bitLen);
    }
  }
  template <BigIntAPI T0, BigIntAPI T1>
  static auto udivmodOp4S(const T0 &lhs, const T1 &rhs) {
    if (lhs.getIs4S() || rhs.getIs4S()) {
      BigIntBase outA;
      BigIntBase outB;
      outA.setRepeating(EXTX_MASK, lhs.getRawNumBits(), 1);
      outB.setRepeating(EXTX_MASK, lhs.getRawNumBits(), 1);
      return std::make_pair(std::move(outA), std::move(outB));
    }
    auto [div, rem] = BigIntBase::udivmodOp(lhs, rhs);
    return std::make_pair(BigIntBase{div}, BigIntBase{rem});
  }
  template <BigIntAPI T0, BigIntAPI T1>
  static auto sdivmodOp4S(const T0 &lhs, const T1 &rhs) {
    if (lhs.getIs4S() || rhs.getIs4S() || rhs.valueEquals(0)) {
      BigIntBase outA;
      BigIntBase outB;
      outA.setRepeating(EXTX_MASK, lhs.getRawNumBits(), 1);
      outB.setRepeating(EXTX_MASK, lhs.getRawNumBits(), 1);
      return std::make_pair(std::move(outA), std::move(outB));
    }
    auto [div, rem] = BigIntBase::sdivmodOp(lhs, rhs);
    return std::make_pair(BigIntBase{div}, BigIntBase{rem});
  }
  template <BigIntAPI T0, BigIntAPI T1>
  static auto udivOp4S(const T0 &lhs, const T1 &rhs) {
    return udivmodOp4S(lhs, rhs).first;
  }
  template <BigIntAPI T0, BigIntAPI T1>
  static auto umodOp4S(const T0 &lhs, const T1 &rhs) {
    return udivmodOp4S(lhs, rhs).second;
  }
  template <BigIntAPI T0, BigIntAPI T1>
  static auto sdivOp4S(const T0 &lhs, const T1 &rhs) {
    return sdivmodOp4S(lhs, rhs).first;
  }
  template <BigIntAPI T0, BigIntAPI T1>
  static auto smodOp4S(const T0 &lhs, const T1 &rhs) {
    return sdivmodOp4S(lhs, rhs).second;
  }
  template <BigIntAPI T0, BigIntAPI T1, int Mode = 0>
  static auto mulOp4S(const T0 &lhs, const T1 &rhs) {
    if (lhs.getIs4S() || rhs.getIs4S()) {
      BigIntBase out;
      out.setRepeating(EXTX_MASK,
                       Mode == 0
                           ? std::max(lhs.getRawNumBits(), rhs.getRawNumBits())
                           : lhs.getRawNumBits() + rhs.getRawNumBits(),
                       1);
      return out;
    }
    return BigIntBase{BigIntBase::mulOp<Mode>(lhs, rhs)};
  }
  template <BigIntAPI T> static void negateOp4S(BigIntBase &out, const T &lhs) {
    subOp4S(out, PatBigInt(lhs.getRawNumBits(), 0), lhs);
  }

  template <BigIntAPI T> static FourState reductionXOROp4S(const T &val) {
    if (val.getIs4S())
      return FourState::SX;
    return reductionXOROp(val);
  }

  template <BigIntAPI T0, BigIntAPI T1>
  constexpr static void concatOp4S(BigIntBase &out, const T0 &lhs,
                                   const T1 &rhs) {
    bool custom = rhs.getIs4S() || lhs.getIs4S();
    if (lhs.getIs4S() && !rhs.getIs4S()) {
      BigIntBase rhsCopy{rhs};
      rhsCopy.conv2To4State();
      concatOp(out, lhs, rhsCopy);
    } else if (!lhs.getIs4S() && rhs.getIs4S()) {
      BigIntBase lhsCopy{lhs};
      lhsCopy.conv2To4State();
      concatOp(out, lhsCopy, rhs);
    } else
      concatOp(out, lhs, rhs);
    out.setCustom(custom);
  }

  template <BigIntAPI T0>
  static void repeatOp4S(BigIntBase &out, const T0 &val, uint32_t count) {
    bool custom = val.getIs4S();
    repeatOp(out, val, count);
    out.setCustom(custom);
  }

  // String Ops
  constexpr static std::optional<BigIntBase>
  parseHex(ArrayRef<const char> digits) {
    BigIntBase out = BigIntBase::ofLen(digits.size() * 4);
    size_t i = digits.size() - 1;
    for (const char digit : digits) {

      unsigned val;
      if (digit >= '0' && digit <= '9')
        val = digit - '0';
      else if (digit >= 'a' && digit <= 'f')
        val = digit - 'a' + 10;
      else if (digit >= 'A' && digit <= 'F')
        val = digit - 'A' + 10;
      else
        return std::nullopt;

      out.words[i / 8] |= (val << (i % 8) * 4);
      i--;
    }
    out.normalize();
    return out;
  }
  constexpr static std::optional<BigIntBase>
  parseDec(ArrayRef<const char> digits) {
    BigIntBase out = BigIntBase::ofLen(0);
    BigIntBase base = BigIntBase::fromU64Pruned(10);

    for (const char digit : digits) {

      uint64_t val;
      if (digit >= '0' && digit <= '9')
        val = digit - '0';
      else
        return std::nullopt;

      // extending multiply.
      out = BigIntBase::mulOp<1>(out, base);
      BigIntBase::addOp(out, out, BigIntBase::fromU64Pruned(val));
      // truncate off unused bits
      auto truncBits = out.numBits - BigIntBase::leadingZeros(out);
      if (truncBits != 0)
        BigIntBase::resizeOp(out, out, truncBits);
    }

    return out;
  }

  template <typename T>
  static void stream_hex(std::ostream &os, const T &self) {
    ssize_t hexDigits = (self.getRawNumBits() + 3) / 4;
    ssize_t wordHexDigits = self.getNumWords() * 8;

    uint32_t extend = repeatExtend(self.getExtend()) & 0xF;

    auto toHex = [](int n) -> char {
      return n > 9 ? (n + 'a' - 10) : (n + '0');
    };

    ssize_t extendDigits = hexDigits - wordHexDigits;
    for (ssize_t i = 0; i < extendDigits; i++) {
      size_t digit = hexDigits - i - 1;
      if (i == 0 && self.getRawNumBits() % 4 != 0)
        os << toHex(extend & ((1 << (self.getRawNumBits() % 4)) - 1));
      else
        os << toHex(extend);
      if (digit % 8 == 0)
        os << '_';
    }

    auto flags = os.flags();
    for (ssize_t i = self.getNumWords() - 1; i >= 0; i--) {
      uint32_t word = self.getWords()[i];
      size_t width = 8;

      if (i == self.getNumWords() - 1 && extendDigits < 0) {
        width = (hexDigits % 8);
        word &= (1 << (self.getRawNumBits() % 32)) - 1;
      }

      os << std::hex << std::setfill('0') << std::setw(width) << word;
      if (i != 0)
        os << '_';
    }
    os.flags(flags);
  }
  template <typename T>
  static void stream_hex_4s_vlog(std::ostream &os, const T &self) {
    if (!self.getIs4S())
      return BigIntBase::stream_hex(os, self);

    constexpr size_t digits_per_word = 4;
    constexpr size_t separator_per = 8;

    size_t realBits = self.getRawNumBits() / 2;
    size_t hexDigits = (realBits + 3) / 4;
    size_t wordHexDigits =
        std::min(self.getNumWords() * digits_per_word, hexDigits);

    uint32_t extend = repeatExtend(self.getExtend()) & 0xF;

    auto toHex4S = [](uint8_t n) -> char {
      if (n == uint8_t(EXTX_MASK))
        return 'x';
      if (n == uint8_t(EXTZ_MASK))
        return 'z';
      if (auto mask = n & REP10) {
        if ((mask >> 1) & n)
          return 'X';
        return 'Z';
      }
      n = pack_bits(n);
      return n > 9 ? (n + 'a' - 10) : (n + '0');
    };

    ssize_t extendDigits = hexDigits - wordHexDigits;
    for (ssize_t i = 0; i < extendDigits; i++) {
      size_t digit = hexDigits - i - 1;
      if (i == 0 && realBits % 4 != 0)
        os << toHex4S(extend & ((1 << (2 * (realBits % 4))) - 1));
      else
        os << toHex4S(extend);
      if (digit % separator_per == 0)
        os << '_';
    }

    for (ssize_t i = 0; i < (ssize_t)wordHexDigits; i++) {
      size_t digit = hexDigits - extendDigits - i - 1;
      size_t idx = (wordHexDigits - i - 1) / digits_per_word;
      size_t sub = (wordHexDigits - i - 1) % digits_per_word;

      uint32_t val = self.getWords()[idx] >> (sub * 8);
      os << toHex4S(val);

      if (digit != 0 && digit % separator_per == 0)
        os << "_";
    }
  }

  template <typename T>
  static void stream_bin(std::ostream &os, const T &self) {
    for (ssize_t i = self.getRawNumBits() - 1; i >= 0; i--) {
      os << (self.getBit(i) ? '1' : '0');
      if (i != 0 && (i % 8) == 0)
        os << "_";
    }
  }

  template <typename T>
  static void stream_bin_4s_vlog(std::ostream &os, const T &self) {
    std::array<char, 4> bitToStr = {'0', '1', 'z', 'x'};
    for (ssize_t i = (self.getRawNumBits() / 2) - 1; i >= 0; i--) {
      os << bitToStr[self.getBit(i)];
      if (i != 0 && (i % 8) == 0)
        os << "_";
    }
  }

  template <typename T>
  static void stream_dec(std::ostream &os, const T &self,
                         bool isSigned = false) {
    if (self.getRawSignBit() && isSigned) {
      BigIntBase temp;
      BigIntBase::negateOp(temp, self);
      os << "-";
      stream_dec(os, temp);
      return;
    }
    SmallVec<uint8_t, 32> str;

    BigIntBase q = BigIntBase::fromU64Pruned(10);
    BigIntBase n{self};
    BigIntBase r;

    do {
      std::tie(n, r) = BigIntBase::udivmodOp(n, q);
      str.emplace_back(r.getWord(0));
    } while (n != BigIntBase::fromU64(0, n.getRawNumBits()));

    for (ssize_t i = str.size() - 1; i >= 0; i--)
      os << uint32_t(str[i]);
  }

  constexpr void prune() {
    uint32_t extendMask = repeatExtend(Extend{field});
    while (words.size() != 1 && words.back() == extendMask)
      words.pop_back();
    if constexpr (requires(BigIntBase b) { b.words.try_to_inline(); })
      words.try_to_inline();
  }

  enum class ParseError {
    LENGTH_DOES_NOT_FIT_IN_32,
    UNKNOWN_BASE,
    ILLEGAL_DIGIT,
    TOO_MANY_DIGITS_GIVEN
  };

  constexpr static BigIntBase parseDec(const char *&ptr) {
    auto isdigit = [](char c) { return c >= '0' && c <= '9'; };
    BigIntBase init = fromU64Pruned(0);
    while (true) {
      if (!isdigit(*ptr))
        break;
      char digit = *ptr++;

      digit -= '0';
      assert(digit <= 9);

      // extending multiply.
      init = BigIntBase::mulOp<1>(init, BigIntBase::fromU64Pruned(10));
      BigIntBase::addOp(init, init, BigIntBase::fromU64Pruned(digit));
      // truncate off unused bits

      auto truncBits = init.numBits - BigIntBase::leadingZeros(init);
      if (truncBits != 0)
        BigIntBase::resizeOp(init, init, truncBits);
    }
    return init;
  }

  struct ParseVlogResult {
    BigIntBase bigInt;
    bool isSigned;
    enum Type : uint8_t { SIMPLE, UNSIZED, SIZED };
    Type type;
  };

  constexpr static std::expected<ParseVlogResult, ParseError>
  parseVlog(const char *&ptr) {
    // todo: optimize this.
    auto isxdigit = [](char c) {
      return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') ||
             (c >= 'A' && c <= 'F');
    };
    auto isNumDigit = [isxdigit](int c) {
      return isxdigit(c) || c == 'x' || c == 'X' || c == 'z' || c == 'Z';
    };

    Optional<uint> numBits;

    if (*ptr != '\'') {
      // first parse simple dec literal. this is either the final value or the
      // size of a following based & sized literal.
      BigIntBase init = parseDec(ptr);

      if (*ptr != '\'') {
        // treat as unsigned 32 bit.
        BigIntBase::resizeOp(init, init, 32, 0);
        return ParseVlogResult(init, false, ParseVlogResult::SIMPLE);
      }

      if (init.numBits > 32)
        return std::unexpected(ParseError::LENGTH_DOES_NOT_FIT_IN_32);

      numBits = init.getExactVal();
    }
    ptr++;

    if (numBits == 0)
      return ParseVlogResult{BigInt::fromU64(0, 0), false,
                             ParseVlogResult::SIZED};

    bool isSigned = false;
    if (*ptr == 's') {
      ptr++;
      isSigned = true;
    }

    uint base;
    switch (*ptr) {
    case 'h':
      base = 16;
      break;
    case 'd':
      base = 10;
      break;
    case 'o':
      base = 8;
      break;
    case 'b':
      base = 2;
      break;

    // unsized/unbased. these are returned to fill all bits when extending.
    case '0':
      ptr++;
      if (isNumDigit(*ptr))
        return std::unexpected(ParseError::TOO_MANY_DIGITS_GIVEN);
      return ParseVlogResult(BigInt::fromU64(0, 1), true,
                             ParseVlogResult::UNSIZED);
    case '1':
      ptr++;
      if (isNumDigit(*ptr))
        return std::unexpected(ParseError::TOO_MANY_DIGITS_GIVEN);
      return ParseVlogResult(BigInt::fromU64(1, 1), true,
                             ParseVlogResult::UNSIZED);
    case 'x':
      ptr++;
      if (isNumDigit(*ptr))
        return std::unexpected(ParseError::TOO_MANY_DIGITS_GIVEN);
      return ParseVlogResult(PatBigInt{2, FourState::SX, 1}, true,
                             ParseVlogResult::UNSIZED);
    case 'z':
      ptr++;
      if (isNumDigit(*ptr))
        return std::unexpected(ParseError::TOO_MANY_DIGITS_GIVEN);
      return ParseVlogResult(PatBigInt{2, FourState::SZ, 1}, true,
                             ParseVlogResult::UNSIZED);
    default:
      return std::unexpected(ParseError::UNKNOWN_BASE);
    }
    ptr++;

    uint baseBits = clog2(base);

    BigIntBase acc = BigIntBase::fromU64Pruned(0);

    while (true) {
      int c = *ptr;
      if (!isNumDigit(c))
        break;
      ptr++;

      BigIntBase digit;
      // these are used when printing to say that some bits in the digit are
      // x/z. info lost so can't parse.
      if (c == 'X' || c == 'Z')
        return std::unexpected(ParseError::ILLEGAL_DIGIT);
      // we can't set log2(10) bits to x/z.
      if (base == 10 && (c == 'x' || c == 'z'))
        return std::unexpected(ParseError::ILLEGAL_DIGIT);
      if (c == 'x')
        digit = PatBigInt{2 * baseBits, FourState::SX, 1};
      if (c == 'z')
        digit = PatBigInt{2 * baseBits, FourState::SZ, 1};

      uint hexVal;
      if (c >= 'a')
        hexVal = c - 'a' + 10;
      else if (c >= 'A')
        hexVal = c - 'A' + 10;
      else
        hexVal = c - '0';

      digit = BigIntBase::fromU64(hexVal, baseBits);

      if (base == 10) {
        assert(!digit.getIs4S());
        acc = BigIntBase::mulOp<1>(acc, BigIntBase::fromU64Pruned(10));
        BigIntBase::addOp(acc, acc, digit);
        BigIntBase::resizeOp(acc, acc,
                             acc.numBits - BigIntBase::leadingZeros(acc));
        continue;
      } else {
        // fixme: super slow bc we're shifting digits each time
        BigIntBase::concatOp4S(acc, acc, digit);
      }
    }

    if (numBits) {
      if (acc.numBits > *numBits) {
        auto leading = BigIntBase::leadingZeros4S(acc);
        if (!leading || acc.numBits - *leading > *numBits)
          return std::unexpected(ParseError::TOO_MANY_DIGITS_GIVEN);
        BigIntBase::resizeOp4S(acc, acc, *numBits, false);
      }
      if (acc.numBits < *numBits)
        BigIntBase::resizeOp4S(acc, acc, *numBits, isSigned);
    }
    return ParseVlogResult(acc, isSigned,
                           numBits ? ParseVlogResult::SIZED
                                   : ParseVlogResult::UNSIZED);
  }

  template <typename T, std::invocable<BigInt &, const BigInt &,
                                       const std::iter_value_t<T> &>
                            FuncT>
  static void reduce(BigInt &acc, Range<T> list, FuncT func) {
    for (auto num : list) {
      func(acc, acc, num);
    }
  }

  template <typename T,
            std::invocable<const BigInt &, const std::iter_value_t<T> &> FuncT>
  static void reduce(BigInt &acc, Range<T> list, FuncT func) {
    for (auto num : list) {
      acc = func(acc, num);
    }
  }

private:
  constexpr void normalizeExtend() {
    if (!isExtended())
      Extend{field} = 0;
    setLastWordBitsToPattern(Extend{field});
  }

  constexpr void setLastWordBitsToPattern(uint8_t pattern) {
    // truncate last word and fill with extend.
    if (words.size() == getExtNumWords() && getRawNumBits() % 32 != 0) {
      auto mask = (1 << (getRawNumBits() % 32)) - 1;
      words.back() &= mask;
      words.back() |= (~mask & repeatExtend(pattern));
    }
  }

  constexpr void normalize() {
    assert(words.size() <= getExtNumWords() ||
           (numBits == 0 && words.size() == 1));
    if (getNumWords() * bit_mask_sz<uint32_t> < numBits)
      prune();
    else if (getNumWords() * bit_mask_sz<uint32_t> >= numBits) {
      Extend{field} =
          (words.back() & bit_mask_ms_nbits<uint32_t>(BigIntExtendBits)) >>
          (bit_mask_sz<uint32_t> - BigIntExtendBits);
      normalizeExtend();
      prune();
    }
  }

  // opposite of normalize
  constexpr void expand() {
    auto oldSize = getNumWords();
    words.resize(getExtNumWords());
    std::fill(words.begin() + oldSize, words.end(),
              repeatExtend(Extend{field}));
  }

  template <auto OpFunc, typename T0, typename T1>
  constexpr static void carryPropOp(BigIntBase &out, const T0 &lhs,
                                    const T1 &rhs) {

    // assert(lhs.getRawNumBits() == rhs.getRawNumBits());
    unsigned maxNumBits = std::max(lhs.getRawNumBits(), rhs.getRawNumBits());
    unsigned numWords = std::max(lhs.getNumWords(), rhs.getNumWords());
    unsigned minNumWords = std::min(lhs.getNumWords(), rhs.getNumWords());

    // out might be lhs or rhs so this must not erase data
    out.words.resize(numWords);

    unsigned carry = 0;

    // fast loop.
    for (size_t i = 0; i < minNumWords; i++)
      out.words[i] =
          OpFunc(lhs.getWords()[i], rhs.getWords()[i], carry, &carry);

    // slow loop. could also hoist if's here.
    for (size_t i = minNumWords; i < numWords; i++) {
      unsigned lhsVal, rhsVal;

      if (i >= lhs.getNumWords())
        lhsVal = repeatExtend(lhs.getExtend());
      else
        lhsVal = lhs.getWords()[i];

      if (i >= rhs.getNumWords())
        rhsVal = repeatExtend(rhs.getExtend());
      else
        rhsVal = rhs.getWords()[i];

      out.words[i] = OpFunc(lhsVal, rhsVal, carry, &carry);
    }

    // zext/sext is purely an optimization in the container, so sext of result
    // is simply what those bits would add up to. Signed/unsigned is layer
    // above.
    unsigned extend = OpFunc(lhs.getExtend(), rhs.getExtend(), carry, &carry);

    Extend{out.field} = extend & bit_mask_ones<uint8_t>(BigIntExtendBits);
    out.numBits = maxNumBits;
    out.normalize();
  }
};

static constexpr CBigInt operator""_b(const char *str) {
  std::optional<CBigInt> rv = std::nullopt;
  if (str[0] == '0' && str[1] == 'x') {
    rv = CBigInt::parseHex(
        ArrayRef<const char>{str + 2, std::char_traits<char>::length(str) - 2});
  } else
    rv = CBigInt::parseDec(ArrayRef{str, std::char_traits<char>::length(str)});
  assert(rv && "invalid str literal");
  return *rv;
}

static constexpr CBigInt operator""_bv(const char *str, size_t sz) {
  auto res = CBigInt::parseVlog(str);
  assert(res);
  return res.value().bigInt;
}

#define LINEAR_OP_OPERATOR_INST(identSimple, identAssign, func)                \
  template <BigIntAPI T0, BigIntAPI T1>                                        \
  inline BigInt identSimple(const T0 &lhs, const T1 &rhs) {                    \
    BigInt out;                                                                \
    BigInt::func(out, lhs, rhs);                                               \
    return out;                                                                \
  }                                                                            \
  template <BigIntAPI T>                                                       \
  inline BigInt &identAssign(BigInt &lhs, const T &rhs) {                      \
    BigInt::func(lhs, lhs, rhs);                                               \
    return lhs;                                                                \
  }
LINEAR_OP_OPERATOR_INST(operator+, operator+=, addOp4S)
LINEAR_OP_OPERATOR_INST(operator-, operator-=, subOp4S)
LINEAR_OP_OPERATOR_INST(operator&, operator&=, andOp4S)
LINEAR_OP_OPERATOR_INST(operator|, operator|=, orOp4S)
LINEAR_OP_OPERATOR_INST(operator^, operator^=, xorOp4S)

#define SHIFT_OP_OPERATOR_INST(identSimple, identAssign, func)                 \
  template <BigIntAPI T0>                                                      \
  inline BigInt identSimple(const T0 &lhs, uint32_t rhs) {                     \
    BigInt out;                                                                \
    BigInt::func(out, lhs, rhs);                                               \
    return out;                                                                \
  }                                                                            \
  inline BigInt &identAssign(BigInt &lhs, uint32_t rhs) {                      \
    BigInt::func(lhs, lhs, rhs);                                               \
    return lhs;                                                                \
  }
SHIFT_OP_OPERATOR_INST(operator<<, operator<<=, shlOp4S)
SHIFT_OP_OPERATOR_INST(operator>>, operator>>=, lshrOp4S)

template <BigIntAPI T0, BigIntAPI T1>
inline BigInt operator*(const T0 &lhs, const T1 &rhs) {
  return BigInt::mulOp<0>(lhs, rhs);
}
template <BigIntAPI T0, BigIntAPI T1>
inline BigInt operator/(const T0 &lhs, const T1 &rhs) {
  return BigInt::udivmodOp<0>(lhs, rhs).first;
}
template <BigIntAPI T0, BigIntAPI T1>
inline BigInt operator%(const T0 &lhs, const T1 &rhs) {
  return BigInt::udivmodOp<0>(lhs, rhs).second;
}

#define OP_ASSIGN_TRIVIAL(identAssign, operator)                               \
  template <BigIntAPI T>                                                       \
  inline BigInt &identAssign(BigInt &lhs, const T &rhs) {                      \
    auto res = lhs operator rhs;                                               \
    lhs = std::move(res);                                                      \
    return lhs;                                                                \
  }
OP_ASSIGN_TRIVIAL(operator*=, *)
OP_ASSIGN_TRIVIAL(operator/=, /)
OP_ASSIGN_TRIVIAL(operator%=, %)

template <BigIntAPI T0> inline BigInt operator-(const T0 &val) {
  BigInt out;
  BigInt::negateOp(out, val);
  return out;
}

template <typename Derived>
void BigIntMixin<Derived>::toStream(std::ostream &os, int base,
                                    bool unsized) const {
  if (self().getNumBits() == 0) {
    os << "0'";
    return;
  }

  const char *baseStr = (base == 16 ? "h" : (base == 10 ? "d" : "b"));
  if (self().getIs4S()) {
    if (!unsized)
      os << self().getRawNumBits() / 2 << "'" << baseStr;

    if (base == 2) {
      BigInt::stream_bin_4s_vlog(os, self());
      return;
    }
    BigInt val{self()};
    BigInt unk{self()};
    val.conv4To2State();
    unk.conv4To2StateHi();
    val.toStream(os, base, true);
    os << "?";
    unk.toStream(os, base, true);
  } else {
    if (!unsized)
      os << self().getRawNumBits() << "'" << baseStr;
    if (base == 16) {
      BigInt::stream_hex(os, self());
    } else if (base == 10) {
      BigInt::stream_dec(os, self());
    } else if (base == 2) {
      BigInt::stream_bin(os, self());
    } else
      dyno_unreachable("only hex, dec and bin supported");
  }
}

class alignas(uint64_t) Constant : public TrailingObjArr<Constant, uint32_t> {
  // we store u32's here st inline storage can use the same API with numWords
  // =
  // 1
  friend class ConstantRef;
  // num bits is detached from actual storage words bc we can sign/zext.
  uint32_t numBits;
  uint32_t field;

  static constexpr size_t AddrSize =
      bit_mask_sz<uint32_t> - BigIntCustomBits - BigIntExtendBits;

  template <std::integral T> using AddrField = BitField<T, AddrSize, 0>;
  template <std::integral T>
  using ExtendField = BitField<T, BigIntExtendBits, AddrSize>;
  template <std::integral T>
  using CustomField =
      BitField<T, BigIntCustomBits, AddrSize + BigIntExtendBits>;

public:
  Constant(DynObjRef ref, size_t sz, const BigInt &bigInt)
      : numBits(bigInt.getRawNumBits()) {

    AddrField{field}.set(bigInt.getNumWords());
    ExtendField{field}.set(bigInt.getExtend());
    CustomField{field}.set(bigInt.getIs4S());

    std::copy(bigInt.getWords().begin(), bigInt.getWords().end(), trailing());
  }

  Constant(const Constant &) = delete;
  Constant(Constant &&) = delete;
  Constant &operator=(const Constant &) = delete;
  Constant &operator=(Constant &&) = delete;

private:
  size_t getNumWords() const { return AddrField{field}; }
  uint8_t getExtend() const { return ExtendField{field}; }
  uint8_t getIs4S() const { return CustomField{field}; }
};
static_assert(TrailingObj<Constant>);
template <> struct ObjTraits<Constant> {
  // static constexpr DialectID dialect{DIALECT_CORE};
  static constexpr DialectType ty{CORE_CONSTANT};
  using FatRefT = ConstantRef;
};

class ConstantRef : public FatObjRef<Constant>,
                    public BigIntMixin<ConstantRef> {
protected:
  using Custom =
      FatObjRef::CustomField<BigIntCustomBits, 16 - BigIntCustomBits>;

public:
  using IsInline = FatObjRef::CustomField<1, 0>;
  using ExtPattern = FatObjRef::CustomField<BigIntExtendBits, 1>;
  using NBits = FatObjRef::CustomField<15 - BigIntExtendBits - BigIntCustomBits,
                                       1 + BigIntExtendBits>;
  using FatObjRef<Constant>::FatObjRef;

  ConstantRef(FatObjRef<Constant> ref) : FatObjRef<Constant>(ref) {}
  explicit ConstantRef(DynObjRef ref)
      : FatObjRef<Constant>(ref.getObjID(), nullptr, ref.getCustom()) {
    assert(ref.is<ObjRef<Constant>>());
    assert(this->isInline());
  }

  ConstantRef(unsigned n, uint32_t val, uint8_t extPattern, uint8_t custom)
      : FatObjRef<Constant>() {
    customField<IsInline>() = true;
    customField<NBits>() = n;
    customField<ExtPattern>() = extPattern;
    customField<Custom>() = custom;
    obj = ObjID{val};
  }

  static ConstantRef fromU32(uint32_t val) {
    return ConstantRef{32, val, 0, 0};
  }
  static ConstantRef fromFourState(FourState f) {
    if (f.isUnk())
      return ConstantRef{2, f, 0, 1};
    return ConstantRef{1, (bool)f, 0, 0};
  }
  static ConstantRef fromBool(bool b) { return ConstantRef{1, b, 0, 0}; }

  static ConstantRef undef32() {
    return ConstantRef{32, BigInt::EXTX_MASK, FourState::SX, 1};
  }
  static ConstantRef zeroBitZero() { return ConstantRef{0, 0, 0, 0}; }
  // static ConstantRef zero(uint32_t bits) { return ConstantRef{bits, 0, 0,
  // 0}; } template <typename T> static ConstantRef zeroLike(const T &t) {
  //   return zero(t.getNumBits());
  // }
  // static ConstantRef one(uint32_t bits) { return ConstantRef{bits, 1, 0,
  // 0}; } template <typename T> static ConstantRef oneLike(const T &t) {
  //   return zero(t.getNumBits());
  // }
  // static ConstantRef ones(uint32_t bits) {
  //   if (bits < WordBits)
  //     return ConstantRef{bits, bit_mask_ones<uint32_t>(bits), 0, 0};
  //   else
  //     return ConstantRef{bits, ~0U, 0b11, 0};
  // }
  // template <typename T> static ConstantRef onesLike(const T &t) {
  //   return zero(t.getNumBits());
  // }

  ArrayRef<uint32_t> getWords() const {
    return isInline() ? ArrayRef<uint32_t>{&obj.num, 1}
                      : ArrayRef<uint32_t>{ptr->trailing(), getNumWords()};
  }
  uint8_t getExtend() const {
    return isInline() ? customField<ExtPattern>() : ptr->getExtend();
  }
  uint8_t getIs4S() const {
    return isInline() ? customField<Custom>() : ptr->getIs4S();
  }
  unsigned getNumWords() const { return isInline() ? 1 : ptr->getNumWords(); };

  uint32_t getRawNumBits() const {
    return customField<IsInline>() ? customField<NBits>() : ptr->numBits;
  };

  bool isInline() const { return customField<IsInline>(); }
  uint32_t valInline() const {
    assert(isInline());
    return obj.num;
  }
};

class ConstantStore {
  NewDeleteObjStore<Constant> store;

  // Just using hash as a key rather than the Constant itself. We want to do
  // the full key compare part manually so that we can compare with BigInt
  // rather than Constant.
  // std::unordered_multimap<uint32_t, ObjRef<Constant>> map;
  DenseMultimap<uint32_t, ObjRef<Constant>> map;

public:
  template <typename T> uint32_t constantHash(const T &constant) {
    uint32_t acc = 0;
    acc ^= hash_u32(constant.getIs4S());
    acc ^= hash_u32(constant.getExtend());
    acc ^= hash_u32(constant.getRawNumBits());
    acc ^= hash_u32(constant.getNumWords());
    for (const auto word : constant.getWords())
      acc ^= hash_u32(word);
    return acc;
  }

  ConstantRef findOrInsert(const BigInt &bigInt) {
    uint32_t hash = constantHash(bigInt);
    auto it = map.find(hash);

    for (; it != map.end(); it = map.find_next(it)) {
      auto ref = store.resolve(it.val());
      if (bigInt == ConstantRef{ref})
        return ref;
    }

    auto ref = store.create(bigInt.getNumWords(), bigInt);
    map.insert(hash, ref);
    return ref;
  }

  Constant &operator[](ObjRef<Constant> ref) { return store[ref]; }
  void destroy(FatObjRef<Constant> ref) { return store.destroy(ref); }
  ConstantRef resolve(ObjRef<Constant> ref) { return store.resolve(ref); }
  ConstantRef resolve(DynObjRef ref) {
    if (ref.isCustom())
      return ConstantRef{ref};
    return store.resolve(ref.as<ObjRef<Constant>>());
  }
  bool exists(ObjRef<Constant> ref) { return store.exists(ref); }
  auto numIDs() { return store.numIDs(); }
};

class GenericBigIntRef : public BigIntMixin<GenericBigIntRef> {
  friend class Constant;
  friend class ConstantStore;
  friend class BigIntMixin<PatBigInt>;
  friend class BigIntBase<SmallVec<uint32_t, 4>>;
  friend class BigIntBase<CexprVec<uint32_t, 16>>;

  union {
    ArrayRef<uint32_t> words;
    uint32_t inlineWord;
  };
  uint32_t numBits;
  uint8_t field;

public:
  template <typename T> using Extend = BitField<T, BigIntExtendBits, 0>;
  template <typename T>
  using Custom = BitField<T, BigIntCustomBits, BigIntExtendBits>;
  template <typename T>
  using Inline = BitField<T, 1, BigIntExtendBits + BigIntCustomBits>;

  constexpr unsigned getNumWords() const {
    return Inline{field} ? 1 : words.size();
  }
  constexpr uint32_t getRawNumBits() const { return numBits; }
  constexpr uint8_t getExtend() const { return Extend{field}; }
  constexpr uint8_t getIs4S() const { return Custom{field}; }
  constexpr ArrayRef<uint32_t> getWords() const {
    return Inline{field} ? ArrayRef{&inlineWord, 1} : words;
  }

  explicit GenericBigIntRef(const BigInt &bigInt)
      : words(bigInt.getWords()), numBits(bigInt.getRawNumBits()), field(0) {
    Extend{field} = bigInt.getExtend();
    Custom{field} = bigInt.getIs4S();
    Inline{field} = 0;
  }
  explicit GenericBigIntRef(ConstantRef ref)
      : numBits(ref.getRawNumBits()), field(0) {
    Extend{field} = ref.getExtend();
    Custom{field} = ref.getIs4S();
    Inline{field} = ref.isInline();
    if (ref.isInline())
      inlineWord = ref.getWords()[0];
    else
      words = ref.getWords();
  }
};

template <typename T> class ConstantBuilderBase {
protected:
  ConstantStore &store;
  T cur;

public:
  ConstantBuilderBase(ConstantStore &store) : store(store) {}

  ConstantBuilderBase &val(unsigned bits, uint64_t value64 = 0) {
    cur.set(value64, bits);
    return *this;
  }
  ConstantBuilderBase &val(unsigned bits, uint32_t value32, bool sign) {
    cur.set(value32, bits);
    if (sign && int32_t(value32) < 0)
      cur.setExtend(bit_mask_ones<uint32_t>(BigIntExtendBits));
    return *this;
  }
  ConstantBuilderBase &raw(unsigned bits, ArrayRef<uint32_t> data,
                           uint8_t extend = 0, uint8_t custom = 0) {
    cur = BigInt::fromRaw(data, bits, extend, custom);
    return *this;
  }
  ConstantBuilderBase &raw(unsigned bits, SmallVecImpl<uint32_t> &&data,
                           uint8_t extend = 0, uint8_t custom = 0) {
    cur = BigInt::fromRaw(std::move(data), bits, extend, custom);
    return *this;
  }
  ConstantBuilderBase &ones(unsigned bits) {
    cur.set(uint32_t(~0), bits);
    cur.setExtend(bit_mask_ones<uint8_t>(BigIntExtendBits));
    return *this;
  }
  ConstantBuilderBase &zero(unsigned bits) {
    cur.set(uint32_t(0), bits);
    return *this;
  }
  ConstantBuilderBase &one(unsigned bits) {
    cur.set(uint32_t(1), bits);
    return *this;
  }
  template <typename U> ConstantBuilderBase &onesLike(U like) {
    cur.set(uint32_t(~0ULL), (uint32_t)like.getNumBits());
    cur.setExtend(bit_mask_ones<uint8_t>(BigIntExtendBits));
    return *this;
  }
  template <typename U> ConstantBuilderBase &zeroLike(U like) {
    cur.set(uint32_t(0), (uint32_t)like.getNumBits());
    return *this;
  }
  template <typename U> ConstantBuilderBase &oneLike(U like) {
    cur.set(uint32_t(1), (uint32_t)like.getNumBits());
    return *this;
  }

  template <BigIntAPI U> ConstantBuilderBase &val(const U &val) {
    cur = val;
    return *this;
  }
  ConstantBuilderBase &undef(unsigned bits) {
    cur.setRepeating(BigInt::EXTX_MASK, 2 * bits, 1);
    return *this;
  }
  ConstantBuilderBase &fourState() {
    cur.setCustom(1);
    return *this;
  }
  ConstantBuilderBase &twoState() {
    cur.setCustom(0);
    return *this;
  }
  template <BigIntAPI U>
  ConstantBuilderBase &valRange(const U &src, uint32_t bitOffs,
                                uint32_t bitLen) {
    BigInt::rangeSelectOp4S(cur, src, bitOffs, bitLen);
    return *this;
  }

#define SIMPLE_OP(ident, impl)                                                 \
  template <BigIntAPI U> ConstantBuilderBase &ident(const U &rhs) {            \
    BigInt::impl(cur, cur, rhs);                                               \
    return *this;                                                              \
  }                                                                            \
  ConstantBuilderBase &ident(uint64_t rhs) {                                   \
    BigInt::impl(cur, cur, BigInt::fromU64Pruned(rhs));                        \
    return *this;                                                              \
  }
  SIMPLE_OP(add, addOp4S)
  SIMPLE_OP(sub, subOp4S)
  SIMPLE_OP(bitAND, andOp4S)
  SIMPLE_OP(bitOR, orOp4S)
  SIMPLE_OP(bitXOR, xorOp4S)
  SIMPLE_OP(bitXNOR, xnorOp4S)

#define INT_RHS_OP(ident, impl)                                                \
  ConstantBuilderBase &ident(uint32_t rhs) {                                   \
    BigInt::impl(cur, cur, rhs);                                               \
    return *this;                                                              \
  }                                                                            \
  template <BigIntAPI U> ConstantBuilderBase &ident(const U &rhs) {            \
    BigInt::impl(cur, cur, rhs.getExactVal());                                 \
                                                                               \
    return *this;                                                              \
  }

  INT_RHS_OP(shl, shlOp4S)
  INT_RHS_OP(ashr, ashrOp4S)
  INT_RHS_OP(lshr, lshrOp4S)

  ConstantBuilderBase &neg() {
    BigInt::negateOp4S(cur, cur);
    return *this;
  }
  ConstantBuilderBase &bitNOT() {
    BigInt::xorOp4S(cur, cur, PatBigInt{cur.getNumBits(), 0b11});
    return *this;
  }
  template <BigIntAPI U> ConstantBuilderBase &mul(const U &rhs) {
    cur = BigInt::mulOp4S(cur, rhs);
    return *this;
  }
  template <BigIntAPI U> ConstantBuilderBase &udiv(const U &rhs) {
    cur = BigInt::udivmodOp4S(cur, rhs).first;
    return *this;
  }
  template <BigIntAPI U> ConstantBuilderBase &umod(const U &rhs) {
    cur = BigInt::udivmodOp4S(cur, rhs).second;
    return *this;
  }
  template <BigIntAPI U> ConstantBuilderBase &sdiv(const U &rhs) {
    cur = BigInt::sdivmodOp4S(cur, rhs).first;
    return *this;
  }
  template <BigIntAPI U> ConstantBuilderBase &smod(const U &rhs) {
    cur = BigInt::sdivmodOp4S(cur, rhs).second;
    return *this;
  }

  template <BigIntAPI U> ConstantBuilderBase &spow(const U &rhs) {
    cur = BigInt::spowOp4S(cur, rhs);
    return *this;
  }
  template <BigIntAPI U> ConstantBuilderBase &upow(const U &rhs) {
    cur = BigInt::upowOp4S(cur, rhs);
    return *this;
  }

  ConstantBuilderBase &resize(uint32_t size, bool sign = false) {
    BigInt::resizeOp4S(cur, cur, size, sign);
    return *this;
  }
  ConstantBuilderBase &repeat(uint32_t count) {
    BigInt::repeatOp4S(cur, cur, count);
    return *this;
  }

  template <BigIntAPI U> ConstantBuilderBase &concat(const U &other) {
    BigInt::concatOp4S(cur, other, cur);
    return *this;
  }
  template <BigIntAPI U> ConstantBuilderBase &concatLHS(const U &other) {
    BigInt::concatOp4S(cur, cur, other);
    return *this;
  }

  template <BigIntAPI U>
  ConstantBuilderBase &concatRange(const U &src, uint32_t bitOffs,
                                   uint32_t bitLen) {
    BigInt tmp;
    BigInt::rangeSelectOp4S(tmp, src, bitOffs, bitLen);
    BigInt::concatOp4S(cur, tmp, cur);
    return *this;
  }

  template <BigIntAPI U>
  ConstantBuilderBase &concatRangeLHS(const U &src, uint32_t bitOffs,
                                      uint32_t bitLen) {
    BigInt tmp;
    BigInt::rangeSelectOp4S(tmp, src, bitOffs, bitLen);
    BigInt::concatOp4S(cur, cur, tmp);
    return *this;
  }

  ConstantRef get() {
    if (cur.getNumWords() == 1 &&
        cur.getRawNumBits() < (1ULL << ConstantRef::NBits::size))
      return ConstantRef{cur.getRawNumBits(), cur.getWords()[0],
                         cur.getExtend(), cur.getIs4S()};

    return store.findOrInsert(cur);
  }

  BigInt getBigInt() { return cur; }

  operator ConstantRef() { return get(); }
};

using ConstantBuilder = ConstantBuilderBase<BigInt>;

} // namespace dyno
