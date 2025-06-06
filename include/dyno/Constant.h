#pragma once

#include "support/Bits.h"
#include "support/DenseMultimap.h"
#include "support/Optional.h"
#include "support/SmallVec.h"
#include "support/Utility.h"
#include <algorithm>
#include <cmath>
#include <concepts>
#include <cstdint>
#include <dyno/IDs.h>
#include <dyno/NewDeleteObjStore.h>
#include <dyno/Obj.h>
#include <iomanip>
#include <ios>
#include <iostream>
#include <ostream>
#include <span>
#include <string>
#include <type_traits>

namespace dyno {

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

class BigInt;
template <typename Derived> class BigIntMixin;

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
  friend class BigInt;

  // Base API for BigInt is getRawNumBits, getNumWords, getExtend, getIs4S and
  // getWords. This is for utility functions using that API that may be shared
  // by all implementing BigInt API.
  Derived &self() { return *static_cast<Derived *>(this); }
  const Derived &self() const { return *static_cast<const Derived *>(this); }

protected:
  static uint32_t bitsToWords(uint32_t bits) {
    return round_up_div(bits, WordBits);
  }
  static constexpr unsigned repeatExtend(uint32_t num) {
    return repeatBits(num, BigIntExtendBits);
  }
  uint32_t getExtNumWords() const {
    return bitsToWords(self().getRawNumBits());
  }
  uint32_t __attribute__((always_inline)) getWord(uint32_t i) const {
    if (i >= bitsToWords(self().getRawNumBits()))
      dyno_unreachable("out of bounds");

    if (i >= self().getNumWords())
      return repeatExtend(self().getExtend());
    return self().getWords()[i];
  }
  uint32_t __attribute__((always_inline)) getWord4S(uint32_t i) const {
    return self().getIs4S() ? getWord(i)
                            : unpack_bits(getWord(i / 2) >> ((i % 2) * 16));
  }
  uint8_t getRawBit(uint32_t i) const {
    assert(i <= self().getRawNumBits());
    return !!(getWord(i / WordBits) & (1 << (i % WordBits)));
  }

  uint8_t getRawSignBit() const {
    return getRawBit(self().getRawNumBits() - 1);
  }
  bool isExtended() const { return self().getNumWords() != getExtNumWords(); }

  uint8_t getExtendPatFromSignBit() const {
    if (self().getIs4S()) {
      return getSignBit();
    } else
      return getSignBit() ? 0b11 : 0;
  }

public:
  FourState getBit(uint32_t i) const {
    assert(i <= getNumBits());
    if (!self().getIs4S())
      return !!(getWord(i / WordBits) & (1 << (i % WordBits)));
    i *= 2;
    assert(i <= self().getRawNumBits());
    return (getWord(i / WordBits) >> (i % WordBits)) &
           bit_mask_ones<uint32_t>(BigIntExtendBits);
  }
  uint8_t getSignBit() const { return getBit(self().getNumBits() - 1); }
  uint32_t getNumBits() const {
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
  friend bool operator==(const Derived &lhs, const T &rhs) {
    if (!(lhs.getRawNumBits() == rhs.getRawNumBits() &&
          lhs.getNumWords() == rhs.getNumWords() &&
          lhs.getExtend() == rhs.getExtend() && lhs.getIs4S() == rhs.getIs4S()))
      return false;

    for (size_t i = 0; i < lhs.getNumWords(); i++)
      if (lhs.getWords()[i] != rhs.getWords()[i])
        return false;
    return true;
  }

  bool valueEquals(uint32_t val) const {
    if (auto val2 = getLimitedVal())
      return val == *val2;
    return false;
  }
  bool valueEqualsS(int32_t val) const {
    if (auto val2 = getLimitedValS())
      return val == *val2;
    return false;
  }

  std::optional<uint32_t> getLimitedVal() const {
    assert(!self().getIs4S());
    if (self().getNumWords() > 1)
      return std::nullopt;
    if (self().getExtNumWords() > 1 && self().getExtend() != 0)
      return std::nullopt;
    return self().getWords()[0];
  }
  std::optional<int32_t> getLimitedValS() const {
    assert(!self().getIs4S());
    if (self().getNumWords() > 1)
      return std::nullopt;
    if (self().getExtNumWords() > 1 &&
        !(self().getExtend() == 0 ||
          (self().getExtend() == 0b11 &&
           self().getWords()[0] & bit_mask_msb<uint32_t>())))
      return std::nullopt;
    return self().getWords()[0];
  }
  uint32_t getExactVal() const {
    auto rv = getLimitedVal();
    assert(rv && "would truncate");
    return *rv;
  }
  uint32_t getExactValS() const {
    auto rv = getLimitedValS();
    assert(rv && "would truncate");
    return *rv;
  }
};

class PatBigInt : public BigIntMixin<PatBigInt> {
  friend class Constant;
  friend class ConstantStore;
  friend class BigIntMixin<PatBigInt>;
  friend class BigInt;

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
  std::span<uint32_t> getWords() const { return std::span<uint32_t>(); };
  PatBigInt(uint32_t bits, uint8_t pattern, uint8_t custom = 0) : bits(bits) {
    Extend{field} = pattern;
    Custom{field} = custom;
  }
};

class BigInt : public BigIntMixin<BigInt> {
  friend class Constant;
  friend class ConstantStore;
  friend class BigIntMixin;
  friend class BigInt;

  SmallVec<uint32_t, 4> words;
  uint32_t numBits;
  uint8_t field;

  template <typename T> using Extend = BitField<T, BigIntExtendBits, 0>;
  template <typename T>
  using Custom = BitField<T, BigIntCustomBits, BigIntExtendBits>;

public:
  // big int API
  unsigned getNumWords() const { return words.size(); }
  uint8_t getExtend() const { return Extend{field}; }
  uint8_t getIs4S() const { return Custom{field}; }
  std::span<uint32_t> getWords() { return words; }
  std::span<const uint32_t> getWords() const { return words; }

private:
  auto custom() { return Custom{field}; }
  BigInt(uint32_t numBits, uint32_t numWords, uint8_t extend, uint8_t custom)
      : words(numWords), numBits(numBits) {
    Extend<uint8_t>{field} = extend;
    Custom<uint8_t>{field} = custom;
  }

public:
  void setExtend(uint8_t val) {
    Extend{field} = val & bit_mask_ones<uint8_t>(BigIntExtendBits);
    normalizeExtend();
  }
  void setCustom(uint8_t val) {
    Custom{field} = val & bit_mask_ones<uint8_t>(BigIntCustomBits);
  }

  template <BigIntAPI T> BigInt &operator=(const T &other) {
    if constexpr (std::is_same_v<T, BigInt>)
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
    return *this;
  }

  BigInt &operator=(const BigInt &other) {
    return this->BigInt::operator= <BigInt>(other);
  }
  BigInt(const BigInt &other) : BigInt() { (*this) = other; }

  template <BigIntAPI T> BigInt(const T &other) : BigInt() { (*this) = other; }

  BigInt &operator=(BigInt &&other) = default;
  BigInt(BigInt &&other) = default;

  BigInt() : words(), numBits(0), field(0) {}
  static BigInt fromU64Pruned(uint64_t val) {
    BigInt rv;
    rv.setPruned(val);
    return rv;
  }
  static BigInt fromU64(uint64_t val, unsigned bits) {
    BigInt rv;
    rv.set(val, bits);
    return rv;
  }
  static BigInt fromI64(int64_t val, unsigned bits) {
    BigInt rv;
    rv.set(uint64_t(val), bits);
    rv.setExtend(val < 0 ? 0b11 : 0);
    return rv;
  }

  static BigInt ofLen(uint32_t bits) {
    return BigInt{bits, round_up_div(bits, WordBits), 0, 0};
  }

  static BigInt fromRaw(std::span<const uint32_t> data, uint32_t bits,
                        uint8_t extend, uint8_t custom) {
    auto rv = BigInt{bits, (uint32_t)data.size(), extend, 0};
    std::copy(data.begin(), data.end(), rv.words.begin());
    rv.normalize();
    if (rv.getIs4S())
      rv.conv4To2StateIfPossible();
    return rv;
  }
  static BigInt fromRaw(SmallVecImpl<uint32_t> &&data, uint32_t bits,
                        uint8_t extend, uint8_t custom) {
    auto rv = BigInt{};
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
  static void ident(BigInt &out, const T0 &lhs, const T1 &rhs) {               \
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

  template <typename T> static void negateOp(BigInt &out, const T &lhs) {
    subOp(out, PatBigInt(lhs.getRawNumBits(), 0), lhs);
  }

  void setRepeating(uint32_t val, unsigned bits, uint8_t custom = 0) {
    words.resize(1);
    words[0] = val;
    numBits = bits;
    Extend{field} = val & bit_mask_ones<uint32_t>(BigIntExtendBits);
    Custom{field} = custom;
  }
  void set(uint64_t val, unsigned bits) {
    if (bits <= WordBits)
      return set((uint32_t)val, bits);
    words.resize(2);
    words[0] = val;
    words[1] = val >> WordBits;
    numBits = bits;
    normalize();
  }
  void set(uint32_t val, unsigned bits) {
    words.resize(1);
    words[0] = val;
    numBits = bits;
  }
  void setPruned(uint64_t val) {
    unsigned bits = clog2(val);
    numBits = bits;
    words.resize(bits <= WordBits ? 1 : 2);
    words[0] = val;
    if (words.size() == 2) {
      words[1] = val >> WordBits;
      normalize();
    }
  }
  uint32_t getRawNumBits() const { return numBits; }

  template <bool Left, bool Arith = false, typename T0>
  static void shiftOp(BigInt &out, const T0 &lhs, uint32_t rhs) {
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
  static BigInt mulOp(const T0 &lhs, const T1 &rhs) {

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
    auto out = BigInt::ofLen(resultBits);

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

  template <typename T0> static uint32_t leadingZeros(const T0 &val) {
    if (val.isExtended()) {
      if (val.getExtend() == 0b11 || val.getExtend() == 0b10)
        return 0;
      if (val.getExtend() == 0b01)
        return 1;
      assert(val.getExtend() == 0);
    }

    // may be negative.
    int64_t zeroBits = val.getRawNumBits() - (val.getNumWords() * WordBits);
    uint32_t lastWord = val.getWords()[val.getNumWords() - 1];
    if (lastWord == 0)
      return zeroBits + WordBits;
    return zeroBits + __builtin_clz(lastWord);
  }

  template <typename T0, typename T1>
  static BigInt upowOp(const T0 &lhs, const T1 &rhs) {
    if (rhs.valueEquals(0))
      return BigInt::fromU64(1, lhs.getNumBits());
    if (lhs.valueEquals(0))
      return BigInt::fromU64(0, lhs.getNumBits());

    BigInt acc = BigInt::fromU64(1, lhs.getNumBits());
    BigInt pow{lhs};

    uint32_t bits = rhs.getRawNumBits() - leadingZeros(rhs);
    for (uint32_t i = 0; i < bits; i++) {
      if (rhs.getRawBit(i)) {
        acc = BigInt::mulOp(acc, pow);
        if (acc.valueEquals(0))
          break;
      }
      pow = BigInt::mulOp(pow, pow);
    }
    return acc;
  }

  template <typename T0, typename T1>
  static BigInt spowOp(const T0 &lhs, const T1 &rhs) {
    if (!rhs.getRawSignBit())
      return upowOp(lhs, rhs);

    if (lhs.valueEqualsS(-1)) {
      return BigInt::fromI64(rhs.getRawBit(0) ? 1 : -1, lhs.getNumBits());
    }
    if (lhs.valueEquals(0)) {
      // in verilog returns x but we check for that in the 4S func
      // instead. just return zero here.
      return BigInt::fromU64(0, lhs.getNumBits());
    }
    if (lhs.valueEquals(1)) {
      return BigInt::fromU64(1, lhs.getNumBits());
    }
    return BigInt::fromU64(0, lhs.getNumBits());
  }

  template <typename T0>
  static Optional<uint32_t> leadingZeros4S(const T0 &val) {
    if (val.getIs4S())
      return nullopt;
    return leadingZeros(val);
  }

  template <typename T0, typename T1>
  static BigInt upowOp4S(const T0 &lhs, const T1 &rhs) {
    if (lhs.getIs4S() || rhs.getIs4S())
      return PatBigInt(lhs.getRawNumBits(), 0b11, 1);
    return BigInt::upowOp(lhs, rhs);
  }

  template <typename T0, typename T1>
  static BigInt spowOp4S(const T0 &lhs, const T1 &rhs) {
    if (lhs.getIs4S() || rhs.getIs4S() ||
        (lhs.valueEquals(0) && rhs.getRawSignBit()))
      return PatBigInt(lhs.getRawNumBits(), 0b11, 1);
    return BigInt::spowOp(lhs, rhs);
  }

  template <typename T>
  static void resizeOp(BigInt &out, const T &lhs, uint32_t newSize,
                       uint8_t extendPat = 0) {

    auto copyIfDifferent = [&]() {
      if (lhs.getWords().begin().base() != out.getWords().begin().base())
        std::copy_n(lhs.getWords().begin(),
                    std::min(lhs.getNumWords(), out.getNumWords()),
                    out.getWords().begin());
    };

    // truncate
    if (newSize < lhs.getRawNumBits()) {
      out.words.resize(
          std::min(lhs.getNumWords(), round_up_div(newSize, WordBits)));
      copyIfDifferent();
      out.numBits = newSize;

      // cutting of top (bits) make reveal a collapsable section
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
  static void rangeSelectOp(BigInt &out, const T &src, uint32_t bitOffs,
                            uint32_t bitLen) {
    if (bitLen == 0) {
      out = BigInt::fromU64(0, 0);
      return;
    }

    uint32_t outWords = round_up_div(bitLen, WordBits);
    do {
      if constexpr (std::is_same_v<T, BigInt>) {
        if (&out == &src)
          break;
      }
      out.words.resize(outWords);
    } while (false);

    uint32_t offs = bitOffs / WordBits;
    uint32_t shamt = bitOffs % WordBits;

    for (uint32_t i = 0; i < round_up_div(bitLen, WordBits); i++) {
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

    BigInt quot = BigInt::ofLen(lhs.getRawNumBits());
    BigInt rem = BigInt::ofLen(rhs.getRawNumBits());

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
    BigInt un;
    BigInt vn;

    // Find most significant 1 bit.
    uint32_t shamt = __builtin_clz(rhs.getWord(highWordIdx));
    vn.resizeOp(vn, rhs, (highWordIdx + 1) * 32 - shamt);
    BigInt::shlOp(vn, vn, shamt);

    BigInt::resizeOp(un, lhs, lhs.getRawNumBits() + shamt);
    BigInt::shlOp(un, un, shamt);
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

    BigInt::lshrOp(rem, un, shamt);
    BigInt::resizeOp(rem, rem, rhs.getRawNumBits());

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

    BigInt lhsAbs = lhs.getRawSignBit() ? -lhs : lhs;
    BigInt rhsAbs = rhs.getRawSignBit() ? -rhs : rhs;

    auto [div, mod] = udivmodOp(lhsAbs, rhsAbs);

    if (lhsSign ^ rhsSign)
      negateOp(div, div);
    if (lhsSign)
      negateOp(mod, mod);

    return std::make_pair(std::move(div), std::move(mod));
  }

  template <typename T0>
  static void shlOp(BigInt &out, const T0 &lhs, uint32_t rhs) {
    shiftOp<true>(out, lhs, rhs);
  }

  template <typename T0>
  static void lshrOp(BigInt &out, const T0 &lhs, uint32_t rhs) {
    shiftOp<false>(out, lhs, rhs);
  }

  template <typename T0>
  static void ashrOp(BigInt &out, const T0 &lhs, uint32_t rhs) {
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
  static void concatOp(BigInt &out, const T0 &lhs, const T1 &rhs) {
    size_t rhsWords = rhs.getNumWords();
    size_t rhsExtWords = rhs.getExtNumWords();
    size_t rhsBits = rhs.getRawNumBits();

    if constexpr (std::is_same_v<BigInt, T0>)
      assert(&out != &lhs && "only rhs may alias out");

    if (lhs.getNumBits() == 0) {
      out = rhs;
      return;
    }
    if (rhs.getNumBits() == 0) {
      out = lhs;
      return;
    }

    size_t outNumWords = rhsExtWords + lhs.getNumWords();
    if ((rhsBits % 32) && (lhs.getRawNumBits() % 32) &&
        (rhsBits % 32) + (lhs.getRawNumBits() % 32) <= 32)
      outNumWords--;

    out.words.resize(outNumWords);
    out.numBits = lhs.getRawNumBits() + rhsBits;

    do {
      // edge case rhs == out.
      if constexpr (std::is_same_v<T1, BigInt>) {
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
  static void repeatOp(BigInt &out, const T0 &val, uint32_t count) {
    if (count == 0) {
      out.set(0U, 0);
      return;
    } else if (count == 1) {
      out = val;
      return;
    } else if (count == 2) {
      concatOp(out, BigInt{val}, out);
      return;
    }

    // out might alias val so do this first
    BigInt pow;
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
      concatOp(pow, BigInt{pow}, pow);
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
    return ~xor_4state(lhs, rhs);
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
  //   static_assert(BigInt::and_4state(0b00'00'01'01'00'10'10'11,
  //                                      0b00'01'00'01'10'00'01'01) ==
  //                 0b00'00'00'01'00'00'11'11);
  //   static_assert(BigInt::or_4state(0b00'01'10'10'11, 0b01'00'01'00'00) ==
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
    SmallVec<uint32_t, 4> buf(outNumWords);

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
  static void bitwiseOp4S(BigInt &out, const T0 &lhs, const T1 &rhs) {
    if (!lhs.getIs4S() && !rhs.getIs4S()) {
      Func2S(out, lhs, rhs);
      return;
    }
    out.words.resize(std::max(lhs.getNumWords(), rhs.getNumWords()));

    uint32_t hasUnk = 0;

    for (size_t i = 0; i < out.getNumWords(); i++) {
      uint32_t lhsV = lhs.getWord4S(i);
      uint32_t rhsV = rhs.getWord4S(i);
      uint32_t outV = Func4S(lhsV, rhsV);
      out.words[i] = outV;
      hasUnk |= outV & REP10;
    }

    Extend{out.field} =
        Func4S(lhs.getIs4S() ? lhs.getExtend() : unpack_bits(lhs.getExtend()),
               rhs.getIs4S() ? rhs.getExtend() : unpack_bits(rhs.getExtend())) &
        0b11;

    if (!hasUnk && (!out.isExtended() || !(Extend{out.field} & 0b10)))
      out.conv4To2State();

    out.normalize();
  }
  template <BigIntAPI T0, BigIntAPI T1>
  static void andOp4S(BigInt &out, const T0 &lhs, const T1 &rhs) {
    return bitwiseOp4S<and_4state, andOp<T0, T1>, T0, T1>(out, lhs, rhs);
  }
  template <BigIntAPI T0, BigIntAPI T1>
  static void orOp4S(BigInt &out, const T0 &lhs, const T1 &rhs) {
    return bitwiseOp4S<or_4state, orOp<T0, T1>, T0, T1>(out, lhs, rhs);
  }
  template <BigIntAPI T0, BigIntAPI T1>
  static void xorOp4S(BigInt &out, const T0 &lhs, const T1 &rhs) {
    return bitwiseOp4S<xor_4state, xorOp<T0, T1>, T0, T1>(out, lhs, rhs);
  }
  template <BigIntAPI T0, BigIntAPI T1>
  static void xnorOp4S(BigInt &out, const T0 &lhs, const T1 &rhs) {
    return bitwiseOp4S<xnor_4state, xnorOp<T0, T1>, T0, T1>(out, lhs, rhs);
  }
  template <BigIntAPI T0> static void notOp4S(BigInt &out, const T0 &lhs) {
    return xorOp4S(out, lhs, PatBigInt{lhs.getRawNumBits(), 0b11});
  }

#define LINEAR_OP_4S(ident, func2s)                                            \
  template <BigIntAPI T0, BigIntAPI T1>                                        \
  static void ident(BigInt &out, const T0 &lhs, const T1 &rhs) {               \
    if (lhs.getIs4S() || rhs.getIs4S()) {                                      \
      out.setRepeating(EXTX_MASK,                                              \
                       std::max(lhs.getRawNumBits(), rhs.getRawNumBits()), 1); \
      return;                                                                  \
    }                                                                          \
    func2s(out, lhs, rhs);                                                     \
  }

  template <BigIntAPI T0, BigIntAPI T1>
  static void addOp4S(BigInt &out, const T0 &lhs, const T1 &rhs) {
    if (lhs.getIs4S() || rhs.getIs4S()) {
      out.setRepeating(EXTX_MASK,
                       std ::max(lhs.getRawNumBits(), rhs.getRawNumBits()), 1);
      return;
    }
    addOp(out, lhs, rhs);
  }
  LINEAR_OP_4S(subOp4S, subOp)

  template <BigIntAPI T0>
  static void shlOp4S(BigInt &out, const T0 &lhs, unsigned rhs) {
    shlOp(out, lhs, lhs.getIs4S() ? 2 * rhs : rhs);
    out.conv4To2StateIfPossible();
  }
  template <BigIntAPI T0>
  static void lshrOp4S(BigInt &out, const T0 &lhs, unsigned rhs) {
    lshrOp(out, lhs, lhs.getIs4S() ? 2 * rhs : rhs);
    out.conv4To2StateIfPossible();
  }
  template <BigIntAPI T0>
  static void ashrOp4S(BigInt &out, const T0 &lhs, unsigned rhs) {
    ashrOp(out, lhs, lhs.getIs4S() ? 2 * rhs : rhs);
    out.conv4To2StateIfPossible();
  }

  template <BigIntAPI T>
  static void resizeOp4S(BigInt &out, const T &lhs, uint32_t newSize,
                         bool sign = false) {
    if (lhs.getIs4S()) {
      BigInt::resizeOp(out, lhs, 2 * newSize,
                       sign ? lhs.getExtendPatFromSignBit() : 0);
      out.conv4To2StateIfPossible();
    } else
      BigInt::resizeOp(out, lhs, newSize,
                       sign ? lhs.getExtendPatFromSignBit() : 0);
  }
  template <BigIntAPI T>
  static void rangeSelectOp4S(BigInt &out, const T &src, uint32_t bitOffs,
                              uint32_t bitLen) {
    if (src.getIs4S()) {
      BigInt::rangeSelectOp(out, src, bitOffs * 2, bitLen * 2);
      out.setCustom(1);
      out.conv4To2StateIfPossible();
    } else {
      BigInt::rangeSelectOp(out, src, bitOffs, bitLen);
    }
  }
  template <BigIntAPI T0, BigIntAPI T1>
  static auto udivmodOp4S(const T0 &lhs, const T1 &rhs) {
    if (lhs.getIs4S() || rhs.getIs4S()) {
      BigInt outA;
      BigInt outB;
      outA.setRepeating(EXTX_MASK, lhs.getRawNumBits(), 1);
      outB.setRepeating(EXTX_MASK, lhs.getRawNumBits(), 1);
      return std::make_pair(std::move(outA), std::move(outB));
    }
    auto [div, rem] = BigInt::udivmodOp(lhs, rhs);
    return std::make_pair(BigInt{div}, BigInt{rem});
  }
  template <BigIntAPI T0, BigIntAPI T1>
  static auto sdivmodOp4S(const T0 &lhs, const T1 &rhs) {
    if (lhs.getIs4S() || rhs.getIs4S() || rhs.valueEquals(0)) {
      BigInt outA;
      BigInt outB;
      outA.setRepeating(EXTX_MASK, lhs.getRawNumBits(), 1);
      outB.setRepeating(EXTX_MASK, lhs.getRawNumBits(), 1);
      return std::make_pair(std::move(outA), std::move(outB));
    }
    auto [div, rem] = BigInt::sdivmodOp(lhs, rhs);
    return std::make_pair(BigInt{div}, BigInt{rem});
  }
  template <int Mode = 0, BigIntAPI T0, BigIntAPI T1>
  static auto mulOp4S(const T0 &lhs, const T1 &rhs) {
    if (lhs.getIs4S() || rhs.getIs4S()) {
      BigInt out;
      out.setRepeating(EXTX_MASK,
                       Mode == 0
                           ? std::max(lhs.getRawNumBits(), rhs.getRawNumBits())
                           : lhs.getRawNumBits() + rhs.getRawNumBits(),
                       1);
      return out;
    }
    return BigInt{BigInt::mulOp<Mode>(lhs, rhs)};
  }
  template <BigIntAPI T> static void negateOp4S(BigInt &out, const T &lhs) {
    subOp4S(out, PatBigInt(lhs.getRawNumBits(), 0), lhs);
  }

  template <BigIntAPI T> static FourState reductionXOROp4S(const T &val) {
    if (val.getIs4S())
      return FourState::SX;
    return reductionXOROp(val);
  }

  template <BigIntAPI T0, BigIntAPI T1>
  static void concatOp4S(BigInt &out, const T0 &lhs, const T1 &rhs) {
    bool custom = rhs.getIs4S() || lhs.getIs4S();
    if (lhs.getIs4S() && !rhs.getIs4S()) {
      BigInt rhsCopy{rhs};
      rhsCopy.conv2To4State();
      concatOp(out, lhs, rhsCopy);
    } else if (!lhs.getIs4S() && rhs.getIs4S()) {
      BigInt lhsCopy{lhs};
      lhsCopy.conv2To4State();
      concatOp(out, lhsCopy, rhs);
    } else
      concatOp(out, lhs, rhs);
    out.setCustom(custom);
  }

  template <BigIntAPI T0>
  static void repeatOp4S(BigInt &out, const T0 &val, uint32_t count) {
    bool custom = val.getIs4S();
    repeatOp(out, val, count);
    out.setCustom(custom);
  }

  // String Ops
  static std::optional<BigInt> parseHex(std::span<const char> digits) {
    BigInt out = BigInt::ofLen(digits.size() * 4);
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
  static std::optional<BigInt> parseDec(std::span<const char> digits) {
    double bits = ceil(digits.size() * std::log2(10.));
    BigInt out = BigInt::ofLen(size_t(bits));
    BigInt base = BigInt::fromU64Pruned(10);

    for (const char digit : digits) {
      out = BigInt::mulOp(out, base);

      uint64_t val;
      if (digit >= '0' && digit <= '9')
        val = digit - '0';
      else
        return std::nullopt;

      BigInt::addOp(out, out, BigInt::fromU64Pruned(val));
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
      return BigInt::stream_hex(os, self);

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
      size_t digit = self.getRawNumBits() - 1 - i;
      if (digit != 0 && (digit % 8) == 0)
        os << "_";
    }
  }

  template <typename T>
  static void stream_bin_4s_vlog(std::ostream &os, const T &self) {
    std::array<char, 4> bitToStr = {'0', '1', 'z', 'x'};
    for (ssize_t i = (self.getRawNumBits() / 2) - 1; i >= 0; i--) {
      os << bitToStr[self.getBit(i)];
      size_t digit = (self.getRawNumBits() / 2) - 1 - i;
      if (digit != 0 && (digit % 8) == 0)
        os << "_";
    }
  }

  template <typename T>
  static void stream_dec(std::ostream &os, const T &self,
                         bool isSigned = false) {
    if (self.getRawSignBit() && isSigned) {
      BigInt temp;
      BigInt::negateOp(temp, self);
      os << "-";
      stream_dec(os, temp);
      return;
    }
    SmallVec<uint8_t, 32> str;

    BigInt q = BigInt::fromU64Pruned(10);
    BigInt n{self};
    BigInt r;

    do {
      std::tie(n, r) = BigInt::udivmodOp(n, q);
      str.emplace_back(r.getWord(0));
    } while (n != BigInt::fromU64(0, n.getRawNumBits()));

    for (ssize_t i = str.size() - 1; i >= 0; i--)
      os << uint32_t(str[i]);
  }

  void prune() {
    uint32_t extendMask = repeatExtend(Extend{field});
    while (words.size() != 1 && words.back() == extendMask)
      words.pop_back();
    words.try_to_inline();
  }

private:
  void normalizeExtend() {
    if (!isExtended())
      Extend{field} = 0;
    setLastWordBitsToPattern(Extend{field});
  }

  void setLastWordBitsToPattern(uint8_t pattern) {
    // truncate last word and fill with extend.
    if (getRawNumBits() % 32 != 0) {
      auto mask = (1 << (getRawNumBits() % 32)) - 1;
      words.back() &= mask;
      words.back() |= (~mask & repeatExtend(pattern));
    }
  }

  void normalize() {
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
  void expand() {
    auto oldSize = getNumWords();
    words.resize(getExtNumWords());
    std::fill(words.begin() + oldSize, words.end(),
              repeatExtend(Extend{field}));
  }

  template <auto OpFunc, typename T0, typename T1>
  static void carryPropOp(BigInt &out, const T0 &lhs, const T1 &rhs) {

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

// fixme: make constexpr (at least parsing)
static inline BigInt operator""_b(const char *str) {
  std::optional<BigInt> rv = std::nullopt;
  if (str[0] == '0' && str[1] == 'x') {
    rv = BigInt::parseHex(std::span<const char>{
        str + 2, std::char_traits<char>::length(str) - 2});
  } else
    rv = BigInt::parseDec(std::span{str, std::char_traits<char>::length(str)});
  assert(rv && "invalid str literal");
  return *rv;
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

  std::span<const uint32_t> getWords() const {
    return isInline()
               ? std::span<const uint32_t>{&obj.num, 1}
               : std::span<const uint32_t>{ptr->trailing(), getNumWords()};
  }
  uint8_t getExtend() const {
    return isInline() ? customField<ExtPattern>() : ptr->getExtend();
  }
  uint8_t getIs4S() const {
    return isInline() ? customField<Custom>() : ptr->getIs4S();
  }
  unsigned getNumWords() const { return isInline() ? 1 : ptr->getNumWords(); };

  uint getRawNumBits() const {
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
  uint32_t hash(uint32_t a) {
    a = (a ^ 61) ^ (a >> 16);
    a = a + (a << 3);
    a = a ^ (a >> 4);
    a = a * 0x27d4eb2d;
    a = a ^ (a >> 15);
    return a;
  }

  template <typename T> uint32_t constantHash(const T &constant) {
    uint32_t acc = 0;
    acc ^= hash(constant.getIs4S());
    acc ^= hash(constant.getExtend());
    acc ^= hash(constant.getRawNumBits());
    acc ^= hash(constant.getNumWords());
    for (const auto word : constant.getWords())
      acc ^= hash(word);
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
  FatObjRef<Constant> resolve(ObjRef<Constant> ref) {
    return store.resolve(ref);
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
  ConstantBuilderBase &raw(unsigned bits, std::span<const uint32_t> data,
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
    cur.set(uint32_t(~0ULL), like.getNumBits());
    cur.setExtend(bit_mask_ones<uint8_t>(BigIntExtendBits));
    return *this;
  }
  template <typename U> ConstantBuilderBase &zeroLike(U like) {
    cur.set(uint32_t(0), like.getNumBits());
    return *this;
  }
  template <typename U> ConstantBuilderBase &oneLike(U like) {
    cur.set(uint32_t(1), like.getNumBits());
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
    BigInt::concatOp4S(cur, BigInt{cur}, other);
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
    BigInt::concatOp4S(cur, BigInt{cur}, tmp);
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
