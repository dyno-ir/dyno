#pragma once

#include "support/Bits.h"
#include "support/SmallVec.h"
#include "support/Utility.h"
#include <algorithm>
#include <dyno/IDs.h>
#include <dyno/NewDeleteObjStore.h>
#include <dyno/Obj.h>
#include <iomanip>
#include <iostream>
#include <ostream>
#include <span>
#include <type_traits>
#include <unordered_map>

namespace dyno {

class ConstantRef;
class ConstantBuilder;

constexpr size_t BigIntExtendBits = 2;
constexpr uint32_t WordBits = sizeof(uint32_t) * 8;
constexpr uint32_t WordBitsM1 = WordBits - 1;

template <typename Derived> class BigIntMixin {

  // Base API for BigInt is getNumWords, getExtend and getWords.
  // This is for utility functions using that API that may be shared by all
  // implementing BigInt API.
  Derived &self() { return *static_cast<Derived *>(this); }
  const Derived &self() const { return *static_cast<const Derived *>(this); }

protected:
  static uint32_t bitsToWords(uint32_t bits) {
    return (bits + WordBitsM1) / WordBits;
  }
  static constexpr unsigned repeatExtend(uint32_t num) {
    unsigned fact = BigIntExtendBits;
    num &= bit_mask_ones<unsigned>(BigIntExtendBits);
    while (fact != bit_mask_sz<unsigned>) {
      num |= (num << fact);
      fact <<= 1;
    }
    return num;
  }
  uint32_t getExtNumWords() const { return bitsToWords(self().getNumBits()); }
  uint32_t getWord(uint32_t i) const {
    if (i > bitsToWords(self().getNumBits()))
      dyno_unreachable("out of bounds");

    if (i >= self().getNumWords())
      return repeatExtend(self().getExtend());
    return self().getWords()[i];
  }

  bool getBit(uint32_t i) const {
    return getWord(i / WordBits) & (1 << (i % WordBits));
  }
  bool getSignBit() const { return getBit(self().getNumBits()); }
  bool isExtended() const { return self().getNumWords() != getExtNumWords(); }
};

class BigInt : public BigIntMixin<BigInt> {
  friend class Constant;
  friend class ConstantBuilder;
  friend class ConstantStore;
  friend class BigIntMixin;

  SmallVec<uint32_t, 4> words;
  uint32_t numBits;
  uint8_t extend;

protected:
  unsigned getNumWords() const { return words.size(); }
  unsigned getExtend() const { return extend; }
  std::span<uint32_t> getWords() { return words; }
  const std::span<uint32_t> getWords() const { return words; }

public:
  BigInt() : words(), numBits(0), extend(0) {}
  BigInt(uint64_t val) { setPruned(val); }
  BigInt(uint64_t val, unsigned bits) { set(val, bits); }

  static BigInt ofLen(uint32_t bits) {
    return BigInt{bits, (bits + WordBitsM1) / WordBits, 0};
  }

  template <typename T0, typename T1>
  static void addOp(BigInt &out, const T0 &lhs, const T1 &rhs) {
    return linearOp<[](unsigned lhs, unsigned rhs, unsigned cin,
                       unsigned *cout) {
      return __builtin_addc(lhs, rhs, cin, cout);
    }>(out, lhs, rhs);
  }
  template <typename T0, typename T1>
  static void subOp(BigInt &out, const T0 &lhs, const T1 &rhs) {
    return linearOp<[](unsigned lhs, unsigned rhs, unsigned cin,
                       unsigned *cout) {
      return __builtin_subc(lhs, rhs, cin, cout);
    }>(out, lhs, rhs);
  }
  template <typename T0, typename T1>
  static void andOp(BigInt &out, const T0 &lhs, const T1 &rhs) {
    return linearOp<[](unsigned lhs, unsigned rhs,
                       [[maybe_unused]] unsigned cin,
                       [[maybe_unused]] unsigned *cout) { return lhs & rhs; }>(
        out, lhs, rhs);
  }
  template <typename T0, typename T1>
  static void orOp(BigInt &out, const T0 &lhs, const T1 &rhs) {
    return linearOp<[](unsigned lhs, unsigned rhs,
                       [[maybe_unused]] unsigned cin,
                       [[maybe_unused]] unsigned *cout) { return lhs | rhs; }>(
        out, lhs, rhs);
  }
  template <typename T0, typename T1>
  static void xorOp(BigInt &out, const T0 &lhs, const T1 &rhs) {
    return linearOp<[](unsigned lhs, unsigned rhs,
                       [[maybe_unused]] unsigned cin,
                       [[maybe_unused]] unsigned *cout) { return lhs ^ rhs; }>(
        out, lhs, rhs);
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
  uint32_t getNumBits() const { return numBits; }

  void setExtend(uint8_t extend) { this->extend = extend & 3; }

  template <bool Left, bool Arith = false, typename T0>
  static void shiftOp(BigInt &out, const T0 &lhs, uint32_t rhs) {
    uint32_t shamtWords = rhs / WordBits;
    uint32_t shamtRem = rhs % WordBits;
    uint8_t oldExtend = lhs.extend;
    out.numBits = lhs.getNumBits();
    if ((rhs & 1) && (out.extend == 1 || out.extend == 2))
      out.extend = (~oldExtend) & bit_mask_ones<uint8_t>(BigIntExtendBits);
    else
      out.extend = oldExtend & bit_mask_ones<uint8_t>(BigIntExtendBits);

    auto originalNumWords = lhs.getNumWords();

    if constexpr (Left)
      out.words.resize(
          std::min(originalNumWords + ((rhs + WordBitsM1) / WordBits),
                   (lhs.getNumBits() + WordBitsM1) / WordBits));
    else {
      if (!Arith && out.extend != 0) {
        out.words.resize((lhs.getNumBits() + WordBitsM1) / WordBits);
        out.extend = 0;
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
            low &= ((1 << (lhs.getNumBits()) % 32)) - 1;
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
            high &= (1 << (lhs.getNumBits() % 32)) - 1;
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
      resultBits = std::max(lhs.getNumBits(), rhs.getNumBits());
      break;
    case 1: // extending, unsigned
      [[fallthrough]];
    case 2: // extending, signed
      resultBits = lhs.getNumBits() + rhs.getNumBits();
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
      if (lhs.getSignBit()) {
        uint32_t b = 0;
        for (size_t i = 0; i < rhs.getExtNumWords() &&
                           i + lhs.getExtNumWords() < out.getNumWords();
             i++) {
          out.getWords()[i + lhs.getExtNumWords()] = __builtin_subc(
              out.getWords()[i + lhs.getExtNumWords()], rhs.getWord(i), b, &b);
        }
      }
      if (rhs.getSignBit()) {
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

  template <int Mode, typename T>
  static void resizeOp(BigInt &out, const T &lhs, uint32_t newSize) {

    auto copyIfDifferent = [&]() {
      if (lhs.getWords().begin() != out.getWords().begin())
        std::copy_n(lhs.getWords().begin(),
                    std::min(lhs.getNumWords(), out.getNumWords()),
                    out.getWords().begin());
    };

    // truncate
    if (newSize < lhs.getNumBits()) {
      out.words.resize(
          std::min(lhs.getNumWords(), (newSize + WordBitsM1) / WordBits));
      copyIfDifferent();
      out.numBits = newSize;

      // cutting of top (bits) make reveal a collapsable section
      out.normalize();
      return;
    }

    uint8_t newExtend;
    if constexpr (Mode == 0)
      newExtend = 0;
    else if constexpr (Mode == 1)
      newExtend = lhs.getSignBit() ? 0b11 : 0;
    else
      static_assert(false);

    if ((newExtend == lhs.extend || !lhs.isExtended())) {
      // as long as extend stays the same (or is freely assignable) we can
      // extend by just changing numBits
      out.words.resize(lhs.getNumWords());
      copyIfDifferent();

      out.numBits = newSize;
      out.extend = newExtend;

      // no need to normalize here
      return;
    }

    // else we need to materialize lhs's extend bits
    out.words.resize(lhs.getExtNumWords());
    for (size_t i = lhs.getNumWords(); i < lhs.getExtNumWords(); i++)
      out.getWords()[i] = lhs.getWord(i);

    out.numBits = newSize;
    out.extend = newExtend;
  }

  template <typename T0, typename T1>
  static auto udivmodOp(const T0 &lhs, const T1 &rhs) {
    const uint64_t base = 1UL << 32;

    BigInt quot = BigInt::ofLen(lhs.getNumBits());
    BigInt rem = BigInt::ofLen(rhs.getNumBits());

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
        auto t = (k * base + lhs.getWord(i));
        // do single digit division
        quot.words[i] = t / rhs.getWord(0);
        // correct back
        k = t - quot.words[i] * rhs.getWord(0);
      }
      rem.words[0] = k;

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
    vn.resizeOp<0>(vn, rhs, (highWordIdx + 1) * 32 - shamt);
    BigInt::shlOp(vn, vn, shamt);

    BigInt::resizeOp<0>(un, lhs, lhs.getNumBits() + shamt);
    BigInt::shlOp(un, un, shamt);
    un.expand();

    std::cout << "un = " << un << "\n";
    std::cout << "vn = " << vn << "\n";

    size_t n = vn.getExtNumWords();
    for (ssize_t i = un.getNumWords() - 1 - n; i >= 0; i--) {
      uint64_t t = (un.getWord(i + n) * base + un.getWord(i + n - 1));

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

      uint64_t k = 0;
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
    BigInt::resizeOp<0>(rem, rem, rhs.getNumBits());
    return std::make_pair(std::move(quot), std::move(rem));
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

  template <typename T>
  friend bool operator==(const BigInt &lhs, const T &rhs) {
    if (!(lhs.getNumBits() == rhs.getNumBits() &&
          lhs.getNumWords() == rhs.getNumWords() &&
          lhs.getExtend() == rhs.getExtend()))
      return false;

    for (size_t i = 0; i < lhs.getNumWords(); i++)
      if (lhs.getWords()[i] != rhs.getWords()[i])
        return false;
    return true;
  }

  friend std::ostream &operator<<(std::ostream &os, const BigInt &self) {
    ssize_t hexDigits = (self.numBits + 3) / 4;
    ssize_t wordHexDigits = self.getNumWords() * 8;

    uint32_t extend = repeatExtend(self.extend) & 0xF;

    auto toHex = [](int n) -> char {
      return n > 9 ? (n + 'a' - 10) : (n + '0');
    };

    ssize_t extendDigits = hexDigits - wordHexDigits;
    for (ssize_t i = 0; i < extendDigits; i++) {
      size_t digit = hexDigits - i - 1;
      if (i == 0 && self.numBits % 4 != 0)
        os << toHex(extend & ((1 << (self.numBits % 4)) - 1));
      else
        os << toHex(extend);
      if (digit % 8 == 0)
        os << '\'';
    }

    auto flags = os.flags();
    for (ssize_t i = self.getNumWords() - 1; i >= 0; i--) {
      uint32_t word = self.getWords()[i];
      size_t width = 8;

      if (i == self.getNumWords() - 1 && extendDigits < 0) {
        width = (hexDigits % 8);
        word &= (1 << (self.getNumBits() % 32)) - 1;
      }

      os << std::hex << std::setfill('0') << std::setw(width) << word;
      if (i != 0)
        os << '\'';
    }
    os.flags(flags);
    return os;
  }

private:
  BigInt(uint32_t numBits, uint32_t numWords, uint32_t extend)
      : words(numWords), numBits(numBits), extend(extend) {}

  void prune() {
    uint32_t extendMask = repeatExtend(extend);
    while (words.size() != 1 && words.back() == extendMask)
      words.pop_back();
    words.try_to_inline();
  }

  void normalize() {

    if (getNumWords() * bit_mask_sz<uint32_t> < numBits)
      prune();
    else if (getNumWords() * bit_mask_sz<uint32_t> >= numBits) {
      extend = (words.back() & bit_mask_ms_nbits<uint32_t>(BigIntExtendBits)) >>
               (bit_mask_sz<uint32_t> - BigIntExtendBits);
      // truncate last word and fill with extend.
      if (getNumBits() % 32 != 0) {
        auto mask = (1 << (getNumBits() % 32)) - 1;
        words.back() &= mask;
        words.back() |= (~mask & repeatExtend(extend));
      }
      prune();
    }
  }

  // opposite of normalize
  void expand() {
    auto oldSize = getNumWords();
    words.resize(getExtNumWords());
    std::fill(words.begin() + oldSize, words.end(), repeatExtend(extend));
  }

  template <auto OpFunc, typename T0, typename T1>
  static void linearOp(BigInt &out, const T0 &lhs, const T1 &rhs) {

    // assert(lhs.getNumBits() == rhs.getNumBits());
    unsigned maxNumBits = std::max(lhs.getNumBits(), rhs.getNumBits());
    unsigned numWords = std::max(lhs.getNumWords(), rhs.getNumWords());
    unsigned minNumWords = std::max(lhs.getNumWords(), rhs.getNumWords());

    unsigned _unused = 0;

    // zext/sext is purely an optimization in the container, so sext of result
    // is simply what those bits would add up to. Signed/unsigned is layer
    // above.
    unsigned extend =
        OpFunc(lhs.getExtend(), rhs.getExtend(), _unused, &_unused);

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

    out.extend = extend;
    out.numBits = maxNumBits;
    out.normalize();
  }
};

class alignas(uint64_t) Constant : public TrailingObjArr<Constant, uint32_t> {
  // we store u32's here st inline storage can use the same API with numWords =
  // 1
  friend class ConstantRef;
  // num bits is detached from actual storage words bc we can sign/zext.
  unsigned numBits;
  unsigned numWords;

public:
  Constant(DynObjRef ref, size_t sz, const BigInt &bigInt)
      : numBits(bigInt.getNumBits()),
        numWords((bigInt.getNumWords() &
                  ~bit_mask_ms_nbits<unsigned>(BigIntExtendBits)) |
                 bigInt.getExtend()
                     << (bit_mask_sz<unsigned> - BigIntExtendBits)) {
    std::copy(bigInt.getWords().begin(), bigInt.getWords().end(), trailing());
  }

  Constant(const Constant &) = delete;
  Constant(Constant &&) = delete;
  Constant &operator=(const Constant &) = delete;
  Constant &operator=(Constant &&) = delete;

private:
  size_t getNumTrailing() const {
    return numWords & ~bit_mask_ms_nbits<unsigned>(BigIntExtendBits);
  }
  uint8_t getExtend() const {
    return numWords >> (bit_mask_sz<unsigned> - BigIntExtendBits);
  }
};
static_assert(TrailingObj<Constant>);

class ConstantRef : public FatDynObjRef<Constant>,
                    public BigIntMixin<ConstantRef> {
  friend class ConstantBuilder;
  using IsInline = DynObjRef::CustomField<1, 0>;
  using ExtPattern = DynObjRef::CustomField<BigIntExtendBits, 1>;
  using NBits =
      DynObjRef::CustomField<15 - BigIntExtendBits, 1 + BigIntExtendBits>;

public:
  using FatDynObjRef<Constant>::FatDynObjRef;

  explicit ConstantRef(FatDynObjRef<Constant> ref)
      : FatDynObjRef<Constant>(ref) {}

  ConstantRef(unsigned n, uint32_t val, uint8_t extPattern)
      : FatDynObjRef<Constant>(DynObjRef::ofTy<Constant>()) {
    customField<IsInline>() = true;
    customField<ExtPattern>() = extPattern;
    customField<NBits>() = n;
    obj = ObjID{val};
  }

  std::span<const uint32_t> getWords() const {
    return isInline()
               ? std::span<const uint32_t>{&obj.num, 1}
               : std::span<const uint32_t>{ptr->trailing(), ptr->numWords};
  }
  uint8_t getExtend() const {
    return isInline() ? customField<ExtPattern>() : ptr->getExtend();
  }
  unsigned getNumWords() const { return isInline() ? 1 : ptr->numWords; };
  uint getNumBits() const {
    return customField<IsInline>() ? customField<NBits>() : ptr->numBits;
  };

  uint64_t valTrunc64() {
    if (customField<IsInline>()) {
      if (customField<ExtPattern>() == 0)
        return (uint32_t)obj;
      else if (customField<ExtPattern>() == 3)
        return (int32_t)obj;
      else
        dyno_unreachable("neither sign nor zext");
    } else {
      // assuming the stored value is normalized
      return getWords()[0];
    }
  }

  bool isInline() const { return customField<IsInline>(); }
  uint32_t valInline() const {
    assert(isInline());
    return obj.num;
  }
};

class ConstantStore {
  NewDeleteObjStore<Constant> store;

  // Just using hash as a key rather than the Constant itself. We want to do the
  // full key compare part manually so that we can compare with BigInt rather
  // than Constant.
  std::unordered_multimap<uint32_t, ObjRef<Constant>> map;

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
    acc ^= hash(constant.getExtend());
    acc ^= hash(constant.getNumBits());
    acc ^= hash(constant.getNumWords());
    for (const auto word : constant.getWords())
      acc ^= hash(word);
    return acc;
  }

  ConstantRef findOrInsert(const BigInt &bigInt) {
    uint32_t hash = constantHash(bigInt);
    for (auto [it, rangeEnd] = map.equal_range(hash); it != rangeEnd; ++it) {
      auto ref = store.resolve(it->second);
      if (bigInt == ConstantRef{ref})
        return ref;
    }

    auto ref = store.create(bigInt.getNumWords(), bigInt);
    map.insert(std::make_pair(hash, ref));
    return ref;
  }

  Constant &operator[](ObjRef<Constant> ref) { return store[ref]; }
  void destroy(FatObjRef<Constant> ref) { return store.destroy(ref); }
  FatObjRef<Constant> resolve(ObjRef<Constant> ref) {
    return store.resolve(ref);
  }
};

class ConstantBuilder {
  ConstantStore &store;
  BigInt cur;

public:
  ConstantBuilder(ConstantStore &store) : store(store) {}

  ConstantBuilder &val(unsigned bits, uint64_t value64 = 0) {
    cur.set(value64, bits);
    return *this;
  }

  ConstantBuilder &add(ConstantRef rhs) {
    BigInt::addOp(cur, cur, rhs);
    return *this;
  }
  ConstantBuilder &add(BigInt rhs) {
    BigInt::addOp(cur, cur, rhs);
    return *this;
  }
  ConstantBuilder &sub(ConstantRef rhs) {
    BigInt::subOp(cur, cur, rhs);
    return *this;
  }
  ConstantBuilder &sub(BigInt rhs) {
    BigInt::subOp(cur, cur, rhs);
    return *this;
  }
  ConstantBuilder &bitAND(ConstantRef rhs) {
    BigInt::andOp(cur, cur, rhs);
    return *this;
  }
  ConstantBuilder &bitAND(BigInt rhs) {
    BigInt::andOp(cur, cur, rhs);
    return *this;
  }
  ConstantBuilder &bitOR(ConstantRef rhs) {
    BigInt::orOp(cur, cur, rhs);
    return *this;
  }
  ConstantBuilder &bitOR(BigInt rhs) {
    BigInt::orOp(cur, cur, rhs);
    return *this;
  }
  ConstantBuilder &bitXOR(ConstantRef rhs) {
    BigInt::xorOp(cur, cur, rhs);
    return *this;
  }
  ConstantBuilder &bitXOR(BigInt rhs) {
    BigInt::xorOp(cur, cur, rhs);
    return *this;
  }

  ConstantRef get() {
    if (cur.getNumWords() == 1 &&
        cur.getNumBits() < (1ULL << ConstantRef::NBits::size))
      return ConstantRef{cur.getNumBits(), cur.words[0], cur.extend};

    return store.findOrInsert(cur);
  }

  operator ConstantRef() { return get(); }
};

template <> struct ObjTraits<Constant> {
  static constexpr DialectID dialect{DIALECT_CORE};
  static constexpr TyID ty{CORE_CONSTANT};
  using FatRefT = ConstantRef;
};
} // namespace dyno
