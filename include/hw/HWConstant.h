#pragma once

#include "dyno/Constant.h"
#include "support/Bits.h"
#include <dyno/IDs.h>
#include <dyno/NewDeleteObjStore.h>
#include <dyno/Obj.h>
#include <type_traits>

namespace dyno {

class HWBigInt;
class HWConstantRef;

template <typename T>
concept HWBigIntAPI = std::is_same_v<std::remove_cvref_t<T>, HWBigInt> ||
                      std::is_same_v<std::remove_cvref_t<T>, HWConstantRef>;

class HWBigInt : public BigInt {

  static constexpr uint32_t REP00 = repeatBits(0b00U, 2);
  static constexpr uint32_t REP01 = repeatBits(0b01U, 2);
  static constexpr uint32_t REP10 = repeatBits(0b10U, 2);
  static constexpr uint32_t REP11 = repeatBits(0b11U, 2);

  static constexpr uint32_t EXT0_MASK = REP00;
  static constexpr uint32_t EXT1_MASK = REP01;
  static constexpr uint32_t EXTZ_MASK = REP10;
  static constexpr uint32_t EXTX_MASK = REP11;

  // returns 10 for equal pairs
  static constexpr uint32_t pair_equal_mask(uint32_t lhs, uint32_t rhs) {
    lhs ^= rhs;
    uint32_t lhsSC = ~(((lhs & REP01) + REP01) | lhs | REP01);
    return lhsSC;
  }

  // could do specialize these with the fancy AVX512 LUT function
  static constexpr uint32_t and_4state(uint32_t lhs, uint32_t rhs) {
    uint32_t lhsSC = pair_equal_mask(lhs, REP00);
    uint32_t rhsSC = pair_equal_mask(rhs, REP00);

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
  static constexpr uint32_t or_4state(uint32_t lhs, uint32_t rhs) {
    uint32_t lhsSC = pair_equal_mask(lhs, REP01);
    uint32_t rhsSC = pair_equal_mask(rhs, REP01);

    uint32_t lhsX = (lhs & REP10);
    lhsX |= lhsX >> 1;

    uint32_t rhsX = (rhs & REP10);
    rhsX |= rhsX >> 1;

    return (((lhs | rhs) | lhsX | rhsX) & ~(lhsSC | rhsSC)) | (lhsSC >> 1) |
           (rhsSC >> 1);
  }
  static void test() {
    static_assert(HWBigInt::and_4state(0b00'00'01'01'00'10'10'11,
                                       0b00'01'00'01'10'00'01'01) ==
                  0b00'00'00'01'00'00'11'11);
    static_assert(HWBigInt::or_4state(0b00'01'10'10'11, 0b01'00'01'00'00) ==
                  0b01'01'01'11'11);
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
    x = (x | (x << 8)) & 0x00FF00FF;
    x = (x | (x << 4)) & 0x0F0F0F0F;
    x = (x | (x << 2)) & 0x33333333;
    x = (x | (x << 1)) & 0x55555555;

    return x;
  }

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

  void conv4To2StateIfPossible() {
    if (!getCustom())
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

public:
  using BigInt::BigInt;
  HWBigInt(const BigInt &b) : BigInt(b) {}
  HWBigInt(BigInt &&b) : BigInt(std::move(b)) {}

  template <auto Func4S, auto Func2S, typename T0, typename T1>
  static void bitwiseOp(HWBigInt &out, const T0 &lhs, const T1 &rhs) {
    if (!lhs.getCustom() && !rhs.getCustom()) {
      Func2S(out, lhs, rhs);
      return;
    }
    out.words.resize(std::max(lhs.getNumWords(), rhs.getNumWords()));

    uint32_t hasUnk = 0;

    for (size_t i = 0; i < out.getNumWords(); i++) {
      uint32_t lhsV = lhs.getCustom()
                          ? lhs.getWord(i)
                          : unpack_bits(lhs.getWord(i / 2) >> ((i % 2) * 16));
      uint32_t rhsV = rhs.getCustom()
                          ? rhs.getWord(i)
                          : unpack_bits(rhs.getWord(i / 2) >> ((i % 2) * 16));
      uint32_t outV = Func4S(lhsV, rhsV);
      out.words[i] = outV;
      hasUnk |= outV & REP10;
    }

    Extend{out.field} =
        Func4S(lhs.getCustom() ? lhs.getExtend() : unpack_bits(lhs.getExtend()),
               rhs.getCustom() ? rhs.getExtend()
                               : unpack_bits(rhs.getExtend())) &
        0b11;

    if (!hasUnk && (!out.isExtended() || !(Extend{out.field} & 0b10)))
      out.conv4To2State();

    out.normalize();
  }

  template <typename T0, typename T1>
  static void andOp(HWBigInt &out, const T0 &lhs, const T1 &rhs) {
    return bitwiseOp<and_4state, BigInt::addOp<T0, T1>, T0, T1>(out, lhs, rhs);
  }
  template <typename T0, typename T1>
  static void orOp(HWBigInt &out, const T0 &lhs, const T1 &rhs) {
    return bitwiseOp<or_4state, BigInt::orOp, T0, T1>(out, lhs, rhs);
  }
  template <typename T0, typename T1>
  static void xorOp(HWBigInt &out, const T0 &lhs, const T1 &rhs) {
    return bitwiseOp<xor_4state, BigInt::xorOp, T0, T1>(out, lhs, rhs);
  }
  template <typename T0> static void notOp(HWBigInt &out, const T0 &lhs) {
    return xorOp(out, lhs, PatBigInt{lhs.getNumBits(), 0b11});
  }

  template <auto Func2S, typename T0, typename T1>
  static void linearOp(HWBigInt &out, const T0 &lhs, const T1 &rhs) {
    if (lhs.getCustom() || rhs.getCustom()) {
      out.setRepeating(EXTX_MASK, std::max(lhs.getNumBits(), rhs.getNumBits()));
      return;
    }
    Func2S(out, lhs, rhs);
  }

  template <typename T0, typename T1>
  static void addOp(HWBigInt &out, const T0 &lhs, const T1 &rhs) {
    linearOp<BigInt::addOp>(out, lhs, rhs);
  }
  template <typename T0, typename T1>
  static void subOp(HWBigInt &out, const T0 &lhs, const T1 &rhs) {
    linearOp<BigInt::subOp>(out, lhs, rhs);
  }

  template <typename T0, typename T1>
  static void shlOp(HWBigInt &out, const T0 &lhs, unsigned rhs) {
    BigInt::shlOp(out, lhs, lhs.getCustom() ? 2 * rhs : rhs);
    out.conv4To2StateIfPossible();
  }
  template <typename T0, typename T1>
  static void lshrOp(HWBigInt &out, const T0 &lhs, unsigned rhs) {
    BigInt::lshrOp(out, lhs, lhs.getCustom() ? 2 * rhs : rhs);
    out.conv4To2StateIfPossible();
  }
  template <typename T0, typename T1>
  static void ashrOp(HWBigInt &out, const T0 &lhs, unsigned rhs) {
    BigInt::ashrOp(out, lhs, lhs.getCustom() ? 2 * rhs : rhs);
    out.conv4To2StateIfPossible();
  }

  template <int Mode, typename T>
  static void resizeOp(HWBigInt &out, const T &lhs, uint32_t newSize) {
    if (lhs.getCustom()) {
      BigInt::resizeOp<Mode>(out, lhs, 2 * newSize);
      out.conv4To2StateIfPossible();
    }
  }

  template <typename T0, typename T1>
  static auto udivmodOp(const T0 &lhs, const T1 &rhs) {
    if (lhs.getCustom() || rhs.getCustom()) {
      HWBigInt outA;
      HWBigInt outB;
      outA.setRepeating(EXTX_MASK, lhs.getNumBits());
      outB.setRepeating(EXTX_MASK, lhs.getNumBits());
      return std::make_pair(std::move(outA), std::move(outB));
    }
    auto [div, rem] = BigInt::udivmodOp(lhs, rhs);
    return std::make_pair(HWBigInt{div}, HWBigInt{rem});
  }

  template <int Mode = 0, typename T0, typename T1>
  static auto mulOp(const T0 &lhs, const T1 &rhs) {
    if (lhs.getCustom() || rhs.getCustom()) {
      HWBigInt out;
      out.setRepeating(EXTX_MASK,
                       Mode == 0 ? std::max(lhs.getNumBits(), rhs.getNumBits())
                                 : lhs.getNumBits() + rhs.getNumBits());
      return out;
    }
    return HWBigInt{BigInt::mulOp<Mode>(lhs, rhs)};
  }

  template <typename T>
  static void stream_hex(std::ostream &os, const T &self) {
    if (!self.getCustom())
      return BigInt::stream_hex(os, self);

    constexpr size_t digits_per_word = 4;
    constexpr size_t separator_per = 8;

    size_t realBits = self.getNumBits() / 2;
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
        os << '\'';
    }

    for (ssize_t i = 0; i < (ssize_t)wordHexDigits; i++) {
      size_t digit = hexDigits - extendDigits - i - 1;
      size_t idx = (wordHexDigits - i - 1) / digits_per_word;
      size_t sub = (wordHexDigits - i - 1) % digits_per_word;

      uint32_t val = self.getWords()[idx] >> (sub * 8);
      os << toHex4S(val);

      if (digit != 0 && digit % separator_per == 0)
        os << "\'";
    }
  }
};

class HWConstantRef : public ConstantRef {
  using HasUnknown = ConstantRef::Custom;
public:
  using ConstantRef::ConstantRef;
};

class HWConstantBuilder : public ConstantBuilderBase<HWBigInt> {
public:
  using ConstantBuilderBase<HWBigInt>::ConstantBuilderBase;

  HWConstantBuilder &raw(unsigned bits, std::span<uint32_t> data,
                         bool hasUnk = false, uint8_t extend = 0) {
    cur = HWBigInt{BigInt::fromRaw(data, bits, extend, hasUnk)};
    return *this;
  }
  HWConstantBuilder &raw(unsigned bits, SmallVecImpl<uint32_t> &&data,
                         bool hasUnk = false, uint8_t extend = 0) {
    cur = BigInt::fromRaw(std::move(data), bits, extend, hasUnk);
    return *this;
  }

  operator HWConstantRef() { return HWConstantRef{get()}; }
};

} // namespace dyno
