#include "dyno/Constant.h"
#include <algorithm>
#include <array>
#include <cstdio>
#include <cstdlib>
#include <random>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

using namespace dyno;

static uint64_t testCount = 0;
static uint64_t failCount = 0;

#define CHECK(cond, msg, ...)                                                  \
  do {                                                                         \
    testCount++;                                                               \
    if (!(cond)) {                                                             \
      failCount++;                                                             \
      fprintf(stderr, "FAIL (test #%lu): " msg "\n", (unsigned long)testCount, \
              ##__VA_ARGS__);                                                  \
      if (failCount >= 20) {                                                   \
        fprintf(stderr, "Too many failures, aborting.\n");                     \
        exit(1);                                                               \
      }                                                                        \
    }                                                                          \
  } while (0)

// ---------------------------------------------------------------------------
// FourBitVec: 4-state bit-level reference for cross-checking all ops
// ---------------------------------------------------------------------------
struct FourBitVec {
  std::vector<FourState> bits;

  static FourBitVec fromBigInt(const BigInt &v) {
    FourBitVec rv;
    rv.bits.resize(v.getNumBits());
    for (uint32_t i = 0; i < v.getNumBits(); i++)
      rv.bits[i] = v.getBit(i);
    return rv;
  }

  FourBitVec range(uint32_t off, uint32_t len) const {
    FourBitVec rv;
    rv.bits.resize(len);
    for (uint32_t i = 0; i < len && off + i < bits.size(); i++)
      rv.bits[i] = bits[off + i];
    return rv;
  }

  FourBitVec concatWith(const FourBitVec &other) const {
    FourBitVec rv;
    rv.bits.resize(bits.size() + other.bits.size());
    for (size_t i = 0; i < other.bits.size(); i++)
      rv.bits[i] = other.bits[i];
    for (size_t i = 0; i < bits.size(); i++)
      rv.bits[other.bits.size() + i] = bits[i];
    return rv;
  }

  FourBitVec insertAt(const FourBitVec &other, uint32_t addr) const {
    assert(addr + other.bits.size() <= bits.size());
    FourBitVec rv;
    rv.bits = bits;
    for (size_t i = 0; i < other.bits.size(); i++)
      rv.bits[addr + i] = other.bits[i];
    return rv;
  }

  FourBitVec shl(uint32_t amt) const {
    FourBitVec rv;
    rv.bits.resize(bits.size(), FourState::S0);
    for (uint32_t i = 0; i + amt < bits.size(); i++)
      rv.bits[i + amt] = bits[i];
    return rv;
  }

  FourBitVec lshr(uint32_t amt) const {
    FourBitVec rv;
    rv.bits.resize(bits.size(), FourState::S0);
    for (uint32_t i = amt; i < bits.size(); i++)
      rv.bits[i - amt] = bits[i];
    return rv;
  }

  FourBitVec ashr(uint32_t amt) const {
    FourBitVec rv;
    rv.bits.resize(bits.size());
    FourState sign = bits.size() ? bits.back() : FourState::S0;
    for (uint32_t i = 0; i < bits.size(); i++) {
      if (i + amt < bits.size())
        rv.bits[i] = bits[i + amt];
      else
        rv.bits[i] = sign;
    }
    return rv;
  }

  FourBitVec repeat(uint32_t count) const {
    FourBitVec rv;
    rv.bits.resize(bits.size() * count);
    for (uint32_t r = 0; r < count; r++)
      for (size_t i = 0; i < bits.size(); i++)
        rv.bits[r * bits.size() + i] = bits[i];
    return rv;
  }

  FourBitVec bitwiseOp(const FourBitVec &other,
                       FourState (*func)(FourState, FourState)) const {
    assert(bits.size() == other.bits.size());
    FourBitVec rv;
    rv.bits.resize(bits.size());
    for (size_t i = 0; i < bits.size(); i++)
      rv.bits[i] = func(bits[i], other.bits[i]);
    return rv;
  }

  FourBitVec unaryOp(FourState (*func)(FourState)) const {
    FourBitVec rv;
    rv.bits.resize(bits.size());
    for (size_t i = 0; i < bits.size(); i++)
      rv.bits[i] = func(bits[i]);
    return rv;
  }

  // Bitwise add/sub for 2-state. Returns nullopt if any 4-state bit.
  static std::optional<std::pair<FourBitVec, FourBitVec>>
  addSub(const FourBitVec &a, const FourBitVec &b) {
    size_t n = a.bits.size();
    FourBitVec sum, diff;
    sum.bits.resize(n);
    diff.bits.resize(n);
    bool carry = false, borrow = false;
    for (size_t i = 0; i < n; i++) {
      if (a.bits[i].val > FourState::S1 || b.bits[i].val > FourState::S1)
        return std::nullopt;
      bool ai = a.bits[i].val == FourState::S1;
      bool bi = b.bits[i].val == FourState::S1;
      uint32_t s = ai + bi + carry;
      sum.bits[i] = (s & 1) ? FourState::S1 : FourState::S0;
      carry = s >= 2;
      int32_t d = (int32_t)ai - (int32_t)bi - (int32_t)borrow;
      diff.bits[i] = (d & 1) ? FourState::S1 : FourState::S0;
      borrow = d < 0;
    }
    return std::make_pair(sum, diff);
  }

  uint32_t countSetBits() const {
    uint32_t cnt = 0;
    for (auto b : bits)
      if (b.val == FourState::S1) cnt++;
    return cnt;
  }

  bool reductionXor() const {
    bool r = false;
    for (auto b : bits) {
      if (b.val > FourState::S1) return false;
      r ^= (b.val == FourState::S1);
    }
    return r;
  }

  bool operator==(const FourBitVec &o) const {
    return bits.size() == o.bits.size() &&
           std::equal(bits.begin(), bits.end(), o.bits.begin(),
                      [](FourState a, FourState b) { return a.val == b.val; });
  }
};

// ---------------------------------------------------------------------------
// Randomization helpers
// ---------------------------------------------------------------------------
enum class ExtPattern { All0, All1, Alt01, Alt10 };
enum class FourStateExt { FS0, FS1, FZ, FX };

static void setLeadingExtend2S(BigInt &src, ExtPattern pat, uint32_t nBits) {
  uint32_t total = src.getNumBits();
  for (uint32_t j = 0; j < nBits && j < total; j++) {
    uint32_t idx = total - j - 1;
    FourState val;
    switch (pat) {
    case ExtPattern::All0: val = FourState::S0; break;
    case ExtPattern::All1: val = FourState::S1; break;
    case ExtPattern::Alt01: val = (j & 1) ? FourState::S1 : FourState::S0; break;
    case ExtPattern::Alt10: val = (j & 1) ? FourState::S0 : FourState::S1; break;
    }
    src.setBit(idx, val);
  }
}

static void setLeadingExtend4S(BigInt &src, FourStateExt pat, uint32_t nBits) {
  uint32_t total = src.getNumBits();
  for (uint32_t j = 0; j < nBits && j < total; j++) {
    uint32_t idx = total - j - 1;
    FourState val;
    switch (pat) {
    case FourStateExt::FS0: val = FourState::S0; break;
    case FourStateExt::FS1: val = FourState::S1; break;
    case FourStateExt::FZ:  val = FourState::SZ; break;
    case FourStateExt::FX:  val = FourState::SX; break;
    }
    src.setBit(idx, val);
  }
}

static BigInt randomizeExtended(std::mt19937 &rng, uint32_t bits, bool force4S) {
  BigInt src = BigInt::fromU64(0, bits);
  if (force4S && bits > 0) {
    do { src = BigInt::fromU64(0, bits); src.randomize4S(rng); }
    while (!src.getIs4S());
    FourStateExt pats[] = { FourStateExt::FS0, FourStateExt::FS1, FourStateExt::FZ, FourStateExt::FX };
    setLeadingExtend4S(src, pats[rng() % 4], rng() % (bits + 1));
  } else {
    src.randomize(rng);
    ExtPattern pats[] = { ExtPattern::All0, ExtPattern::All1, ExtPattern::Alt01, ExtPattern::Alt10 };
    setLeadingExtend2S(src, pats[rng() % 4], rng() % (bits + 1));
  }
  return src;
}

static BigInt randomizeExtended(std::mt19937 &rng, uint32_t bits) {
  return randomizeExtended(rng, bits, rng() % 2);
}

static BigInt randomizeForceExtended2S(std::mt19937 &rng, uint32_t bits, ExtPattern pat) {
  BigInt src = BigInt::fromU64(0, bits);
  src.randomize(rng);
  setLeadingExtend2S(src, pat, bits);
  return src;
}

static BigInt randomizeForceExtended4S(std::mt19937 &rng, uint32_t bits, FourStateExt pat) {
  BigInt src = BigInt::fromU64(0, bits);
  do { src = BigInt::fromU64(0, bits); src.randomize4S(rng); }
  while (!src.getIs4S());
  setLeadingExtend4S(src, pat, bits);
  return src;
}

// ---------------------------------------------------------------------------
// _ExtInt reference (clang-only)
// ---------------------------------------------------------------------------
#ifdef __clang__

// Bitwise equality: compares bit values, not internal storage
static bool bigIntBitEq(const BigInt &a, const BigInt &b) {
  uint32_t n = std::min(a.getNumBits(), b.getNumBits());
  if (a.getNumBits() != b.getNumBits()) return false;
  for (uint32_t i = 0; i < n; i++)
    if (a.getBit(i) != b.getBit(i)) return false;
  return true;
}

template <unsigned N> struct ExtInt {
  using T = typename std::conditional<N <= 64, unsigned long long,
                                      unsigned _ExtInt(N)>::type;
};

template <unsigned N>
typename ExtInt<N>::T bigIntToEI(const BigInt &v) {
  using T = typename ExtInt<N>::T;
  T val = 0;
  uint32_t bits = std::min(v.getNumBits(), N);
  for (uint32_t i = 0; i < bits; i++)
    if (v.getBit(i) == FourState::S1)
      val |= (T(1) << i);
  return val;
}

template <unsigned N>
BigInt eiToBigInt(typename ExtInt<N>::T val, uint32_t bits) {
  using T = typename ExtInt<N>::T;
  BigInt v = BigInt::fromU64(0, bits);
  for (uint32_t i = 0; i < bits; i++)
    if (val & (T(1) << i))
      v.setBit(i, FourState::S1);
  return v;
}

template <unsigned N>
typename ExtInt<N>::T eiMask() {
  using T = typename ExtInt<N>::T;
  if constexpr (N < 64)
    return (T(1) << N) - 1;
  else
    return ~T(0);
}

template <unsigned N>
typename ExtInt<N>::T eiSignExt(typename ExtInt<N>::T val) {
  using T = typename ExtInt<N>::T;
  T signBit = T(1) << (N - 1);
  if (val & signBit) val |= ~eiMask<N>();
  else val &= eiMask<N>();
  return val;
}

constexpr std::array<unsigned, 59> ExtIntSizes = {
  1, 2, 3, 7, 8, 11, 13, 15, 16, 19, 23, 27, 31, 32, 33, 37, 41, 47, 53,
  63, 64, 65, 71, 79, 96, 97, 103, 107, 127, 128, 129, 131, 137, 149,
  191, 192, 193, 199, 251, 255, 256, 257, 263, 311, 384, 511, 512, 513,
  521, 640, 767, 768, 769, 896, 1023, 1024, 1025, 1031
};

#endif // __clang__

// ---------------------------------------------------------------------------
// Existing tests (cleaned up)
// ---------------------------------------------------------------------------

static void testRangeSelectBitwise(std::mt19937 &rng) {
  for (int i = 0; i < 5000; i++) {
    uint32_t bits = rng() % 128 + 1;
    BigInt src = BigInt::fromU64(0, bits); src.randomize(rng);
    uint32_t len = rng() % (bits + 1);
    uint32_t off = bits > len ? rng() % (bits - len + 1) : 0;
    BigInt out;
    BigInt::rangeSelectOp(out, src, off, len);
    CHECK(out.getNumBits() == len, "rangeSelect numBits off=%u len=%u srcBits=%u", off, len, bits);
    CHECK(FourBitVec::fromBigInt(out) == FourBitVec::fromBigInt(src).range(off, len),
          "rangeSelect mismatch off=%u len=%u srcBits=%u", off, len, bits);
  }
}

static void testConcatBitwise(std::mt19937 &rng) {
  for (int i = 0; i < 5000; i++) {
    uint32_t lBits = rng() % 49;
    uint32_t rBits = rng() % (49 - lBits);
    BigInt lhs = BigInt::fromU64(0, lBits); if (lBits) lhs.randomize(rng);
    BigInt rhs = BigInt::fromU64(0, rBits); if (rBits) rhs.randomize(rng);
    BigInt out;
    BigInt::concatOp(out, lhs, rhs);
    CHECK(out.getNumBits() == lBits + rBits, "concat numBits lhs=%u rhs=%u got=%u", lBits, rBits, (unsigned)out.getNumBits());
    if (lBits == 0) { CHECK(out == rhs, "concat zero-lhs mismatch"); continue; }
    if (rBits == 0) { CHECK(out == lhs, "concat zero-rhs mismatch"); continue; }
    CHECK(FourBitVec::fromBigInt(out) == FourBitVec::fromBigInt(lhs).concatWith(FourBitVec::fromBigInt(rhs)),
          "concat bitwise mismatch l=%u r=%u", lBits, rBits);
  }
}

static void testShlBitwise(std::mt19937 &rng) {
  for (int i = 0; i < 5000; i++) {
    uint32_t bits = rng() % 128 + 1;
    BigInt src = BigInt::fromU64(0, bits); src.randomize(rng);
    uint32_t amt = rng() % (bits + 2);
    BigInt out;
    BigInt::shlOp(out, src, amt);
    CHECK(FourBitVec::fromBigInt(out) == FourBitVec::fromBigInt(src).shl(amt), "shl mismatch bits=%u amt=%u", bits, amt);
  }
}

static void testLshrBitwise(std::mt19937 &rng) {
  for (int i = 0; i < 5000; i++) {
    uint32_t bits = rng() % 128 + 1;
    BigInt src = BigInt::fromU64(0, bits); src.randomize(rng);
    uint32_t amt = rng() % (bits + 2);
    BigInt out;
    BigInt::lshrOp(out, src, amt);
    CHECK(FourBitVec::fromBigInt(out) == FourBitVec::fromBigInt(src).lshr(amt), "lshr mismatch bits=%u amt=%u", bits, amt);
  }
}

static void testAshrSignExtend(std::mt19937 &rng) {
  for (int i = 0; i < 3000; i++) {
    uint32_t bits = rng() % 65 + 1;
    BigInt src = BigInt::fromU64(0, bits); src.randomize(rng);
    uint32_t amt = rng() % (bits + 1);
    if (amt == 0) continue;
    BigInt out; BigInt::ashrOp(out, src, amt);
    FourState sign = src.getBit(bits - 1);
    for (uint32_t b = bits - amt; b < bits; b++) {
      CHECK(out.getBit(b) == sign,
            "ashr sign-extend bits=%u amt=%u bit=%u sign=%u got=%u",
            bits, amt, b, (unsigned)sign, (unsigned)out.getBit(b));
    }
  }
}

static void testAshrVsLshr(std::mt19937 &rng) {
  for (int i = 0; i < 3000; i++) {
    uint32_t bits = rng() % 65 + 1;
    BigInt src = BigInt::fromU64(0, bits); src.randomize(rng);
    src.setBit(bits - 1, FourState::S0);
    uint32_t amt = rng() % (bits + 2);
    BigInt ashrOut, lshrOut;
    BigInt::ashrOp(ashrOut, src, amt);
    BigInt::lshrOp(lshrOut, src, amt);
    CHECK(ashrOut == lshrOut, "ashr!=lshr (sign=0) bits=%u amt=%u", bits, amt);
  }
}

static void testAshrVsLshrSigned(std::mt19937 &rng) {
  for (int i = 0; i < 3000; i++) {
    uint32_t bits = rng() % 65 + 1;
    BigInt src = BigInt::fromU64(0, bits); src.randomize(rng);
    src.setBit(bits - 1, FourState::S1);
    uint32_t amt = rng() % (bits + 1);
    if (amt == 0 || amt >= bits) continue;
    BigInt ashrOut, lshrOut;
    BigInt::ashrOp(ashrOut, src, amt);
    BigInt::lshrOp(lshrOut, src, amt);
    BigInt ashrLo, lshrLo;
    BigInt::rangeSelectOp(ashrLo, ashrOut, 0, bits - amt);
    BigInt::rangeSelectOp(lshrLo, lshrOut, 0, bits - amt);
    ashrLo.normalize(); lshrLo.normalize();
    CHECK(ashrLo == lshrLo, "ashr low bits!=lshr bits=%u amt=%u", bits, amt);
  }
}

static void testInsertExtractRoundtrip(std::mt19937 &rng) {
  for (int i = 0; i < 32000; i++) {
    uint32_t dstBits = rng() % 128 + 1;
    uint32_t insBits = rng() % 33 + 1;
    if (dstBits < insBits) continue;
    BigInt dst = BigInt::fromU64(0, dstBits); dst.randomize(rng);
    BigInt ins = BigInt::fromU64(0, insBits); ins.randomize(rng);
    uint32_t addr = rng() % (dstBits - insBits + 1);
    BigInt out;
    BigInt::insertOp(out, dst, ins, addr);
    BigInt extracted;
    BigInt::rangeSelectOp(extracted, out, addr, insBits);
    CHECK(extracted == ins, "insert-extract failed dst=%u ins=%u addr=%u", dstBits, insBits, addr);
  }
}

static void testConcatExtractRoundtrip(std::mt19937 &rng) {
  for (int i = 0; i < 8000; i++) {
    uint32_t lBits = rng() % 97 + 1;
    uint32_t rBits = rng() % 97 + 1;
    BigInt lhs = BigInt::fromU64(0, lBits); lhs.randomize(rng);
    BigInt rhs = BigInt::fromU64(0, rBits); rhs.randomize(rng);
    BigInt out;
    BigInt::concatOp(out, lhs, rhs);
    BigInt hi; BigInt::rangeSelectOp(hi, out, rBits, lBits);
    CHECK(hi == lhs, "concat-extract hi failed l=%u r=%u", lBits, rBits);
    BigInt lo; BigInt::rangeSelectOp(lo, out, 0, rBits);
    CHECK(lo == rhs, "concat-extract lo failed l=%u r=%u", lBits, rBits);
  }
}

static void testShiftRoundtrip(std::mt19937 &rng) {
  for (int i = 0; i < 8000; i++) {
    uint32_t bits = rng() % 128 + 1;
    BigInt src = BigInt::fromU64(0, bits); src.randomize(rng);
    uint32_t amt = rng() % (bits + 1);
    BigInt shifted, recovered;
    BigInt::shlOp(shifted, src, amt);
    BigInt::lshrOp(recovered, shifted, amt);
    CHECK(FourBitVec::fromBigInt(recovered) == FourBitVec::fromBigInt(src).shl(amt).lshr(amt),
          "shl-lshr roundtrip bits=%u amt=%u", bits, amt);
  }
}

static void testExtractReconstruct(std::mt19937 &rng) {
  for (int i = 0; i < 5000; i++) {
    uint32_t bits = rng() % 128 + 1;
    BigInt src = BigInt::fromU64(0, bits); src.randomize(rng);
    uint32_t split = rng() % (bits + 1);
    BigInt lo, hi;
    BigInt::rangeSelectOp(lo, src, 0, split);
    BigInt::rangeSelectOp(hi, src, split, bits - split);
    BigInt reconstructed;
    BigInt::concatOp(reconstructed, hi, lo);
    CHECK(reconstructed == src, "extract-reconstruct failed bits=%u split=%u", bits, split);
  }
}

static void testConcatAssoc(std::mt19937 &rng) {
  for (int i = 0; i < 5000; i++) {
    uint32_t aBits = rng() % 49 + 1, bBits = rng() % 49 + 1, cBits = rng() % 49 + 1;
    BigInt A = BigInt::fromU64(0, aBits); A.randomize(rng);
    BigInt B = BigInt::fromU64(0, bBits); B.randomize(rng);
    BigInt C = BigInt::fromU64(0, cBits); C.randomize(rng);
    BigInt bc, abc, ab, abc2;
    BigInt::concatOp(bc, B, C); BigInt::concatOp(abc, A, bc);
    BigInt::concatOp(ab, A, B); BigInt::concatOp(abc2, ab, C);
    CHECK(abc == abc2, "concat associativity failed a=%u b=%u c=%u", aBits, bBits, cBits);
  }
}

static void testInsertVsConcat(std::mt19937 &rng) {
  for (int i = 0; i < 5000; i++) {
    uint32_t dstBits = rng() % 65 + 1;
    uint32_t insBits = rng() % 33 + 1;
    BigInt dst = BigInt::fromU64(0, dstBits); dst.randomize(rng);
    BigInt ins = BigInt::fromU64(0, insBits); ins.randomize(rng);
    uint32_t maxAddr = dstBits >= insBits ? dstBits - insBits : 0;
    uint32_t addr = rng() % (maxAddr + 1);
    if (int64_t(dstBits) - addr - insBits < 0) continue;
    BigInt viaInsert;
    BigInt::insertOp(viaInsert, dst, ins, addr);
    BigInt loPart, hiPart;
    BigInt::rangeSelectOp(loPart, dst, 0, addr);
    BigInt::rangeSelectOp(hiPart, dst, addr + insBits, dstBits - addr - insBits);
    BigInt inner, viaConcat;
    BigInt::concatOp(inner, ins, loPart);
    BigInt::concatOp(viaConcat, hiPart, inner);
    CHECK(viaInsert == viaConcat, "insert-vs-concat dst=%u ins=%u addr=%u", dstBits, insBits, addr);
  }
}

static void testShiftVsConcat(std::mt19937 &rng) {
  for (int i = 0; i < 5000; i++) {
    uint32_t bits = rng() % 128 + 1;
    BigInt src = BigInt::fromU64(0, bits); src.randomize(rng);
    uint32_t amt = rng() % bits;
    if (amt == 0) continue;
    BigInt shifted; BigInt::shlOp(shifted, src, amt);
    BigInt bottomBits; BigInt::rangeSelectOp(bottomBits, src, 0, bits - amt);
    BigInt loZeros = BigInt::fromU64(0, amt);
    BigInt viaConcat; BigInt::concatOp(viaConcat, bottomBits, loZeros);
    CHECK(shifted == viaConcat, "shl-vs-concat mismatch bits=%u amt=%u", bits, amt);
  }
}

static void testLshrVsRangeSelect(std::mt19937 &rng) {
  for (int i = 0; i < 5000; i++) {
    uint32_t bits = rng() % 128 + 1;
    BigInt src = BigInt::fromU64(0, bits); src.randomize(rng);
    uint32_t amt = rng() % bits;
    if (amt == 0) continue;
    BigInt shifted, top, shiftedTrimmed;
    BigInt::lshrOp(shifted, src, amt);
    BigInt::rangeSelectOp(top, src, amt, bits - amt);
    BigInt::rangeSelectOp(shiftedTrimmed, shifted, 0, bits - amt);
    CHECK(shiftedTrimmed == top, "lshr-vs-rangeSelect mismatch bits=%u amt=%u", bits, amt);
  }
}

static void testMultiConcatExtract(std::mt19937 &rng) {
  for (int i = 0; i < 3000; i++) {
    int nParts = rng() % 5 + 2;
    std::vector<BigInt> parts;
    uint32_t totalBits = 0;
    for (int p = 0; p < nParts; p++) {
      uint32_t pBits = rng() % 33 + 1;
      parts.push_back(BigInt::fromU64(0, pBits));
      parts.back().randomize(rng);
      totalBits += pBits;
    }
    BigInt acc = BigInt::ofLen(0);
    for (int p = 0; p < nParts; p++)
      BigInt::concatOp(acc, parts[p], acc);
    CHECK(acc.getNumBits() == totalBits, "multi-concat wrong total bits");
    uint32_t running = 0;
    for (int p = 0; p < nParts; p++) {
      BigInt ext;
      BigInt::rangeSelectOp(ext, acc, running, parts[p].getNumBits());
      CHECK(ext == parts[p], "multi-concat extract part %d failed", p);
      running += parts[p].getNumBits();
    }
  }
}

static void testShiftZero(std::mt19937 &rng) {
  for (int i = 0; i < 3000; i++) {
    uint32_t bits = rng() % 128 + 1;
    BigInt src = BigInt::fromU64(0, bits); src.randomize(rng);
    BigInt out;
    BigInt::shlOp(out, src, 0); CHECK(out == src, "shl by 0 not identity");
    BigInt::lshrOp(out, src, 0); CHECK(out == src, "lshr by 0 not identity");
    BigInt::ashrOp(out, src, 0); CHECK(out == src, "ashr by 0 not identity");
  }
}

static void testShiftOvershoot(std::mt19937 &rng) {
  for (int i = 0; i < 3000; i++) {
    uint32_t bits = rng() % 65 + 1;
    BigInt src = BigInt::fromU64(0, bits); src.randomize(rng);
    uint32_t amt = bits + (rng() % 32);
    BigInt out;
    FourBitVec ref = FourBitVec::fromBigInt(src);
    BigInt::shlOp(out, src, amt);
    CHECK(FourBitVec::fromBigInt(out) == ref.shl(amt), "shl overshoot bits=%u amt=%u", bits, amt);
    BigInt::lshrOp(out, src, amt);
    CHECK(FourBitVec::fromBigInt(out) == ref.lshr(amt), "lshr overshoot bits=%u amt=%u", bits, amt);
    BigInt::ashrOp(out, src, amt);
    CHECK(FourBitVec::fromBigInt(out) == ref.ashr(amt), "ashr overshoot bits=%u amt=%u", bits, amt);
  }
}

static void testEdgeSizes(std::mt19937 &rng) {
  std::array<uint32_t, 12> sizes = {1,2,7,8,15,16,31,32,33,64,65,96};
  for (uint32_t bits : sizes) {
    BigInt v = BigInt::fromU64(0, bits); v.randomize(rng);
    BigInt cc; BigInt::concatOp(cc, v, v);
    CHECK(cc.getNumBits() == bits * 2, "edge concat bits=%u", bits);
    BigInt hi, lo;
    BigInt::rangeSelectOp(hi, cc, bits, bits);
    BigInt::rangeSelectOp(lo, cc, 0, bits);
    CHECK(hi == v, "edge concat-extract hi bits=%u", bits);
    CHECK(lo == v, "edge concat-extract lo bits=%u", bits);
    BigInt s1, sr1;
    BigInt::shlOp(s1, v, 1);
    BigInt::lshrOp(sr1, s1, 1);
    CHECK(FourBitVec::fromBigInt(sr1) == FourBitVec::fromBigInt(v).shl(1).lshr(1),
          "edge shl-lshr bits=%u", bits);
    if (bits >= 2) {
      BigInt ins; BigInt::rangeSelectOp(ins, v, 0, bits / 2);
      BigInt out; BigInt::insertOp(out, v, ins, 0);
      BigInt ext; BigInt::rangeSelectOp(ext, out, 0, bits / 2);
      CHECK(ext == ins, "edge insert-extract bits=%u", bits);
    }
  }
}

static void testWordBoundaryCrossing(std::mt19937 &rng) {
  for (uint32_t boundary : {31, 32, 33, 63, 64, 65}) {
    uint32_t bits = boundary + 32;
    BigInt src = BigInt::fromU64(0, bits); src.randomize(rng);
    for (int32_t delta = -2; delta <= 2; delta++) {
      uint32_t off = boundary + delta;
      for (uint32_t len : {1,7,8,15,16,31,32,33}) {
        if (off + (int32_t)len > (int32_t)bits) continue;
        BigInt out; BigInt::rangeSelectOp(out, src, off, len);
        CHECK(FourBitVec::fromBigInt(out) == FourBitVec::fromBigInt(src).range(off, len),
              "word-boundary rangeSelect bits=%u off=%u len=%u", bits, off, len);
      }
    }
    for (int32_t delta = -2; delta <= 2; delta++) {
      uint32_t split = boundary + delta;
      if (split > bits) continue;
      uint32_t loBits = split, hiBits = bits - split;
      BigInt lo = BigInt::fromU64(0, loBits); if (loBits) lo.randomize(rng);
      BigInt hi = BigInt::fromU64(0, hiBits); if (hiBits) hi.randomize(rng);
      BigInt cc; BigInt::concatOp(cc, hi, lo);
      BigInt extLo, extHi;
      BigInt::rangeSelectOp(extLo, cc, 0, loBits);
      BigInt::rangeSelectOp(extHi, cc, loBits, hiBits);
      CHECK(extLo == lo, "word-boundary concat lo bits=%u split=%u", bits, split);
      CHECK(extHi == hi, "word-boundary concat hi bits=%u split=%u", bits, split);
    }
  }
}

static void testConcatZeroBits(std::mt19937 &rng) {
  for (int i = 0; i < 2000; i++) {
    uint32_t bits = rng() % 65 + 1;
    BigInt v = BigInt::fromU64(0, bits); v.randomize(rng);
    BigInt zero0 = BigInt::fromU64(0, 0);
    BigInt out;
    BigInt::concatOp(out, v, zero0); CHECK(out == v, "concat(v, zero) != v bits=%u", bits);
    BigInt::concatOp(out, zero0, v); CHECK(out == v, "concat(zero, v) != v bits=%u", bits);
    BigInt::concatOp(out, zero0, zero0); CHECK(out.getNumBits() == 0, "concat(zero, zero) != zero");
  }
}

static void testRangeSelectZeroLen(std::mt19937 &rng) {
  for (int i = 0; i < 1000; i++) {
    uint32_t bits = rng() % 65 + 1;
    BigInt src = BigInt::fromU64(0, bits); src.randomize(rng);
    BigInt out;
    BigInt::rangeSelectOp(out, src, 0, 0); CHECK(out.getNumBits() == 0, "rangeSelect 0-len wrong size");
    BigInt::rangeSelectOp(out, src, bits, 0); CHECK(out.getNumBits() == 0, "rangeSelect 0-len at end wrong size");
  }
}

static void testTruncateVsRangeSelect(std::mt19937 &rng) {
  for (int i = 0; i < 20000; i++) {
    uint32_t bits = rng() % 512 + 1;
    uint32_t extendBits = rng() % bits;
    BigInt src = BigInt::fromU64(0, bits); src.randomize(rng);
    for (uint32_t j = 0; j < extendBits; j++) {
      auto idx = src.getNumBits() - j - 1;
      src.setBit(idx, (idx & 1) ? FourState::S1 : FourState::S0);
    }
    uint32_t truncBits = rng() % bits;
    BigInt viaResize, viaRange;
    BigInt::resizeOp(viaResize, src, truncBits);
    BigInt::rangeSelectOp(viaRange, src, 0, truncBits);
    CHECK(viaResize == viaRange, "truncate vs rangeSelect(0) mismatch srcBits=%u truncBits=%u", bits, truncBits);
    FourBitVec refTrunc = FourBitVec::fromBigInt(src).range(0, truncBits);
    CHECK(FourBitVec::fromBigInt(viaResize) == refTrunc, "truncate bitwise mismatch srcBits=%u truncBits=%u", bits, truncBits);
    CHECK(FourBitVec::fromBigInt(viaRange) == refTrunc, "rangeSelect(0) bitwise mismatch srcBits=%u truncBits=%u", bits, truncBits);
  }
}

static void testTruncateVsRangeSelectExtended(std::mt19937 &rng) {
  for (int i = 0; i < 10000; i++) {
    uint32_t bits = rng() % 256 + 1;
    BigInt src = randomizeExtended(rng, bits);
    uint32_t truncBits = rng() % bits;
    BigInt viaResize, viaRange;
    if (src.getIs4S()) {
      BigInt::resizeOp4S(viaResize, src, truncBits);
      BigInt::rangeSelectOp4S(viaRange, src, 0, truncBits);
    } else {
      BigInt::resizeOp(viaResize, src, truncBits);
      BigInt::rangeSelectOp(viaRange, src, 0, truncBits);
    }
    CHECK(viaResize == viaRange, "truncate vs rangeSelect(0) ext mismatch bits=%u trunc=%u is4S=%u", bits, truncBits, (unsigned)src.getIs4S());
    FourBitVec refTrunc = FourBitVec::fromBigInt(src).range(0, truncBits);
    CHECK(FourBitVec::fromBigInt(viaResize) == refTrunc, "truncate bitwise ext mismatch bits=%u trunc=%u is4S=%u", bits, truncBits, (unsigned)src.getIs4S());
    CHECK(FourBitVec::fromBigInt(viaRange) == refTrunc, "rangeSelect(0) bitwise ext mismatch bits=%u trunc=%u is4S=%u", bits, truncBits, (unsigned)src.getIs4S());
  }
}

static void testRangeSelectNonZeroExtended(std::mt19937 &rng) {
  for (int i = 0; i < 10000; i++) {
    uint32_t bits = rng() % 256 + 1;
    BigInt src = randomizeExtended(rng, bits);
    uint32_t maxOff = bits > 0 ? bits : 0;
    uint32_t off = rng() % (maxOff + 1);
    uint32_t len = bits > off ? rng() % (bits - off + 1) : 0;
    BigInt viaOp;
    if (src.getIs4S()) BigInt::rangeSelectOp4S(viaOp, src, off, len);
    else               BigInt::rangeSelectOp(viaOp, src, off, len);
    CHECK(viaOp.getNumBits() == len, "rangeSelect nonZero ext wrong numBits bits=%u off=%u len=%u is4S=%u", bits, off, len, (unsigned)src.getIs4S());
    CHECK(FourBitVec::fromBigInt(viaOp) == FourBitVec::fromBigInt(src).range(off, len),
          "rangeSelect nonZero ext bitwise mismatch bits=%u off=%u len=%u is4S=%u", bits, off, len, (unsigned)src.getIs4S());
  }
}

static void testInsertNonZeroExtended(std::mt19937 &rng) {
  for (int i = 0; i < 8000; i++) {
    uint32_t dstBits = rng() % 256 + 1;
    uint32_t insBits = rng() % (dstBits > 1 ? dstBits - 1 : 1) + 1;
    if (insBits > dstBits) continue;
    bool is4S = rng() % 2;
    BigInt dst = randomizeExtended(rng, dstBits, is4S);
    BigInt ins = randomizeExtended(rng, insBits, is4S);
    uint32_t addr = rng() % (dstBits - insBits + 1);
    BigInt viaOp;
    if (is4S) BigInt::insertOp4S(viaOp, dst, ins, addr);
    else      BigInt::insertOp(viaOp, dst, ins, addr);
    CHECK(viaOp.getNumBits() == dstBits, "insert nonZero ext wrong numBits dstBits=%u insBits=%u addr=%u is4S=%u", dstBits, insBits, addr, (unsigned)is4S);
    CHECK(FourBitVec::fromBigInt(viaOp) == FourBitVec::fromBigInt(dst).insertAt(FourBitVec::fromBigInt(ins), addr),
          "insert nonZero ext bitwise mismatch dstBits=%u insBits=%u addr=%u is4S=%u", dstBits, insBits, addr, (unsigned)is4S);
  }
}

static void testConcatExtended(std::mt19937 &rng) {
  for (int i = 0; i < 8000; i++) {
    uint32_t lBits = rng() % 128 + 1;
    uint32_t rBits = rng() % 128 + 1;
    bool is4S = rng() % 2;
    BigInt lhs = randomizeExtended(rng, lBits, is4S);
    BigInt rhs = randomizeExtended(rng, rBits, is4S);
    BigInt viaOp;
    if (is4S) BigInt::concatOp4S(viaOp, lhs, rhs);
    else      BigInt::concatOp(viaOp, lhs, rhs);
    CHECK(viaOp.getNumBits() == lBits + rBits, "concat ext wrong numBits l=%u r=%u got=%u", lBits, rBits, (unsigned)viaOp.getNumBits());
    CHECK(FourBitVec::fromBigInt(viaOp) == FourBitVec::fromBigInt(lhs).concatWith(FourBitVec::fromBigInt(rhs)),
          "concat ext bitwise mismatch l=%u r=%u is4S=%u", lBits, rBits, (unsigned)is4S);
  }
}

static void testExtractInsertRoundtrip(std::mt19937 &rng) {
  for (int i = 0; i < 5000; i++) {
    uint32_t bits = rng() % 128 + 1;
    BigInt src = BigInt::fromU64(0, bits); src.randomize(rng);
    uint32_t off = rng() % bits;
    uint32_t len = rng() % (bits - off) + 1;
    BigInt extracted; BigInt::rangeSelectOp(extracted, src, off, len);
    BigInt zeroFill = BigInt::fromU64(0, len);
    BigInt cleared, restored;
    BigInt::insertOp(cleared, src, zeroFill, off);
    BigInt::insertOp(restored, cleared, extracted, off);
    CHECK(restored == src, "extract-insert roundtrip bits=%u off=%u len=%u", bits, off, len);
  }
}

static void testMultiInsertRangeCheck(std::mt19937 &rng) {
  for (int i = 0; i < 3000; i++) {
    uint32_t dstBits = rng() % 65 + 1;
    BigInt dst = BigInt::fromU64(0, dstBits); dst.randomize(rng);
    uint32_t aBits = rng() % 17 + 1;
    uint32_t bBits = rng() % 17 + 1;
    uint32_t aMax = dstBits >= aBits ? dstBits - aBits : 0;
    uint32_t bMax = dstBits >= bBits ? dstBits - bBits : 0;
    uint32_t aOff = rng() % (aMax + 1);
    uint32_t bOff = rng() % (bMax + 1);
    if (aBits > dstBits || bBits > dstBits) continue;
    BigInt insA = BigInt::fromU64(0, aBits); insA.randomize(rng);
    BigInt insB = BigInt::fromU64(0, bBits); insB.randomize(rng);
    BigInt step1, step2;
    BigInt::insertOp(step1, dst, insA, aOff);
    BigInt::insertOp(step2, step1, insB, bOff);
    BigInt extB;
    BigInt::rangeSelectOp(extB, step2, bOff, bBits);
    CHECK(extB == insB, "multi-insert B dst=%u", dstBits);
    bool overlap = !(aOff + aBits <= bOff || bOff + bBits <= aOff);
    if (!overlap) {
      BigInt extA;
      BigInt::rangeSelectOp(extA, step2, aOff, aBits);
      CHECK(extA == insA, "multi-insert A dst=%u", dstBits);
    }
  }
}

static void testInsertBoundary(std::mt19937 &rng) {
  for (int i = 0; i < 32000; i++) {
    uint32_t dstBits = 516, insBits = 129;
    uint32_t toInsert = dstBits / insBits;
    BigInt dst = BigInt::fromU64(0, dstBits); dst.randomize4S(rng);
    SmallVec<BigInt, 4> ins;
    for (uint32_t j = 0; j < toInsert; j++) {
      auto &ref = ins.emplace_back(BigInt::fromU64(0, insBits));
      ref.randomize4S(rng);
    }
    for (uint32_t j = 0; j < toInsert; j++) {
      uint32_t addr = insBits * j;
      BigInt::insertOp4S(dst, dst, ins[j], addr);
      for (uint32_t k = 0; k <= j; k++) {
        BigInt refIns;
        BigInt::rangeSelectOp4S(refIns, dst, insBits * k, insBits);
        CHECK(refIns == ins[k], "insert boundary failed j=%u k=%u", j, k);
      }
    }
  }
}

// ---------------------------------------------------------------------------
// NEW: Bitwise ops (and, or, xor, xnor, not) with extended + 4S
// ---------------------------------------------------------------------------

static void testBitwiseExtended(std::mt19937 &rng) {
  // 4-state Verilog truth tables
  // AND: 0 short-circuits to 0; x/z propagates to x; else a & b
  // OR:  1 short-circuits to 1; x/z propagates to x; else a | b
  // XOR: x/z propagates to x; else a ^ b
  // XNOR: x/z propagates to x; else a == b
  // NOT: x/z propagates to x; else !a
  auto andFunc = [](FourState a, FourState b) -> FourState {
    if (a.val == FourState::S0 || b.val == FourState::S0) return FourState::S0;
    if (a.val > FourState::S1 || b.val > FourState::S1) return FourState::SX;
    return FourState::S1; };
  auto orFunc = [](FourState a, FourState b) -> FourState {
    if (a.val == FourState::S1 || b.val == FourState::S1) return FourState::S1;
    if (a.val > FourState::S1 || b.val > FourState::S1) return FourState::SX;
    return FourState::S0; };
  auto xorFunc = [](FourState a, FourState b) -> FourState {
    if (a.val > FourState::S1 || b.val > FourState::S1) return FourState::SX;
    return FourState{uint8_t(a.val ^ b.val)}; };
  auto xnorFunc = [](FourState a, FourState b) -> FourState {
    if (a.val > FourState::S1 || b.val > FourState::S1) return FourState::SX;
    return FourState{(a.val == b.val)}; };
  auto notFunc = [](FourState a) -> FourState {
    if (a.val > FourState::S1) return FourState::SX;
    return (!a.val) ? FourState::S1 : FourState::S0; };

  for (int iter = 0; iter < 20000; iter++) {
    uint32_t bits = rng() % 256 + 1;
    bool is4S = rng() % 2;
    BigInt a = randomizeExtended(rng, bits, is4S);
    BigInt b = randomizeExtended(rng, bits, is4S);
    auto runOp = [&](const char *name,
                     void (*op)(BigInt &, const BigInt &, const BigInt &),
                     FourState (*refFunc)(FourState, FourState)) {
      BigInt out; op(out, a, b);
      CHECK(FourBitVec::fromBigInt(out) == FourBitVec::fromBigInt(a).bitwiseOp(FourBitVec::fromBigInt(b), refFunc),
            "%s mismatch bits=%u is4S=%u", name, bits, (unsigned)is4S);
    };
    runOp("and", BigInt::andOp4S, andFunc);
    runOp("or", BigInt::orOp4S, orFunc);
    runOp("xor", BigInt::xorOp4S, xorFunc);
    runOp("xnor", BigInt::xnorOp4S, xnorFunc);
    { BigInt out; BigInt::notOp4S(out, a);
      CHECK(FourBitVec::fromBigInt(out) == FourBitVec::fromBigInt(a).unaryOp(notFunc),
            "not mismatch bits=%u is4S=%u", bits, (unsigned)is4S); }
  }

  // Forced extend patterns 2S
  for (int pi = 0; pi < 4; pi++) {
    ExtPattern pat = (ExtPattern)pi;
    for (int iter = 0; iter < 5000; iter++) {
      uint32_t bits = rng() % 256 + 1;
      BigInt a = randomizeForceExtended2S(rng, bits, pat);
      BigInt b = randomizeForceExtended2S(rng, bits, pat);
      auto runOp = [&](const char *name,
                       void (*op)(BigInt &, const BigInt &, const BigInt &),
                       FourState (*refFunc)(FourState, FourState)) {
        BigInt out; op(out, a, b);
        CHECK(FourBitVec::fromBigInt(out) == FourBitVec::fromBigInt(a).bitwiseOp(FourBitVec::fromBigInt(b), refFunc),
              "%s 2S ext pat=%d bits=%u", name, pi, bits);
      };
      runOp("and", BigInt::andOp4S, andFunc);
      runOp("or", BigInt::orOp4S, orFunc);
      runOp("xor", BigInt::xorOp4S, xorFunc);
      runOp("xnor", BigInt::xnorOp4S, xnorFunc);
      { BigInt out; BigInt::notOp4S(out, a);
        CHECK(FourBitVec::fromBigInt(out) == FourBitVec::fromBigInt(a).unaryOp(notFunc),
              "not 2S ext pat=%d bits=%u", pi, bits); }
    }
  }

  // Forced extend patterns 4S
  for (int pi = 0; pi < 4; pi++) {
    FourStateExt pat = (FourStateExt)pi;
    for (int iter = 0; iter < 3000; iter++) {
      uint32_t bits = rng() % 256 + 1;
      BigInt a = randomizeForceExtended4S(rng, bits, pat);
      BigInt b = randomizeForceExtended4S(rng, bits, pat);
      auto runOp = [&](const char *name,
                       void (*op)(BigInt &, const BigInt &, const BigInt &),
                       FourState (*refFunc)(FourState, FourState)) {
        BigInt out; op(out, a, b);
        CHECK(FourBitVec::fromBigInt(out) == FourBitVec::fromBigInt(a).bitwiseOp(FourBitVec::fromBigInt(b), refFunc),
              "%s 4S ext pat=%d bits=%u", name, pi, bits);
      };
      runOp("and", BigInt::andOp4S, andFunc);
      runOp("or", BigInt::orOp4S, orFunc);
      runOp("xor", BigInt::xorOp4S, xorFunc);
      runOp("xnor", BigInt::xnorOp4S, xnorFunc);
      { BigInt out; BigInt::notOp4S(out, a);
        CHECK(FourBitVec::fromBigInt(out) == FourBitVec::fromBigInt(a).unaryOp(notFunc),
              "not 4S ext pat=%d bits=%u", pi, bits); }
    }
  }
}

// ---------------------------------------------------------------------------
// NEW: _ExtInt arithmetic + shift + pow tests (clang-only)
// ---------------------------------------------------------------------------

#ifdef __clang__

template <unsigned N>
static void testArithmeticExtIntWidth(std::mt19937 &rng) {
  using T = typename ExtInt<N>::T;
  for (int i = 0; i < 1000; i++) {
    T aVal = (T)rng() & eiMask<N>();
    if constexpr (N > 32) aVal |= (T(rng()) << 32) & eiMask<N>();
    if constexpr (N > 64) aVal |= (T(rng()) << 64) & eiMask<N>();
    T bVal = (T)rng() & eiMask<N>();
    if constexpr (N > 32) bVal |= (T(rng()) << 32) & eiMask<N>();
    if constexpr (N > 64) bVal |= (T(rng()) << 64) & eiMask<N>();
    BigInt a = eiToBigInt<N>(aVal, N);
    BigInt b = eiToBigInt<N>(bVal, N);

    // add
    { BigInt out; BigInt::addOp(out, a, b);
      T r = (aVal + bVal) & eiMask<N>();
      CHECK(out == eiToBigInt<N>(r, N), "add bits=%u", N); }
    // sub
    { BigInt out; BigInt::subOp(out, a, b);
      T r = (aVal - bVal) & eiMask<N>();
      CHECK(out == eiToBigInt<N>(r, N), "sub bits=%u", N); }
    // negate
    { BigInt out; BigInt::negateOp(out, a);
      T r = (T(0) - aVal) & eiMask<N>();
      CHECK(out == eiToBigInt<N>(r, N), "negate bits=%u", N); }
    // mul (truncating)
    { BigInt out = BigInt::mulOp(a, b);
      T r = (aVal * bVal) & eiMask<N>();
      CHECK(out == eiToBigInt<N>(r, N), "mul bits=%u", N); }
    // udivmod
    { if (bVal != 0) {
        auto [q, rm] = BigInt::udivmodOp(a, b);
        T qRef = aVal / bVal, rRef = aVal % bVal;
        do {
          testCount++;
          if (!(q == eiToBigInt<N>(qRef & eiMask<N>(), N))) {
            auto [q2, rm2] = BigInt::udivmodOp(a, b);
            failCount++;
            fprintf(stderr,
                    "FAIL (test #%lu): "
                    "udivmod quot bits=%u"
                    "\n",
                    (unsigned long)testCount, N);
            if (failCount >= 20) {
              fprintf(stderr, "Too many failures, aborting.\n");
              exit(1);
            }
          }
        } while (0);
        CHECK(rm == eiToBigInt<N>(rRef & eiMask<N>(), N), "udivmod rem bits=%u", N);
        BigInt qb = BigInt::mulOp<0>(q, b);
        BigInt sum; BigInt::addOp(sum, qb, rm);
        BigInt aTrunc; BigInt::resizeOp(aTrunc, a, N);
        CHECK(sum == aTrunc, "udivmod verify q*b+r!=a a=%s b=%s", a.toString().c_str(), b.toString().c_str());
      } }
    // sdivmod - use signed reference (only for N <= 64)
    { if constexpr (N <= 64) {
        if (bVal != 0) {
          T signBit = T(1) << (N - 1);
          if (!(bVal == signBit && aVal == signBit)) {
            auto aS = eiSignExt<N>(aVal);
            auto bS = eiSignExt<N>(bVal);
            using ST = typename std::make_signed<T>::type;
            ST aSigned = static_cast<ST>(aS);
            ST bSigned = static_cast<ST>(bS);
            if (bSigned != 0) {
              ST qRef = aSigned / bSigned, rRef = aSigned % bSigned;
              auto [q, rm] = BigInt::sdivmodOp(a, b);
              CHECK(q == eiToBigInt<N>(static_cast<T>(qRef) & eiMask<N>(), N), "sdivmod quot a=%s b=%s", a.toString().c_str(), b.toString().c_str());
              CHECK(rm == eiToBigInt<N>(static_cast<T>(rRef) & eiMask<N>(), N), "sdivmod rem a=%s b=%s", a.toString().c_str(), b.toString().c_str());
            }
          }
        }
      } }
  }

 // Forced extend patterns
  for (int pi = 0; pi < 4; pi++) {
    for (int i = 0; i < 200; i++) {
      BigInt a = BigInt::fromU64(0, N); a.randomize(rng);
      setLeadingExtend2S(a, (ExtPattern)pi, N);
      BigInt b = BigInt::fromU64(0, N); b.randomize(rng);
      setLeadingExtend2S(b, (ExtPattern)(pi ^ 1), N);
      T aVal = bigIntToEI<N>(a);
      T bVal = bigIntToEI<N>(b);
      { BigInt out; BigInt::addOp(out, a, b);
        T r = (aVal + bVal) & eiMask<N>();
        CHECK(out == eiToBigInt<N>(r, N), "add ext pat=%d bits=%u", pi, N); }
      { BigInt out; BigInt::subOp(out, a, b);
        T r = (aVal - bVal) & eiMask<N>();
        CHECK(out == eiToBigInt<N>(r, N), "sub ext pat=%d bits=%u", pi, N); }
      { BigInt out = BigInt::mulOp(a, b);
        T r = (aVal * bVal) & eiMask<N>();
        CHECK(out == eiToBigInt<N>(r, N), "mul ext pat=%d bits=%u", pi, N); }
    }
  }
}

static void testArithmeticExtInt(std::mt19937 &rng) {
  for (unsigned bits : ExtIntSizes) {
    fprintf(stderr, "  [_ExtInt width %u]\n", bits);
    switch (bits) {
#define X(N) case N: testArithmeticExtIntWidth<N>(rng); break
      X(1);X(2);X(3);X(7);X(8);X(11);X(13);X(15);X(16);X(19);X(23);X(27);X(31);X(32);X(33);X(37);X(41);X(47);X(53);
      X(63);X(64);X(65);X(71);X(79);X(96);X(97);X(103);X(107);X(127);X(128);X(129);X(131);X(137);X(149);
      X(191);X(192);X(193);X(199);X(251);X(255);X(256);X(257);X(263);X(311);X(384);X(511);X(512);X(513);
      X(521);X(640);X(767);X(768);X(769);X(896);X(1023);X(1024);X(1025);X(1031);
#undef X
      default: break;
    }
  }
}

template <unsigned N>
static void testShiftExtIntWidth(std::mt19937 &rng) {
  using T = typename ExtInt<N>::T;
  for (int i = 0; i < 500; i++) {
    T aVal = (T)rng() & eiMask<N>();
    if constexpr (N > 32) aVal |= (T(rng()) << 32) & eiMask<N>();
    if constexpr (N > 64) aVal |= (T(rng()) << 64) & eiMask<N>();
    BigInt a = eiToBigInt<N>(aVal, N);
    uint32_t amt = rng() % (N + 2);
    // Skip shift by >= N: _ExtInt shift is UB, BigInt correctly zeroes
    if (amt < N) {
      { BigInt out; BigInt::shlOp(out, a, amt);
        T r = (aVal << amt) & eiMask<N>();
        CHECK(out == eiToBigInt<N>(r, N), "shl bits=%u amt=%u", N, amt); }
      { BigInt out; BigInt::lshrOp(out, a, amt);
        T r = (aVal >> amt) & eiMask<N>();
        CHECK(out == eiToBigInt<N>(r, N), "lshr bits=%u amt=%u", N, amt); }
      { BigInt out; BigInt::ashrOp(out, a, amt);
        if (!(aVal & (T(1) << (N - 1)))) {
          T aS = eiSignExt<N>(aVal);
          T r = (aS >> amt) & eiMask<N>();
          CHECK(out == eiToBigInt<N>(r, N), "ashr bits=%u amt=%u positive", N, amt);
        }
      }
    }
  }
}

static void testShiftExtInt(std::mt19937 &rng) {
  for (unsigned bits : ExtIntSizes) {
    fprintf(stderr, "  [_ExtInt shift width %u]\n", bits);
    switch (bits) {
#define X(N) case N: testShiftExtIntWidth<N>(rng); break
      X(1);X(2);X(3);X(7);X(8);X(11);X(13);X(15);X(16);X(19);X(23);X(27);X(31);X(32);X(33);X(37);X(41);X(47);X(53);
      X(63);X(64);X(65);X(71);X(79);X(96);X(97);X(103);X(107);X(127);X(128);X(129);X(131);X(137);X(149);
      X(191);X(192);X(193);X(199);X(251);X(255);X(256);X(257);X(263);X(311);X(384);X(511);X(512);X(513);
      X(521);X(640);X(767);X(768);X(769);X(896);X(1023);X(1024);X(1025);X(1031);
#undef X
      default: break;
    }
  }
}

template <unsigned N>
static void testPowExtIntWidth(std::mt19937 &rng) {
  using T = typename ExtInt<N>::T;
  for (int i = 0; i < 200; i++) {
    T base = (rng() & eiMask<N>()) % 8;
    T expVal = rng() % 10;
    BigInt a = eiToBigInt<N>(base, N);
    BigInt e = eiToBigInt<N>(expVal, N);
    BigInt result = BigInt::upowOp(a, e);
    T ref = T(1);
    for (uint32_t j = 0; j < expVal; j++)
      ref = (ref * base) & eiMask<N>();
    CHECK(result == eiToBigInt<N>(ref, N), "upow bits=%u base=%u exp=%u", N, (unsigned)base, (unsigned)expVal);
  }
}

static void testPowExtInt(std::mt19937 &rng) {
  std::array<unsigned, 8> widths = { 8, 16, 32, 64, 65, 128, 129, 256 };
  for (unsigned bits : widths) {
    fprintf(stderr, "  [_ExtInt pow width %u]\n", bits);
    switch (bits) {
#define X(N) case N: testPowExtIntWidth<N>(rng); break
      X(8);X(16);X(32);X(64);X(65);X(128);X(129);X(256);
#undef X
      default: break;
    }
  }
}

static void testArithmetic4S(std::mt19937 &rng) {
  for (int iter = 0; iter < 5000; iter++) {
    uint32_t bits = rng() % 256 + 1;
    BigInt a = randomizeForceExtended4S(rng, bits, FourStateExt::FX);
    BigInt b = randomizeExtended(rng, bits, true);
    { BigInt out; BigInt::addOp4S(out, a, b);
      CHECK(out.getIs4S(), "addOp4S should produce 4S");
      CHECK(out.allBitsUndef(), "addOp4S with 4S input should be all x"); }
    { BigInt out; BigInt::subOp4S(out, a, b);
      CHECK(out.getIs4S(), "subOp4S should produce 4S");
      CHECK(out.allBitsUndef(), "subOp4S with 4S input should be all x"); }
    { BigInt out = BigInt::mulOp4S(a, b);
      CHECK(out.getIs4S(), "mulOp4S should produce 4S");
      CHECK(out.allBitsUndef(), "mulOp4S with 4S input should be all x"); }
    { auto [q, r] = BigInt::udivmodOp4S(a, b);
      CHECK(q.getIs4S(), "udivmodOp4S q should be 4S");
      CHECK(r.getIs4S(), "udivmodOp4S r should be 4S"); }
    // 2S+2S through 4S ops should stay 2S
    { BigInt a2 = BigInt::fromU64(0, bits); a2.randomize(rng);
      BigInt b2 = BigInt::fromU64(0, bits); b2.randomize(rng);
      BigInt out;
      BigInt::addOp4S(out, a2, b2);
      CHECK(!out.getIs4S(), "addOp4S with 2S inputs should produce 2S");
      BigInt::subOp4S(out, a2, b2);
      CHECK(!out.getIs4S(), "subOp4S with 2S inputs should produce 2S");
      BigInt mulOut = BigInt::mulOp4S(a2, b2);
      CHECK(!mulOut.getIs4S(), "mulOp4S with 2S inputs should produce 2S"); }
  }
}

static void testShift4SExtended(std::mt19937 &rng) {
  for (int iter = 0; iter < 8000; iter++) {
    uint32_t bits = rng() % 128 + 1;
    bool is4S = rng() % 2;
    BigInt src = randomizeExtended(rng, bits, is4S);
    uint32_t amt = rng() % (bits + 2);
    FourBitVec ref = FourBitVec::fromBigInt(src);
    { BigInt out; BigInt::shlOp4S(out, src, amt);
      CHECK(FourBitVec::fromBigInt(out) == ref.shl(amt), "shl4S mismatch bits=%u amt=%u is4S=%u", bits, amt, (unsigned)is4S); }
    { BigInt out; BigInt::lshrOp4S(out, src, amt);
      CHECK(FourBitVec::fromBigInt(out) == ref.lshr(amt), "lshr4S mismatch bits=%u amt=%u is4S=%u", bits, amt, (unsigned)is4S); }
    { BigInt out; BigInt::ashrOp4S(out, src, amt);
      CHECK(FourBitVec::fromBigInt(out) == ref.ashr(amt), "ashr4S mismatch bits=%u amt=%u is4S=%u", bits, amt, (unsigned)is4S); }
  }

  // Forced extend 4S
  for (int pi = 0; pi < 4; pi++) {
    FourStateExt pat = (FourStateExt)pi;
    for (int iter = 0; iter < 3000; iter++) {
      uint32_t bits = rng() % 128 + 1;
      BigInt src = randomizeForceExtended4S(rng, bits, pat);
      uint32_t amt = rng() % (bits + 1);
      FourBitVec ref = FourBitVec::fromBigInt(src);
      { BigInt out; BigInt::shlOp4S(out, src, amt);
        CHECK(FourBitVec::fromBigInt(out) == ref.shl(amt), "shl4S ext pat=%d bits=%u amt=%u", pi, bits, amt); }
      { BigInt out; BigInt::lshrOp4S(out, src, amt);
        CHECK(FourBitVec::fromBigInt(out) == ref.lshr(amt), "lshr4S ext pat=%d bits=%u amt=%u", pi, bits, amt); }
    }
  }
}

#endif // __clang__

// ---------------------------------------------------------------------------
// NEW: repeat, reductionXOR, leadingZeros, trailingZeros, countBits, pow
// ---------------------------------------------------------------------------

static void testRepeatExtended(std::mt19937 &rng) {
  for (int iter = 0; iter < 5000; iter++) {
    uint32_t bits = rng() % 64 + 1;
    bool is4S = rng() % 2;
    BigInt val = randomizeExtended(rng, bits, is4S);
    uint32_t count = rng() % 8 + 1;
    BigInt out;
    if (is4S) BigInt::repeatOp4S(out, val, count);
    else      BigInt::repeatOp(out, val, count);
    uint32_t expectedBits = bits * count;
    CHECK(out.getNumBits() == expectedBits, "repeat wrong bits valBits=%u count=%u got=%u", bits, count, out.getNumBits());
    CHECK(FourBitVec::fromBigInt(out) == FourBitVec::fromBigInt(val).repeat(count),
          "repeat mismatch valBits=%u count=%u is4S=%u", bits, count, (unsigned)is4S);
  }
}

static void testReductionXORExtended(std::mt19937 &rng) {
  for (int iter = 0; iter < 10000; iter++) {
    uint32_t bits = rng() % 256 + 1;
    BigInt val = randomizeExtended(rng, bits);
    FourBitVec ref = FourBitVec::fromBigInt(val);
    if (val.getIs4S()) {
      FourState result = BigInt::reductionXOROp4S(val);
      CHECK(result == FourState::SX, "reductionXOR4S with 4S should be x, got %u", (unsigned)result);
    } else {
      bool refResult = ref.reductionXor();
      bool actualResult = BigInt::reductionXOROp(val);
      CHECK(actualResult == refResult, "reductionXOR mismatch bits=%u", bits);
    }
  }
}

static void testLeadingZerosExtended(std::mt19937 &rng) {
  for (int iter = 0; iter < 10000; iter++) {
    uint32_t bits = rng() % 256 + 1;
    BigInt val = BigInt::fromU64(0, bits); val.randomize(rng);
    uint32_t lz = BigInt::leadingZeros(val);
    FourBitVec ref = FourBitVec::fromBigInt(val);
    uint32_t refLz = 0;
    for (uint32_t i = bits; i > 0; i--) {
      if (ref.bits[i - 1].val == FourState::S1) break;
      refLz++;
    }
    CHECK(lz == refLz, "leadingZeros mismatch bits=%u expected=%u got=%u", bits, refLz, lz);
  }
}

static void testTrailingZerosExtended(std::mt19937 &rng) {
  for (int iter = 0; iter < 10000; iter++) {
    uint32_t bits = rng() % 256 + 1;
    BigInt val = BigInt::fromU64(0, bits); val.randomize(rng);
    uint32_t tz = BigInt::trailingZeros(val);
    FourBitVec ref = FourBitVec::fromBigInt(val);
    uint32_t refTz = 0;
    for (uint32_t i = 0; i < bits; i++) {
      if (ref.bits[i].val == FourState::S1) break;
      refTz++;
    }
    CHECK(tz == refTz, "trailingZeros mismatch bits=%u expected=%u got=%u", bits, refTz, tz);
  }
}

static void testCountBitsExtended(std::mt19937 &rng) {
  for (int iter = 0; iter < 10000; iter++) {
    uint32_t bits = rng() % 256 + 1;
    bool is4S = rng() % 2;
    BigInt val = randomizeExtended(rng, bits, is4S);
    FourBitVec ref = FourBitVec::fromBigInt(val);

    if (!val.getIs4S()) {
      uint32_t ones = BigInt::countBitsExact(val, true);
      uint32_t zeros = BigInt::countBitsExact(val, false);
      uint32_t refOnes = ref.countSetBits();
      uint32_t refZeros = (uint32_t)ref.bits.size() - refOnes;
      CHECK(ones == refOnes, "countBitsExact(1) mismatch bits=%u expected=%u got=%u",
            bits, refOnes, ones);
      CHECK(zeros == refZeros, "countBitsExact(0) mismatch bits=%u expected=%u got=%u",
            bits, refZeros, zeros);

      uint32_t extOnes = BigInt::countBits4SExact(val, FourState::S1);
      uint32_t extZeros = BigInt::countBits4SExact(val, FourState::S0);
      CHECK(extOnes == refOnes, "countBits4SExact(S1) mismatch bits=%u expected=%u got=%u",
            bits, refOnes, extOnes);
      CHECK(extZeros == refZeros, "countBits4SExact(S0) mismatch bits=%u expected=%u got=%u",
            bits, refZeros, extZeros);
      CHECK(BigInt::countBits4SExact(val, FourState::SX) == 0,
            "countBits4SExact(SX) on 2S should be 0");
      CHECK(BigInt::countBits4SExact(val, FourState::SZ) == 0,
            "countBits4SExact(SZ) on 2S should be 0");
    } else {
      uint32_t refS0 = 0, refS1 = 0, refSZ = 0, refSX = 0;
      for (auto &b : ref.bits) {
        switch (b.val) {
        case FourState::S0: refS0++; break;
        case FourState::S1: refS1++; break;
        case FourState::SZ: refSZ++; break;
        case FourState::SX: refSX++; break;
        }
      }
      CHECK(BigInt::countBits4SExact(val, FourState::S0) == refS0,
            "countBits4SExact(S0) mismatch bits=%u expected=%u got=%u",
            bits, refS0, (unsigned)BigInt::countBits4SExact(val, FourState::S0));
      CHECK(BigInt::countBits4SExact(val, FourState::S1) == refS1,
            "countBits4SExact(S1) mismatch bits=%u expected=%u got=%u",
            bits, refS1, (unsigned)BigInt::countBits4SExact(val, FourState::S1));
      CHECK(BigInt::countBits4SExact(val, FourState::SZ) == refSZ,
            "countBits4SExact(SZ) mismatch bits=%u expected=%u got=%u",
            bits, refSZ, (unsigned)BigInt::countBits4SExact(val, FourState::SZ));
      CHECK(BigInt::countBits4SExact(val, FourState::SX) == refSX,
            "countBits4SExact(SX) mismatch bits=%u expected=%u got=%u",
            bits, refSX, (unsigned)BigInt::countBits4SExact(val, FourState::SX));
    }
  }
}

static void testPowExtended(std::mt19937 &rng) {
  // Test 1: upowOp with extended 2-state BigInts, small bases/exponents
  for (int iter = 0; iter < 5000; iter++) {
    uint32_t bits = rng() % 512 + 65; // 65-575 bits, always extended
    uint64_t baseVal = rng() % 8;
    uint64_t expVal = rng() % 10;

    BigInt base = BigInt::fromU64(baseVal, bits);
    BigInt exp = BigInt::fromU64(expVal, bits);

    BigInt result = BigInt::upowOp(base, exp);

    // Reference computation
    uint64_t ref = 1;
    for (uint32_t j = 0; j < expVal; j++)
      ref *= baseVal;
    ref &= (bits >= 64 ? ~0ULL : (1ULL << bits) - 1);

    auto resultVal = result.getLimitedVal();
    if (resultVal) {
      CHECK(*resultVal == ref, "upowOp extended 2S mismatch bits=%u base=%lu exp=%lu expected=%lu got=%lu",
            bits, (unsigned long)baseVal, (unsigned long)expVal,
            (unsigned long)ref, (unsigned long)*resultVal);
    } else {
      CHECK(false, "upowOp extended 2S result too large bits=%u base=%lu exp=%lu ref=%lu",
            bits, (unsigned long)baseVal, (unsigned long)expVal, (unsigned long)ref);
    }
  }

  // Test 2: upowOp4S with extended 2-state BigInts (delegates to upowOp)
  for (int iter = 0; iter < 5000; iter++) {
    uint32_t bits = rng() % 512 + 65;
    uint64_t baseVal = rng() % 8;
    uint64_t expVal = rng() % 10;

    BigInt base = BigInt::fromU64(baseVal, bits);
    BigInt exp = BigInt::fromU64(expVal, bits);

    BigInt result = BigInt::upowOp4S(base, exp);

    uint64_t ref = 1;
    for (uint32_t j = 0; j < expVal; j++)
      ref *= baseVal;
    ref &= (bits >= 64 ? ~0ULL : (1ULL << bits) - 1);

    CHECK(!result.getIs4S(), "upowOp4S with 2S inputs should produce 2S");
    auto resultVal = result.getLimitedVal();
    if (resultVal) {
      CHECK(*resultVal == ref, "upowOp4S extended 2S mismatch bits=%u base=%lu exp=%lu expected=%lu got=%lu",
            bits, (unsigned long)baseVal, (unsigned long)expVal,
            (unsigned long)ref, (unsigned long)*resultVal);
    }
  }

  // Test 3: upowOp4S with extended 4-state BigInts (should return undef)
  for (int iter = 0; iter < 5000; iter++) {
    uint32_t bits = rng() % 256 + 65;
    BigInt a;
    do { a = randomizeExtended(rng, bits, true); } while (!a.getIs4S());
    BigInt b;
    do { b = randomizeExtended(rng, bits, true); } while (!b.getIs4S());

    BigInt result = BigInt::upowOp4S(a, b);
    CHECK(result.getIs4S(), "upowOp4S with 4S inputs should produce 4S, iter=%d bits=%u", iter, bits);
    CHECK(result.allBitsUndef(), "upowOp4S with 4S inputs should be all x, iter=%d bits=%u", iter, bits);
  }

  // Test 4: upowOp4S with one 4S, one 2S input (should return undef)
  for (int iter = 0; iter < 3000; iter++) {
    uint32_t bits = rng() % 256 + 65;
    bool base4S = rng() % 2;
    BigInt a;
    do { a = randomizeExtended(rng, bits, base4S); }
      while (base4S && !a.getIs4S());
    BigInt b;
    do { b = randomizeExtended(rng, bits, !base4S); }
      while (!base4S && !b.getIs4S());

    // Ensure at least one is 4S for meaningful test
    if (!a.getIs4S() && !b.getIs4S()) continue;

    BigInt result = BigInt::upowOp4S(a, b);
    CHECK(result.getIs4S(), "upowOp4S with mixed 4S/2S should produce 4S");
    CHECK(result.allBitsUndef(), "upowOp4S with mixed 4S/2S should be all x");
  }

  // Test 5: Edge cases - exponent 0, base 0, base 1
  for (unsigned bits : {65u, 129u, 257u, 513u}) {
    // 0^0 = 1
    { BigInt base = BigInt::fromU64(0, bits);
      BigInt exp = BigInt::fromU64(0, bits);
      BigInt result = BigInt::upowOp(base, exp);
      auto v = result.getLimitedVal();
      CHECK(v && *v == 1, "0^0 should be 1, got %s", v ? "non-1" : "null"); }

    // 0^n = 0 (n > 0)
    { BigInt base = BigInt::fromU64(0, bits);
      BigInt exp = BigInt::fromU64(5, bits);
      BigInt result = BigInt::upowOp(base, exp);
      auto v = result.getLimitedVal();
      CHECK(v && *v == 0, "0^5 should be 0, got %s", v ? "non-0" : "null"); }

    // 1^n = 1
    { BigInt base = BigInt::fromU64(1, bits);
      BigInt exp = BigInt::fromU64(9, bits);
      BigInt result = BigInt::upowOp(base, exp);
      auto v = result.getLimitedVal();
      CHECK(v && *v == 1, "1^9 should be 1, got %s", v ? "non-1" : "null"); }

    // n^0 = 1
    { BigInt base = BigInt::fromU64(7, bits);
      BigInt exp = BigInt::fromU64(0, bits);
      BigInt result = BigInt::upowOp(base, exp);
      auto v = result.getLimitedVal();
      CHECK(v && *v == 1, "7^0 should be 1, got %s", v ? "non-1" : "null"); }
  }

  // Test 6: upowOp with large bases that span multiple words
  for (int iter = 0; iter < 2000; iter++) {
    uint32_t bits = rng() % 256 + 65;
    // Create base with actual multi-word value
    BigInt base = BigInt::fromU64(0, bits);
    base.randomize(rng);
    // Cap to small value for reference check
    auto baseValOpt = base.getLimitedVal();
    if (!baseValOpt || *baseValOpt > 5) continue;
    uint64_t baseVal = *baseValOpt;

    uint64_t expVal = rng() % 6 + 1;
    BigInt exp = BigInt::fromU64(expVal, bits);

    BigInt result = BigInt::upowOp(base, exp);

    uint64_t ref = 1;
    for (uint32_t j = 0; j < expVal; j++)
      ref *= baseVal;

    auto resultVal = result.getLimitedVal();
    if (resultVal) {
      CHECK(*resultVal == ref, "upowOp multi-word base mismatch bits=%u base=%lu exp=%lu expected=%lu got=%lu",
            bits, (unsigned long)baseVal, (unsigned long)expVal,
            (unsigned long)ref, (unsigned long)*resultVal);
    }
  }
}

// ---------------------------------------------------------------------------
// NEW: Comparison ops
// ---------------------------------------------------------------------------

static void testIcmpExtended(std::mt19937 &rng) {
  for (int iter = 0; iter < 10000; iter++) {
    uint32_t bits = rng() % 128 + 1;
    BigInt a = BigInt::fromU64(0, bits); a.randomize(rng);
    BigInt b = BigInt::fromU64(0, bits); b.randomize(rng);
    CHECK(BigInt::icmpOp(a, a, BigInt::ICMP_EQ), "icmp EQ self");
    CHECK(BigInt::icmpOp(a, a, BigInt::ICMP_NE) == false, "icmp NE self");
    if (a != b) {
      CHECK(BigInt::icmpOp(a, b, BigInt::ICMP_EQ) == false, "icmp EQ distinct");
      CHECK(BigInt::icmpOp(a, b, BigInt::ICMP_NE), "icmp NE distinct");
    }
    if (bits <= 64) {
      uint64_t aVal = 0, bVal = 0;
      for (uint32_t i = 0; i < bits; i++) {
        if (a.getBit(i) == FourState::S1) aVal |= (1ULL << i);
        if (b.getBit(i) == FourState::S1) bVal |= (1ULL << i);
      }
      CHECK(BigInt::icmpOp(a, b, BigInt::ICMP_ULT) == (aVal < bVal), "icmp ULT mismatch bits=%u", bits);
      CHECK(BigInt::icmpOp(a, b, BigInt::ICMP_ULE) == (aVal <= bVal), "icmp ULE mismatch bits=%u", bits);
      CHECK(BigInt::icmpOp(a, b, BigInt::ICMP_UGT) == (aVal > bVal), "icmp UGT mismatch bits=%u", bits);
      CHECK(BigInt::icmpOp(a, b, BigInt::ICMP_UGE) == (aVal >= bVal), "icmp UGE mismatch bits=%u", bits);
    }
    if (bits <= 63) {
      int64_t aVal = 0, bVal = 0;
      for (uint32_t i = 0; i < bits; i++) {
        if (a.getBit(i) == FourState::S1) aVal |= (1LL << i);
        if (b.getBit(i) == FourState::S1) bVal |= (1LL << i);
      }
      if (aVal & (1LL << (bits - 1))) aVal |= (~0LL << bits);
      if (bVal & (1LL << (bits - 1))) bVal |= (~0LL << bits);
      CHECK(BigInt::icmpOp(a, b, BigInt::ICMP_SLT) == (aVal < bVal), "icmp SLT mismatch bits=%u", bits);
      CHECK(BigInt::icmpOp(a, b, BigInt::ICMP_SLE) == (aVal <= bVal), "icmp SLE mismatch bits=%u", bits);
      CHECK(BigInt::icmpOp(a, b, BigInt::ICMP_SGT) == (aVal > bVal), "icmp SGT mismatch bits=%u", bits);
      CHECK(BigInt::icmpOp(a, b, BigInt::ICMP_SGE) == (aVal >= bVal), "icmp SGE mismatch bits=%u", bits);
    }
  }
}

static void testIcmp4SExtended(std::mt19937 &rng) {
  for (int iter = 0; iter < 5000; iter++) {
    uint32_t bits = rng() % 128 + 1;
    bool is4S = rng() % 2;
    BigInt a = randomizeExtended(rng, bits, is4S);
    BigInt b = randomizeExtended(rng, bits, is4S);
    { FourState result = BigInt::icmpOp4S(a, b, BigInt::ICMP_CEQ);
      FourState resultNE = BigInt::icmpOp4S(a, b, BigInt::ICMP_CNE);
      if (!a.getIs4S() && !b.getIs4S()) {
        CHECK(result.val <= FourState::S1, "icmp CEQ 2S should not be x");
        CHECK(resultNE.val <= FourState::S1, "icmp CNE 2S should not be x");
        CHECK(result.val != resultNE.val, "CEQ and CNE should be opposite for 2S");
      } }
    { FourState result = BigInt::icmpOp4S(a, b, BigInt::ICMP_CZEQ);
      CHECK(result.val == FourState::S0 || result.val == FourState::S1, "CZEQ should be S0 or S1"); }
    { FourState result = BigInt::icmpOp4S(a, b, BigInt::ICMP_CXEQ);
      CHECK(result.val == FourState::S0 || result.val == FourState::S1, "CXEQ should be S0 or S1"); }
    // ULT with 4S should be x, but only when actually 4S (conv4To2StateIfPossible may collapse)
    if (a.getIs4S() || b.getIs4S()) {
      FourState result = BigInt::icmpOp4S(a, b, BigInt::ICMP_ULT);
      CHECK(result == FourState::SX, "icmp ULT 4S should be x, got %u", (unsigned)result);
    }
  }
  { auto x32 = PatBigInt::undef(32);
    CHECK(BigInt::icmpOp4S(x32, x32, BigInt::ICMP_WEQ).val == FourState::S1, "x == x wildcard should be true");
    auto z32 = PatBigInt::undef2(32);
    CHECK(BigInt::icmpOp4S(z32, z32, BigInt::ICMP_CZEQ).val == FourState::S1, "z == z casez should be true"); }
}

// ---------------------------------------------------------------------------
// NEW: 4S-specific ops
// ---------------------------------------------------------------------------

static void testBitsExactEqual4S(std::mt19937 &rng) {
  for (int iter = 0; iter < 3000; iter++) {
    uint32_t bits = rng() % 64 + 1;
    BigInt a = randomizeExtended(rng, bits, true);
    BigInt b = randomizeExtended(rng, bits, true);
    BigInt result = BigInt::bitsExactEqual4S(a, b);
    FourBitVec refA = FourBitVec::fromBigInt(a);
    FourBitVec refB = FourBitVec::fromBigInt(b);
    for (uint32_t i = 0; i < bits; i++) {
      bool refSet = (refA.bits[i].val == refB.bits[i].val);
      bool actualSet = (result.getBit(i) == FourState::S1);
      CHECK(actualSet == refSet, "bitsExactEqual4S bit %u mismatch bits=%u", i, bits);
    }
  }
}

static void testUnknownMaskExtended(std::mt19937 &rng) {
  for (int iter = 0; iter < 3000; iter++) {
    uint32_t bits = rng() % 64 + 1;
    BigInt val = randomizeExtended(rng, bits, true);
    if (!val.getIs4S()) continue;
    BigInt mask = BigInt::unknownMask(val);
    FourBitVec ref = FourBitVec::fromBigInt(val);
    for (uint32_t i = 0; i < bits; i++) {
      bool refUnk = ref.bits[i].val > FourState::S1;
      bool maskBit = (mask.getBit(i) == FourState::S1);
      CHECK(maskBit == refUnk, "unknownMask bit %u mismatch bits=%u expected=%u got=%u", i, bits, refUnk, maskBit);
    }
  }
}

static void testConv4To2StateRoundTrip(std::mt19937 &rng) {
  for (int iter = 0; iter < 5000; iter++) {
    uint32_t bits = rng() % 128 + 1;
    BigInt orig2S = BigInt::fromU64(0, bits); orig2S.randomize(rng);
    BigInt as4S = orig2S; as4S.conv2To4State();
    BigInt back2S = as4S; back2S.conv4To2State();
    // Bitwise check instead of == (which compares internal storage)
    bool match = true;
    for (uint32_t i = 0; i < bits && match; i++)
      if (back2S.getBit(i) != orig2S.getBit(i)) match = false;
    CHECK(match, "conv2To4State -> conv4To2State roundtrip failed bits=%u", bits);
    BigInt allKnown = BigInt::fromU64(0, bits); allKnown.randomize4S(rng);
    if (allKnown.getIs4S()) {
      for (uint32_t i = 0; i < bits; i++) {
        FourState bit = allKnown.getBit(i);
        allKnown.setBit(i, bit.val <= 1 ? bit : FourState::S0);
      }
      CHECK(!allKnown.getIs4S(), "conv4To2StateIfPossible should collapse all-known 4S bits=%u", bits);
    }
  }
}

// ---------------------------------------------------------------------------
// Bug reproduction tests
// ---------------------------------------------------------------------------

static void testBug() {
  auto upper = "34'h2_02003110"_bd;
  auto falseRes = "103'h00_4006221f_ffffffff_ffffffff?00_0000000f_ffffffff_ffff8000"_bd;
  BigInt lower; BigInt::resizeOp4S(lower, falseRes, 69);
  BigInt out; BigInt::concatOp4S(out, upper, lower);
  BigInt check; BigInt::rangeSelectOp4S(check, out, 69, 34);
  CHECK(upper == check, "concat upper bits failure");
}

static void testBug2() {
  auto val = "32'h00000063?ffffff80"_bd;
  CHECK(val.toString(16, false) == "32'h00000063?ffffff80", "parse print round trip failed");
}

static void testBug3() {
  // Pre-existing resize round-trip bug with 4S BigInts
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

int main() {
  std::mt19937 rng(42);

  fprintf(stderr, "=== Bug reproductions ===\n");
  testBug(); testBug2(); testBug3();

  fprintf(stderr, "=== Insert boundary 4S ===\n");
  testInsertBoundary(rng);

  fprintf(stderr, "=== rangeSelect bitwise ===\n");
  testRangeSelectBitwise(rng);
  fprintf(stderr, "=== concat bitwise ===\n");
  testConcatBitwise(rng);
  fprintf(stderr, "=== shl bitwise ===\n");
  testShlBitwise(rng);
  fprintf(stderr, "=== lshr bitwise ===\n");
  testLshrBitwise(rng);
  fprintf(stderr, "=== ashr sign-extend ===\n");
  testAshrSignExtend(rng);
  fprintf(stderr, "=== ashr vs lshr (sign=0) ===\n");
  testAshrVsLshr(rng);
  fprintf(stderr, "=== ashr vs lshr (sign=1) ===\n");
  testAshrVsLshrSigned(rng);
  fprintf(stderr, "=== insert-extract roundtrip ===\n");
  testInsertExtractRoundtrip(rng);
  fprintf(stderr, "=== concat-extract roundtrip ===\n");
  testConcatExtractRoundtrip(rng);
  fprintf(stderr, "=== shift roundtrip ===\n");
  testShiftRoundtrip(rng);
  fprintf(stderr, "=== extract-reconstruct ===\n");
  testExtractReconstruct(rng);
  fprintf(stderr, "=== concat associativity ===\n");
  testConcatAssoc(rng);
  fprintf(stderr, "=== insert vs concat ===\n");
  testInsertVsConcat(rng);
  fprintf(stderr, "=== shl vs concat ===\n");
  testShiftVsConcat(rng);
  fprintf(stderr, "=== lshr vs rangeSelect ===\n");
  testLshrVsRangeSelect(rng);
  fprintf(stderr, "=== multi-concat extract ===\n");
  testMultiConcatExtract(rng);
  fprintf(stderr, "=== shift by zero ===\n");
  testShiftZero(rng);
  fprintf(stderr, "=== shift overshoot ===\n");
  testShiftOvershoot(rng);
  fprintf(stderr, "=== edge sizes ===\n");
  testEdgeSizes(rng);
  fprintf(stderr, "=== word-boundary crossing ===\n");
  testWordBoundaryCrossing(rng);
  fprintf(stderr, "=== concat zero bits ===\n");
  testConcatZeroBits(rng);
  fprintf(stderr, "=== rangeSelect zero len ===\n");
  testRangeSelectZeroLen(rng);
  fprintf(stderr, "=== truncate vs rangeSelect(0) ===\n");
  testTruncateVsRangeSelect(rng);
  fprintf(stderr, "=== truncate vs rangeSelect(0) extended+4S ===\n");
  testTruncateVsRangeSelectExtended(rng);
  fprintf(stderr, "=== rangeSelect nonZero extended+4S ===\n");
  testRangeSelectNonZeroExtended(rng);
  fprintf(stderr, "=== insert nonZero extended+4S ===\n");
  testInsertNonZeroExtended(rng);
  fprintf(stderr, "=== concat extended+4S ===\n");
  testConcatExtended(rng);
  fprintf(stderr, "=== extract-insert roundtrip ===\n");
  testExtractInsertRoundtrip(rng);
  fprintf(stderr, "=== multi-insert range check ===\n");
  testMultiInsertRangeCheck(rng);

  // New tests
  fprintf(stderr, "=== bitwise ops extended ===\n");
  testBitwiseExtended(rng);
  fprintf(stderr, "=== repeat extended ===\n");
  testRepeatExtended(rng);
  fprintf(stderr, "=== reductionXOR extended ===\n");
  testReductionXORExtended(rng);
  fprintf(stderr, "=== leadingZeros extended ===\n");
  testLeadingZerosExtended(rng);
  fprintf(stderr, "=== trailingZeros extended ===\n");
  testTrailingZerosExtended(rng);
  fprintf(stderr, "=== countBits extended ===\n");
  testCountBitsExtended(rng);
  fprintf(stderr, "=== pow extended ===\n");
  testPowExtended(rng);
  fprintf(stderr, "=== icmp extended ===\n");
  testIcmpExtended(rng);
  fprintf(stderr, "=== icmp 4S extended ===\n");
  testIcmp4SExtended(rng);
  fprintf(stderr, "=== bitsExactEqual4S ===\n");
  testBitsExactEqual4S(rng);
  fprintf(stderr, "=== unknownMask extended ===\n");
  testUnknownMaskExtended(rng);
  fprintf(stderr, "=== conv4To2State roundtrip ===\n");
  testConv4To2StateRoundTrip(rng);

#ifdef __clang__
  fprintf(stderr, "=== arithmetic _ExtInt ===\n");
  testArithmeticExtInt(rng);
  fprintf(stderr, "=== shift _ExtInt ===\n");
  testShiftExtInt(rng);
  fprintf(stderr, "=== pow _ExtInt ===\n");
  testPowExtInt(rng);
  fprintf(stderr, "=== arithmetic 4S ===\n");
  testArithmetic4S(rng);
  fprintf(stderr, "=== shift 4S extended ===\n");
  testShift4SExtended(rng);
#else
  fprintf(stderr, "=== _ExtInt tests skipped (not clang) ===\n");
#endif

  fprintf(stderr, "\n=== Results: %lu tests, %lu failures ===\n",
          (unsigned long)testCount, (unsigned long)failCount);
  return failCount ? 1 : 0;
}
