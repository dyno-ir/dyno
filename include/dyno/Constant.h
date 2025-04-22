#pragma once

#include "support/Bits.h"
#include "support/SmallVec.h"
#include "support/Utility.h"
#include <dyno/IDs.h>
#include <dyno/NewDeleteObjStore.h>
#include <dyno/Obj.h>
#include <span>
#include <unordered_map>

namespace dyno {

class ConstantRef;
class ConstantBuilder;

constexpr size_t BigIntExtendBits = 2;

class BigInt {
  friend class Constant;
  friend class ConstantBuilder;
  friend class ConstantStore;
  SmallVec<uint32_t, 4> words;
  uint32_t numBits;
  uint8_t extend;

  unsigned getNumWords() const { return words.size(); }
  unsigned getExtend() const { return extend; }
  std::span<uint32_t> getWords() { return words; }
  const std::span<uint32_t> getWords() const { return words; }

public:
  BigInt() : words(), numBits(0), extend(0) {}
  BigInt(uint64_t val) { setPruned(val); }
  BigInt(uint64_t val, unsigned bits) { set(val, bits); }

  template <typename T0, typename T1>
  static void addOp(BigInt &out, const T0 &lhs, const T1 &rhs) {
    return linearOp<[](unsigned lhs, unsigned rhs, unsigned cin,
                       unsigned *cout) {
      return __builtin_addc(lhs, rhs, cin, cout);
    }>(out, lhs, rhs);
  }

  void set(uint64_t val, unsigned bits) {
    if (bits <= 32)
      return set((uint32_t)val, bits);
    words.resize(2);
    words[0] = val;
    words[1] = val >> 32;
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
    words.resize(bits <= 32 ? 1 : 2);
    words[0] = val;
    if (words.size() == 2) {
      words[1] = val >> 32;
      normalize();
    }
  }
  uint32_t getNumBits() const { return numBits; }

private:
  BigInt(uint32_t numBits, uint32_t numWords, uint32_t extend)
      : words(numWords), numBits(numBits), extend(extend) {}

  static constexpr unsigned repeat_extend(uint8_t num) {
    unsigned fact = BigIntExtendBits;
    num &= bit_mask_ones<unsigned>(BigIntExtendBits);
    while (fact != bit_mask_sz<unsigned>) {
      num |= (num << fact);
      fact <<= 1;
    }
    return num;
  }

  void prune() {
    uint32_t extendMask = repeat_extend(extend);
    while (words.size() != 1 && words.back() == extendMask)
      words.pop_back();
  }

  void normalize() {
    if (getNumWords() * bit_mask_sz<uint32_t> < numBits)
      prune();

    // try to find new extend
    if (getNumWords() * bit_mask_sz<uint32_t> >= numBits) {
      extend = words.back() & bit_mask_ms_nbits<uint32_t>(BigIntExtendBits);
      prune();
    }
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
        lhsVal = repeat_extend(lhs.getExtend());
      else
        lhsVal = lhs.getWords()[i];

      if (i >= rhs.getNumWords())
        rhsVal = repeat_extend(rhs.getExtend());
      else
        rhsVal = rhs.getWords()[i];

      out.words[i] = OpFunc(lhsVal, rhsVal, carry, &carry);
    }

    out.extend = extend;
    out.numBits = maxNumBits;
    out.normalize();
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

class ConstantRef : public FatDynObjRef<Constant> {
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
    customField<ExtPattern>() = 0;
    customField<NBits>() = n;
    obj = ObjID{val};
  }

  std::span<const uint32_t> getWords() const {
    return isInline()
               ? std::span<const uint32_t>{&obj.num, 1}
               : std::span<const uint32_t>{ptr->trailing(), ptr->numWords};
  }
  // bool getIsSext() {
  //   return isInline() ? customField<IsSext>()
  //                     : (ptr->numWords & bit_mask_msb<unsigned>());
  // };
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

  // friend std::ostream &operator<<(std::ostream &os, const ConstantRef &self) {}
  // public:
  //   friend std::string to_string(ConstantRef const &self) {
  //   }
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

  ConstantBuilder &add(const uint64_t val) {
    BigInt::addOp(cur, cur, BigInt{val});
    return *this;
  }
  ConstantBuilder &add(ConstantRef rhs) {
    BigInt::addOp(cur, cur, rhs);
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
