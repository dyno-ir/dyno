#pragma once

#include "support/Bits.h"
#include "support/Utility.h"
#include <dyno/IDs.h>
#include <dyno/NewDeleteObjStore.h>
#include <dyno/Obj.h>
#include <span>

namespace dyno {

class ConstantRef;
class ConstantBuilder;

class alignas(uint64_t) Constant : public TrailingObjArr<Constant, uint64_t> {
  friend class ConstantRef;
  // num bits is detached from actual storage words bc we can sign/zext.
  unsigned numBits;
  // msb is sext/zext
  unsigned numWords;

public:
  Constant(DynObjRef ref, unsigned numWords, unsigned numBits, bool sext)
      : numBits(numBits), numWords((numWords & ~bit_mask_msb<unsigned>()) |
                                   sext << (bit_mask_sz<unsigned> - 1)) {}

  Constant(const Constant &) = delete;
  Constant(Constant &&) = delete;
  Constant &operator=(const Constant &) = delete;
  Constant &operator=(Constant &&) = delete;

private:
  size_t getNumTrailing() { return numWords & ~bit_mask_msb<unsigned>(); }
};
static_assert(TrailingObj<Constant>);

using ConstantStore = NewDeleteObjStore<Constant>;

class ConstantRef : public FatDynObjRef<Constant> {
  using IsInline = DynObjRef::CustomField<1, 0>;
  using IsZext = DynObjRef::CustomField<1, 1>;
  using IsSext = DynObjRef::CustomField<1, 2>;
  using NBits = DynObjRef::CustomField<13, 3>;

public:
  using FatDynObjRef<Constant>::FatDynObjRef;

  explicit ConstantRef(FatDynObjRef<Constant> ref)
      : FatDynObjRef<Constant>(ref) {}

  ConstantRef(unsigned n, uint32_t val, bool sext = false)
      : FatDynObjRef<Constant>(DynObjRef::ofTy<Constant>()) {
    customField<IsInline>() = true;
    customField<IsZext>() = !sext;
    customField<IsSext>() = sext;
    customField<NBits>() = n;
    obj = ObjID{val};
  }

  std::span<uint64_t> words() {
    return std::span{ptr->trailing(), ptr->numWords};
  }
  bool getIsSext() { return ptr->numWords & bit_mask_msb<unsigned>(); };
  unsigned getNumWords() { return ptr->numWords & ~bit_mask_msb<unsigned>(); };

  uint64_t valTrunc64() {
    if (customField<IsInline>()) {
      // todo: can also assume val is normalized.
      if (customField<IsZext>())
        return obj & bit_mask_nbits<uint32_t>(customField<NBits>());
      else if (customField<IsSext>())
        return (int64_t(obj) << (64 - customField<NBits>())) >>
               (64 - customField<NBits>());
      else
        dyno_unreachable("neither sign nor zext");
    } else {
      // assuming the stored value is normalized
      return words()[0];
    }
  }
};

class ConstantBuilder {
  ConstantStore &store;

public:
  ConstantBuilder(ConstantStore &store) : store(store) {}
  ConstantRef build(unsigned bits, uint64_t value64, bool sext = 0) {
    if (bits <= 32) {
      return ConstantRef{bits, static_cast<uint32_t>(value64), sext};
    } else {
      uint words = (bits + 63) / 64;
      auto constRef = store.create(words, bits, sext).as<ConstantRef>();
      constRef.words()[0] = value64;
      return constRef;
    }
  }
};

template <> struct ObjTraits<Constant> {
  static constexpr DialectID dialect{DIALECT_CORE};
  static constexpr TyID ty{CORE_CONSTANT};
  using FatRefT = ConstantRef;
};
} // namespace dyno
