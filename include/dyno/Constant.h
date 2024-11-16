#pragma once

#include <dyno/IDs.h>
#include <dyno/NewDeleteObjStore.h>
#include <dyno/Obj.h>

namespace dyno {

class ConstantRef;
class ConstantBuilder;

class alignas(uint64_t) Constant : public TrailingObjArr<Constant, uint64_t> {
  friend class ConstantRef;

  unsigned numWords;

public:
  Constant(DynObjRef, unsigned numWords) : numWords(numWords) {}

  Constant(const Constant &) = delete;
  Constant(Constant &&) = delete;
  Constant &operator=(const Constant &) = delete;
  Constant &operator=(Constant &&) = delete;

private:
  size_t getNumTrailing() { return numWords; }
};
static_assert(TrailingObj<Constant>);

using ConstantStore = NewDeleteObjStore<Constant>;

class ConstantRef : public FatDynObjRef<Constant> {
  using IsInline = DynObjRef::CustomField<1, 0>;
  using IsZext = DynObjRef::CustomField<1, 1>;
  using IsSext = DynObjRef::CustomField<1, 2>;
  using NBits = DynObjRef::CustomField<13, 3>;

public:
  explicit ConstantRef(FatDynObjRef<Constant> ref) : FatDynObjRef<Constant>(ref) {}

  ConstantRef(unsigned n, uint32_t val) : FatDynObjRef<Constant>(DynObjRef::ofTy<Constant>()) {
    customField<IsInline>() = true;
    customField<IsZext>() = true;
    customField<NBits>() = n;
    obj = ObjID{val};
  }
};

class ConstantBuilder {
  ConstantStore &store;
  ConstantRef ref;

public:
  ConstantBuilder(ConstantStore &store, ConstantRef ref)
      : store(store), ref(ref) {}
};

template <> struct ObjTraits<Constant> {
  static constexpr DialectID dialect{DIALECT_CORE};
  static constexpr TyID ty{CORE_CONSTANT};
  using FatRefT = ConstantRef;
};
} // namespace dyno
