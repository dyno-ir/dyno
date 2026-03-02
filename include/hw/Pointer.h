#pragma once

#include "dyno/Constant.h"
#include "dyno/Instr.h"
#include "dyno/RefUnion.h"
#include "hw/DefUseMixin.h"
#include "hw/IDs.h"
namespace dyno {
class Pointer {
public:
  InstrDefUse defUse;

  Pointer(DynObjRef) {}
  Pointer(DynObjRef, FatObjRef<Pointer>) {}
};
class PointerRef : public FatObjRef<Pointer>,
                   public InstrDefUseMixin<PointerRef> {
  using FatObjRef::FatObjRef;
  PointerRef(FatObjRef ref) : FatObjRef(ref) {}
};

template <> struct ObjTraits<Pointer> {
  using FatRefT = PointerRef;
  static constexpr DialectType ty{HW_POINTER};
};

using HWAddress = FatRefUnion<PointerRef, ConstantRef>;

}; // namespace dyno
