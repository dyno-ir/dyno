#pragma once

#include "dsl/IDs.h"
#include "dyno/CFG.h"
#include "dyno/Constant.h"
#include "dyno/RefUnion.h"
#include "op/StringObj.h"
#include <dyno/Instr.h>
#include <dyno/InstrMixin.h>
#include <dyno/Obj.h>

namespace dyno {

class DSLVal {
public:
  InstrDefUse defUse;
  DSLVal(DynObjRef) {}
  DSLVal(DynObjRef, FatObjRef<DSLVal>) {}
};

class DSLNull {};

class DSLNullRef : public FatObjRef<DSLNull> {
public:
  constexpr DSLNullRef()
      : FatObjRef(ObjID(0), static_cast<DSLNull *>(nullptr)) {}
};

inline constexpr DSLNullRef dsl_nullref = DSLNullRef();

class DSLValRef : public FatObjRef<DSLVal>, public InstrDefUseMixin<DSLValRef> {
public:
  using FatObjRef::FatObjRef;

  auto getDefI() { return getDef().instr(); }
};

template <> struct ObjTraits<DSLVal> {
  static constexpr DialectType ty{DSL_VAL};
  using FatRefT = DSLValRef;
};

template <> struct ObjTraits<DSLNull> {
  static constexpr DialectType ty{DSL_NULL};
  using FatRefT = DSLNullRef;
};

class DSLValue : public FatRefUnion<DSLValRef, DSLNullRef, ConstantRef,
                                    BlockRef, StringObjRef> {
public:
  using FatRefUnion::FatRefUnion;
  using FatRefUnion::operator=;
};

} // namespace dyno
