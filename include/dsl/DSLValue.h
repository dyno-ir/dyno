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

class DSLTy {
public:
  InstrDefUse defUse;
  DSLTy(DynObjRef) {}
  DSLTy(DynObjRef, FatObjRef<DSLTy>) {}
};

class DSLTyRef : public FatObjRef<DSLTy>, public InstrDefUseMixin<DSLTyRef> {
public:
  using FatObjRef::FatObjRef;
  DSLTyRef(FatObjRef<DSLTy> ref) : FatObjRef(ref) {}

  auto getDefI() { return getDef().instr(); }
};

template <> struct ObjTraits<DSLTy> {
  static constexpr DialectType ty{DSL_TY};
  using FatRefT = DSLTyRef;
};

class DSLIntRef : public FatDynObjRef<> {
public:
  using FatDynObjRef::FatDynObjRef;

  explicit constexpr DSLIntRef(uint32_t numBits, bool isSigned = true)
      : FatDynObjRef(DynObjRef(DIALECT_DSL,
                               isSigned ? DSL_INT.type : DSL_UINT.type,
                               ObjID(numBits), 0),
                     nullptr) {}

  static bool is_impl(DynObjRef ref) {
    return ref.getType() == DSL_INT || ref.getType() == DSL_UINT;
  }

  uint32_t getNumBits() const { return getObjID().num; }
  bool isSigned() const { return getType() == DSL_INT; }
  bool isUnsigned() const { return getType() == DSL_UINT; }
};

class DSLTyVar {};

class DSLTyVarRef : public FatObjRef<DSLTyVar> {
public:
  using FatObjRef::FatObjRef;
  explicit constexpr DSLTyVarRef(ObjID id)
      : FatObjRef(id, static_cast<DSLTyVar *>(nullptr)) {}
};

template <> struct ObjTraits<DSLTyVar> {
  static constexpr DialectType ty{DSL_TYVAR};
  using FatRefT = DSLTyVarRef;
};

class DSLUnit {};

class DSLUnitRef : public FatObjRef<DSLUnit> {
public:
  constexpr DSLUnitRef()
      : FatObjRef(ObjID(0), static_cast<DSLUnit *>(nullptr)) {}
};

inline constexpr DSLUnitRef dsl_unit;

template <> struct ObjTraits<DSLUnit> {
  static constexpr DialectType ty{DSL_UNIT};
  using FatRefT = DSLUnitRef;
};

class DSLBool {};

class DSLBoolRef : public FatObjRef<DSLBool> {
public:
  constexpr DSLBoolRef()
      : FatObjRef(ObjID(0), static_cast<DSLBool *>(nullptr)) {}
};

inline constexpr DSLBoolRef dsl_bool;

template <> struct ObjTraits<DSLBool> {
  static constexpr DialectType ty{DSL_BOOL};
  using FatRefT = DSLBoolRef;
};

class DSLString {};

class DSLStringRef : public FatObjRef<DSLString> {
public:
  constexpr DSLStringRef()
      : FatObjRef(ObjID(0), static_cast<DSLString *>(nullptr)) {}
};

inline constexpr DSLStringRef dsl_string;

template <> struct ObjTraits<DSLString> {
  static constexpr DialectType ty{DSL_STRING};
  using FatRefT = DSLStringRef;
};

class DSLTypename {};

class DSLTypenameRef : public FatObjRef<DSLTypename> {
public:
  constexpr DSLTypenameRef()
      : FatObjRef(ObjID(0), static_cast<DSLTypename *>(nullptr)) {}
};

inline constexpr DSLTypenameRef dsl_typename;

template <> struct ObjTraits<DSLTypename> {
  static constexpr DialectType ty{DSL_TYPENAME};
  using FatRefT = DSLTypenameRef;
};

class DSLTypeRef
    : public FatRefUnion<DSLTyRef, DSLTyVarRef, DSLIntRef, DSLUnitRef,
                         DSLBoolRef, DSLStringRef, DSLTypenameRef> {
public:
  using FatRefUnion::FatRefUnion;
  using FatRefUnion::operator=;

  InstrRef getTypeI() const {
    if (auto ref = dyn_as<DSLTyRef>())
      return ref.getDefI();
    return nullref;
  }
};

class DSLVal {
public:
  InstrDefUse defUse;
  DSLTypeRef ty = nullref;
  DSLVal(DynObjRef) {}
  DSLVal(DynObjRef, FatObjRef<DSLVal>) {}

  DSLTypeRef getTy() { return ty; }
};

class DSLValRef : public FatObjRef<DSLVal>, public InstrDefUseMixin<DSLValRef> {
public:
  using FatObjRef::FatObjRef;

  auto getDefI() { return getDef().instr(); }
};

template <> struct ObjTraits<DSLVal> {
  static constexpr DialectType ty{DSL_VAL};
  using FatRefT = DSLValRef;
};

class DSLValue : public FatRefUnion<DSLValRef, DSLTypeRef, ConstantRef,
                                    BlockRef, StringObjRef> {
public:
  using FatRefUnion::FatRefUnion;
  using FatRefUnion::operator=;
};

} // namespace dyno
