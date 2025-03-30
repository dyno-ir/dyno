#pragma once

#include "dyno/CFG.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/IDs.h"
#include "support/SmallVec.h"

namespace dyno {

class Register {
public:
  InstrDefUse defUse;
  // todo: size

  Register(DynObjRef) {}
};

class RegisterRef : FatObjRef<Register> {
  using FatObjRef<Register>::FatObjRef;
};

template <> struct ObjTraits<Register> {
  static constexpr DialectID dialect{DIALECT_RTL};
  static constexpr TyID ty{RTL_REGISTER};
  using FatRefT = RegisterRef;
};

}; // namespace dyno
