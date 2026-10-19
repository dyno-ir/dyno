#pragma once

#include "dsl/DSLValue.h"
#include "op/IDs.h"
#include "support/Ranges.h"

namespace dyno {

class DSLInstrRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  DSLInstrRef(InstrRef ref) : InstrRef(ref) {}

  OperandRef result() const { return def(); }
  DSLValue getResult() const { return result()->as<DSLValue>(); }
};

class DSLBindingInstrRef : public OpcodeInstrRef<InstrRef, DSL_LET, DSL_DEF> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  OperandRef initializer() const { return other(0); }
  DSLValue getInitializer() const { return initializer()->as<DSLValue>(); }
};

class DSLAssignInstrRef : public OpcodeInstrRef<DSLInstrRef, DSL_ASSIGN> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  bool isBinding() const {
    auto use = getResult().as<DSLValRef>().getSingleUse();
    return use && use->instr().is<DSLBindingInstrRef>();
  }

  OperandRef pattern() const { return other(0); }
  DSLValue getPattern() const { return pattern()->as<DSLValue>(); }
  OperandRef initializer() const { return other(1); }
  DSLValue getInitializer() const { return initializer()->as<DSLValue>(); }
};

class DSLFuncInstrRef : public OpcodeInstrRef<DSLInstrRef, DSL_FUNC> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  // Value functions define a body; function types use (return, parameter).
  bool isTy() const { return def()->is<DSLTyRef>(); }
  OperandRef body() const { return def(1); }
  BlockRef getBody() const { return body()->as<BlockRef>(); }
  OperandRef retTy() const { return other(0); }
  DSLTypeRef getRetTy() const { return retTy()->as<DSLTypeRef>(); }
  OperandRef paramTy() const { return other(1); }
  DSLTypeRef getParamTy() const { return paramTy()->as<DSLTypeRef>(); }

  DSLAssignInstrRef findBinding() const {
    for (auto use : getResult().as<DSLValRef>().uses()) {
      auto assignment = use.instr().dyn_as<DSLAssignInstrRef>();
      if (assignment && use == assignment.initializer() &&
          assignment.isBinding())
        return assignment;
    }
    return nullref;
  }
};

class DSLApplyInstrRef : public OpcodeInstrRef<DSLInstrRef, DSL_APPLY> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  OperandRef callee() const { return other(0); }
  DSLValue getCallee() const { return callee()->as<DSLValue>(); }
  OperandRef arg() const { return other(1); }
  DSLValue getArg() const { return arg()->as<DSLValue>(); }
};

class DSLBlockInstrRef : public OpcodeInstrRef<DSLInstrRef, DSL_BLOCK> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  OperandRef body() const { return def(1); }
  BlockRef getBody() const { return body()->as<BlockRef>(); }
};

class DSLStructInstrRef : public OpcodeInstrRef<DSLInstrRef, DSL_STRUCT> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  OperandRef body() const { return def(1); }
  BlockRef getBody() const { return body()->as<BlockRef>(); }
};

class DSLEnumInstrRef : public OpcodeInstrRef<DSLInstrRef, DSL_ENUM> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  OperandRef body() const { return def(1); }
  BlockRef getBody() const { return body()->as<BlockRef>(); }
};

class DSLTupleInstrRef : public OpcodeInstrRef<DSLInstrRef, DSL_TUPLE> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  bool isTy() const { return def()->is<DSLTyRef>(); }
  unsigned getNumElements() const { return getNumOthers(); }
  OperandRef element(unsigned index) const { return other(index); }
  DSLValue getElement(unsigned index) const {
    return element(index)->as<DSLValue>();
  }
  auto elements() { return others(); }
  auto getElements() { return elements().as<DSLValue>(); }
};

class DSLLetInstrRef : public OpcodeInstrRef<DSLBindingInstrRef, DSL_LET> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;
};

class DSLDefInstrRef : public OpcodeInstrRef<DSLBindingInstrRef, DSL_DEF> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;
};

class DSLAccessInstrRef : public OpcodeInstrRef<DSLInstrRef, DSL_ACCESS> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  OperandRef base() const { return other(0); }
  DSLValue getBase() const { return base()->as<DSLValue>(); }
  OperandRef member() const { return other(1); }
  StringObjRef getMember() const { return member()->as<StringObjRef>(); }
};

class DSLInstanceInstrRef : public OpcodeInstrRef<DSLInstrRef, DSL_INSTANCE> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  OperandRef ty() const { return other(0); }
  DSLValue getTy() const { return ty()->as<DSLValue>(); }
  OperandRef body() const { return def(1); }
  BlockRef getBody() const { return body()->as<BlockRef>(); }
};

class DSLStringLitInstrRef
    : public OpcodeInstrRef<DSLInstrRef, DSL_STRING_LIT> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  OperandRef literal() const { return other(0); }
  StringObjRef getLiteral() const { return literal()->as<StringObjRef>(); }
};

class DSLVarInstrRef : public OpcodeInstrRef<DSLInstrRef, DSL_VAR> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  OperandRef name() const { return other(0); }
  StringObjRef getName() const { return name()->as<StringObjRef>(); }
};

class DSLRefInstrRef : public OpcodeInstrRef<DSLInstrRef, DSL_REF> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  OperandRef target() const { return other(0); }
  DSLValue getTarget() const { return target()->as<DSLValue>(); }
};

class DSLIntLitInstrRef : public OpcodeInstrRef<DSLInstrRef, DSL_INT_LIT> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  OperandRef literal() const { return other(0); }
  ConstantRef getLiteral() const { return literal()->as<ConstantRef>(); }
  OperandRef signedFlag() const { return other(1); }
  ConstantRef getSignedFlag() const { return signedFlag()->as<ConstantRef>(); }
  OperandRef unsuffixedFlag() const { return other(2); }
  ConstantRef getUnsuffixedFlag() const {
    return unsuffixedFlag()->as<ConstantRef>();
  }
  bool isSigned() const { return getSignedFlag().getExactVal() != 0; }
  bool isUnsuffixed() const { return getUnsuffixedFlag().getExactVal() != 0; }
};

class DSLBoolLitInstrRef : public OpcodeInstrRef<DSLInstrRef, DSL_BOOL_LIT> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  OperandRef literal() const { return other(0); }
  ConstantRef getLiteral() const { return literal()->as<ConstantRef>(); }
};

class DSLParamInstrRef : public OpcodeInstrRef<DSLInstrRef, OP_PARAM> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  OperandRef pattern() const { return other(0); }
  DSLValue getPattern() const { return pattern()->as<DSLValue>(); }
};

class DSLReturnInstrRef : public OpcodeInstrRef<InstrRef, OP_RETURN> {
public:
  using OpcodeInstrRef::OpcodeInstrRef;

  OperandRef value() const { return other(0); }
  DSLValue getValue() const { return value()->as<DSLValue>(); }
};

} // namespace dyno
