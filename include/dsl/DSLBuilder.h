#pragma once

#include "dsl/DSLContext.h"
#include "dsl/DSLValue.h"
#include "dsl/IDs.h"
#include "op/OpBuilder.h"

namespace dyno {
class DSLBuilder : public OpBuilder<DSLVal, DSLValue> {
public:
  using OpBuilder::OpBuilder;

  InstrRef buildOp(DialectOpcode opc, DSLValRef res,
                   ArrayRef<DSLValue> others) {
    return buildInstrRaw(opc, ArrayRef{res}, others).instr();
  }

  InstrRef buildOp(DialectOpcode opc, ArrayRef<DSLValue> others) {
    auto res = ctx.getStore<DSLVal>().create();
    return buildOp(opc, res, others);
  }

  DSLTyRef buildType(DialectOpcode opc, ArrayRef<DSLTypeRef> operands) {
    auto type = ctx.getStore<DSLTy>().create();
    buildInstrRaw(opc, ArrayRef{type}, operands);
    return type;
  }

  InstrRef buildParam(DSLValue pattern) { return buildOp(OP_PARAM, {pattern}); }

  InstrRef buildFunc(BlockRef body) {
    auto res = ctx.getStore<DSLVal>().create();
    return buildInstrRaw(DSL_FUNC, ArrayRef<DSLValue>{res, body},
                         ArrayRef<DSLValue>{})
        .instr();
  }

  InstrRef buildReturn(DSLValue value) {
    return buildInstrRaw(OP_RETURN, ArrayRef<DSLValue>{}, ArrayRef{value})
        .instr();
  }

  InstrRef buildIntLit(ConstantRef constant, bool isSigned, bool isUnsuffixed) {
    return buildOp(DSL_INT_LIT, {constant, ConstantRef::fromBool(isSigned),
                                 ConstantRef::fromBool(isUnsuffixed)});
  }

  InstrRef buildLet(DSLValue val) {
    return buildInstrRaw(DSL_LET, ArrayRef<DSLValue>{}, ArrayRef{val}).instr();
  }

  InstrRef buildDef(DSLValue val) {
    return buildInstrRaw(DSL_DEF, ArrayRef<DSLValue>{}, ArrayRef{val}).instr();
  }

  InstrRef buildInstance(DSLValue ty, BlockRef blk) {
    auto res = ctx.getStore<DSLVal>().create();
    return buildInstrRaw(DSL_INSTANCE, ArrayRef<DSLValue>{res, blk},
                         ArrayRef<DSLValue>{ty})
        .instr();
  }

  InstrRef buildBlock(BlockRef blk) {
    auto res = ctx.getStore<DSLVal>().create();
    return buildInstrRaw(DSL_BLOCK, ArrayRef<DSLValue>{res, blk},
                         ArrayRef<DSLValue>{})
        .instr();
  }

  InstrRef buildStruct(BlockRef blk) {
    auto res = ctx.getStore<DSLVal>().create();
    return buildInstrRaw(DSL_STRUCT, ArrayRef<DSLValue>{res, blk},
                         ArrayRef<DSLValue>{})
        .instr();
  }
};
} // namespace dyno
