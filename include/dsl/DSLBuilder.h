#pragma once

#include "dsl/DSLContext.h"
#include "dsl/DSLValue.h"
#include "dsl/IDs.h"
#include "op/OpBuilder.h"

namespace dyno {
class DSLBuilder : public OpBuilder<DSLVal, DSLValue> {
public:
  using OpBuilder::OpBuilder;

  InstrRef buildOp(DialectOpcode opc, ArrayRef<DSLValue> others) {
    auto res = ctx.getStore<DSLVal>().create();
    return buildInstrRaw(opc, ArrayRef{res}, others).instr();
  }

  InstrRef buildLet(DSLValue val) {
    return buildInstrRaw(DSL_LET, ArrayRef<DSLValue>{}, ArrayRef{val}).instr();
  }

  InstrRef buildDef(DSLValue val) {
    return buildInstrRaw(DSL_DEF, ArrayRef<DSLValue>{}, ArrayRef{val}).instr();
  }

  InstrRef buildStruct(BlockRef blk) {
    auto res = ctx.getStore<DSLVal>().create();
    return buildInstrRaw(DSL_STRUCT, ArrayRef<DSLValue>{res, blk},
                         ArrayRef<DSLValue>{})
        .instr();
  }
};
} // namespace dyno
