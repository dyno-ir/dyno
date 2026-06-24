#pragma once

#include "dyno/Builder.h"
#include "dyno/Context.h"
#include "op/Function.h"
#include "op/IDs.h"
#include "op/OpContext.h"
#include "op/StructuredControlFlow.h"

namespace dyno {

template <typename DefT, typename ValueT> class OpBuilder : public CoreBuilder {
public:
  using CoreBuilder::CoreBuilder;

  IfInstrRef buildIfElse(ValueT cond, BlockRef trueBlk, BlockRef falseBlk,
                         unsigned yieldPrealloc = 0) {
    auto iB = buildInstrRaw(OP_IF, {trueBlk, falseBlk}, yieldPrealloc + 1);

    for (unsigned i = 0; i < yieldPrealloc; ++i)
      iB.addRef(ctx.getStore<DefT>().create());

    iB.other().addRef(cond);

    return IfInstrRef(iB.instr());
  }

  IfInstrRef buildIf(ValueT cond, BlockRef trueBlk) {
    auto iB = buildInstrRaw(OP_IF, {trueBlk}, {cond});
    return IfInstrRef(iB.instr());
  }

  InstrRef buildYield(ValueT val) {
    return buildInstrRaw(OP_YIELD, {}, {val}).instr();
  }
};

} // namespace dyno
