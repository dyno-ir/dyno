#pragma once

#include "dyno/Context.h"
#include "dyno/DialectInfo.h"
#include "dyno/IDImpl.h"
#include "dyno/Opcode.h"
#include "meta/MetaPassManager.h"

namespace dyno {
using MetaType = SpecificDialectType<DialectID{DIALECT_META}>;
using MetaOpcode = SpecificDialectOpcode<DialectID{DIALECT_META}>;

template <>
inline void registerDialect<DialectID{DIALECT_META}>(
    Context *ctx, const DialectInfo **dialectInfos, ArrayRef<TyInfo> *typeInfos,
    ArrayRef<OpcodeInfo> *opcodeInfos) {
  static constexpr DialectInfo info{"meta"};
  dialectInfos[DIALECT_META] = &info;

  static constexpr std::array<TyInfo, 0> tyInfoArr;
  typeInfos[DIALECT_META] = ArrayRef{tyInfoArr};

  if (ctx) {
    // meta needs a context to find psses. no opcodes w/o context.
    opcodeInfos[DIALECT_META] =
        ArrayRef{ctx->getPassRegistry().metaOpcodeInfoArr.data(),
                 ctx->getPassRegistry().metaOpcodeInfoArr.size()};
  } else {
    opcodeInfos[DIALECT_META] = ArrayRef<OpcodeInfo>::emptyRef();
  }
}

template <> struct DialectTraits<DIALECT_META> {
  constexpr static DialectInfo info{"meta"};
};

} // namespace dyno
