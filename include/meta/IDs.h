#pragma once

#include "dyno/DialectInfo.h"
#include "dyno/IDImpl.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Opcode.h"
#include "meta/MetaPassManager.h"

namespace dyno {
using MetaType = SpecificDialectType<DialectID{DIALECT_META}>;
using MetaOpcode = SpecificDialectOpcode<DialectID{DIALECT_META}>;

inline MetaPassManager metaPassManager;

template <>
inline void
registerDialect<DialectID{DIALECT_META}>(const DialectInfo **dialectInfos,
                                         ArrayRef<TyInfo> *typeInfos,
                                         ArrayRef<OpcodeInfo> *opcodeInfos) {
  static constexpr DialectInfo info{"meta"};
  dialectInfos[DIALECT_META] = &info;

  static constexpr std::array<TyInfo, 0> tyInfoArr;
  typeInfos[DIALECT_META] = ArrayRef{tyInfoArr};

  opcodeInfos[DIALECT_META] =
      ArrayRef{metaPassManager.metaOpcodeInfoArr.data(),
               metaPassManager.metaOpcodeInfoArr.size()};
}

template <> struct DialectTraits<DIALECT_META> {
  constexpr static DialectInfo info{"meta"};
};

} // namespace dyno