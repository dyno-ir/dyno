#pragma once
#include "dyno/IDImpl.h"
#include "dyno/passes/ResolveImports.h"
#include "meta/MetaPassManager.h"

namespace dyno {
template <>
inline void
registerDialectPasses<DialectID{DIALECT_CORE}>(PassRegistry &passRegistry) {
  passRegistry.registerPass<ResolveImportsPass>();
}
}; // namespace dyno
