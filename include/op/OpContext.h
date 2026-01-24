#pragma once

#include "dyno/Context.h"
#include "op/Function.h"
namespace dyno {

class OpDialectContext {
  NewDeleteObjStore<Function> functionObjs;

public:
  static constexpr DialectID dialect{DIALECT_OP};
  auto &getFunctions() { return functionObjs; }
  template <typename T> auto &getStore();
  template <> auto &getStore<Function>() { return functionObjs; }
};
template <> struct DialectContext<DialectID{DIALECT_OP}> {
  using t = OpDialectContext;
};
}; // namespace dyno
