#pragma once

#include "dyno/Context.h"
#include "op/Function.h"
#include "op/MapObj.h"
#include "op/StringObj.h"

namespace dyno {

class OpDialectContext {
  NewDeleteObjStore<Function> functionObjs;
  NewDeleteObjStore<MapObj> mapObjs;
  NewDeleteObjStore<StringObj> stringObjs;

public:
  static constexpr DialectID dialect{DIALECT_OP};
  auto &getFunctions() { return functionObjs; }
  template <typename T> auto &getStore();
  auto &getMaps() { return mapObjs; }
  template <> auto &getStore<Function>() { return functionObjs; }
  template <> auto &getStore<MapObj>() { return mapObjs; }
  template <> auto &getStore<StringObj>() { return stringObjs; }
};
template <> struct DialectContext<DialectID{DIALECT_OP}> {
  using t = OpDialectContext;
};
}; // namespace dyno
