#pragma once

#include "dyno/Context.h"
#include "op/Function.h"
#include "op/MapObj.h"
#include "op/StringObj.h"

namespace dyno {

class OpDialectContext : public ContextMixin<OpDialectContext> {
public:
  Tuple<NewDeleteObjStore<Function>, NewDeleteObjStore<MapObj>,
        NewDeleteObjStore<StringObj>>
      stores;

  static constexpr DialectID dialect{DIALECT_OP};
  template <typename T> auto &getStore();
  template <> auto &getStore<Function>() { return stores.get<0>(); }
  template <> auto &getStore<MapObj>() { return stores.get<1>(); }
  template <> auto &getStore<StringObj>() { return stores.get<2>(); }
};
template <> struct DialectContext<DialectID{DIALECT_OP}> {
  using t = OpDialectContext;
};
}; // namespace dyno
