#pragma once

#include "dyno/Context.h"
#include "op/Function.h"
#include "op/MapObj.h"
#include "op/StringObj.h"

namespace dyno {

class OpDialectContext : public ContextMixin<OpDialectContext> {
public:
  std::tuple<NewDeleteObjStore<Function>, NewDeleteObjStore<MapObj>,
             NewDeleteObjStore<StringObj>>
      stores;

  static constexpr DialectID dialect{DIALECT_OP};
  template <typename T> auto &getStore();
  template <> auto &getStore<Function>() { return std::get<0>(stores); }
  template <> auto &getStore<MapObj>() { return std::get<1>(stores); }
  template <> auto &getStore<StringObj>() { return std::get<2>(stores); }
};
template <> struct DialectContext<DialectID{DIALECT_OP}> {
  using t = OpDialectContext;
};
}; // namespace dyno
