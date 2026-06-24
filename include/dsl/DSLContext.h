#pragma once

#include "dsl/DSLValue.h"
#include "dyno/Context.h"

namespace dyno {

class DSLDialectContext : public ContextMixin<DSLDialectContext> {
public:
  Tuple<NewDeleteObjStore<DSLVal>> stores;

  static constexpr DialectID dialect{DIALECT_DSL};
  template <typename T> auto &getStore();
  template <> auto &getStore<DSLVal>() { return stores.get<0>(); }
};
template <> struct DialectContext<DialectID{DIALECT_DSL}> {
  using t = DSLDialectContext;
};

}; // namespace dyno
