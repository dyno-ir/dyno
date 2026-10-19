#pragma once

#include "dsl/DSLValue.h"
#include "dyno/Context.h"
#include "dyno/IDObjStore.h"

namespace dyno {

class DSLDialectContext : public ContextMixin<DSLDialectContext> {
public:
  Tuple<NewDeleteObjStore<DSLVal>, NewDeleteObjStore<DSLTy>,
        IDObjStore<DSLTyVar>>
      stores;

  static constexpr DialectID dialect{DIALECT_DSL};
  template <typename T> auto &getStore();
  template <> auto &getStore<DSLVal>() { return stores.get<0>(); }
  template <> auto &getStore<DSLTy>() { return stores.get<1>(); }
  template <> auto &getStore<DSLTyVar>() { return stores.get<2>(); }
};
template <> struct DialectContext<DialectID{DIALECT_DSL}> {
  using t = DSLDialectContext;
};

}; // namespace dyno
