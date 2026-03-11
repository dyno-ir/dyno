#pragma once

#include "aig/AIG.h"
#include "dyno/Context.h"
#include "dyno/DialectInfo.h"
#include "dyno/IDImpl.h"
namespace dyno {

class AIGDialectContext : public ContextMixin<AIGDialectContext> {
public:
  Tuple<NewDeleteObjStore<AIGObj>> stores;

  static constexpr DialectID dialect{DIALECT_AIG};

  template <typename T> auto &getStore();
  template <> auto &getStore<AIGObj>() { return stores.get<0>(); }
};

template <> struct DialectContext<DialectID{DIALECT_AIG}> {
  using t = AIGDialectContext;
};
}; // namespace dyno
