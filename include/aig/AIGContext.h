#pragma once

#include "aig/AIG.h"
#include "dyno/Context.h"
#include "dyno/DialectInfo.h"
#include "dyno/IDImpl.h"
namespace dyno {

class AIGDialectContext : public ContextMixin<AIGDialectContext> {
public:
  std::tuple<NewDeleteObjStore<AIGObj>> stores;

  static constexpr DialectID dialect{DIALECT_AIG};

  template <typename T> auto &getStore();
  template <> auto &getStore<AIGObj>() { return std::get<0>(stores); }
};

template <> struct DialectContext<DialectID{DIALECT_AIG}> {
  using t = AIGDialectContext;
};
}; // namespace dyno
