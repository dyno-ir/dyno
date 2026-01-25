#pragma once

#include "aig/AIG.h"
#include "dyno/Context.h"
#include "dyno/DialectInfo.h"
#include "dyno/IDImpl.h"
namespace dyno {

class AIGDialectContext {
  NewDeleteObjStore<AIGObj> aigObjs;

public:
  static constexpr DialectID dialect{DIALECT_AIG};
  auto &getAIGs() { return aigObjs; }
  template <typename T> auto &getStore();
  template <> auto &getStore<AIGObj>() { return aigObjs; }
};

template <> struct DialectContext<DialectID{DIALECT_AIG}> {
  using t = AIGDialectContext;
};
}; // namespace dyno
