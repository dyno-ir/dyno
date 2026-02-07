#pragma once

#include "dyno/CFG.h"
#include "dyno/Constant.h"
#include "dyno/Context.h"
#include "dyno/DialectInfo.h"
#include "dyno/Instr.h"
#include "dyno/NewDeleteObjStore.h"
#include "hw/DebugInfo.h"
#include "op/MapObj.h"
#include <tuple>

namespace dyno {
class MetaContext {
  NewDeleteObjStore<Instr> instrs;
  ConstantStore constants;
  CFG cfg;
  NewDeleteObjStore<MapObj> mapObjs;

public:
  auto &getConstants() { return constants; }
  auto &getMaps() { return mapObjs; }
  auto &getInstrs() { return instrs; }
  auto &getCFG() { return cfg; }

  SourceLocInfo<Instr> sourceLocInfo;
};

class MetaDialectContext : public ContextMixin<MetaDialectContext> {
public:
  std::tuple<> stores;
  static constexpr DialectID dialect{DIALECT_META};
  template <typename T> auto &getStore();
};
template <> struct DialectContext<DialectID{DIALECT_META}> {
  using t = MetaDialectContext;
};
}; // namespace dyno
