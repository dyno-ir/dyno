#pragma once

#include "dyno/CFG.h"
#include "dyno/Constant.h"
#include "dyno/DialectInfo.h"
#include "dyno/Instr.h"
#include "dyno/NewDeleteObjStore.h"
#include "hw/DebugInfo.h"
#include "op/MapObj.h"
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

class MetaDialectContext {
  NewDeleteObjStore<MapObj> mapObjs;

public:
  static constexpr DialectID dialect{DIALECT_META};
  auto &getMaps() { return mapObjs; }
  template <typename T> auto &get();
  template <> auto &get<MapObj>() { return mapObjs; }
};
}; // namespace dyno
