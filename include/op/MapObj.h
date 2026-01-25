#pragma once
#include "dyno/Obj.h"
#include "dyno/Type.h"
#include "op/IDs.h"
#include <map>

namespace dyno {

class MapObj {
public:
  std::map<std::string, std::string> data;
  MapObj(DynObjRef, std::map<std::string, std::string> &&data)
      : data(std::move(data)) {}
};

using MapRef = FatObjRef<MapObj>;

template <> struct ObjTraits<MapObj> {
  static constexpr DialectType ty{OP_MAP};
  using FatRefT = MapRef;
};

}; // namespace dyno