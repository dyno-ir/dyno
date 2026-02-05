#pragma once
#include "dyno/Obj.h"
#include "dyno/Type.h"
#include "op/IDs.h"
#include <string>

namespace dyno {

class StringObj {
public:
  std::string data;
  StringObj(DynObjRef, std::string &&data) : data(std::move(data)) {}
};

using StringObjRef = FatObjRef<StringObj>;

template <> struct ObjTraits<StringObj> {
  static constexpr DialectType ty{OP_STRING};
  using FatRefT = StringObjRef;
};

}; // namespace dyno
