#pragma once
#include "dyno/Obj.h"
#include "dyno/Type.h"
namespace dyno {

// change type for API, but keep assignable lvalue.
template <typename TypedRefT, typename Ref = FatDynObjRef<>> class TypeHint {
  Ref &ref;

public:
  explicit TypeHint(Ref &ref) : ref(ref) {}

  Ref operator=(TypedRefT ref) {
    this->ref = ref;
    return ref;
  }

  TypedRefT as() { return ref.template as<TypedRefT>(); }
  operator TypedRefT() { return as(); }
};

}; // namespace dyno
