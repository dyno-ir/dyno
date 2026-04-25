#pragma once

#include "dyno/Context.h"
#include "dyno/Interface.h"
#include "type/IDs.h"
#include "type/TypeInfo.h"

namespace dyno {

class TypeDialectContext : public ContextMixin<TypeDialectContext> {
public:
  TypeObjStore typeStore;
  // clang-format off
  Tuple<
    TypeObjStore&,
    RemapStore<DedupeStore<StructTypeObj>, 2, 0>&,
    RemapStore<DedupeStore<EnumTypeObj>,   2, 1>&,
    RemapStore<DedupeStore<ArrayTypeObj>,  2, 2>&
    > stores = {typeStore, typeStore.structStore, typeStore.enumStore, typeStore.arrayStore};
  // clang-format on

  StringDedupeMap strings;

  ArrayInterface<const char *> baseTypeNames;

  static constexpr DialectID dialect{DIALECT_TYPE};
  template <typename T> auto &getStore();
  template <> auto &getStore<TypeObj>() { return stores.get<0>(); }
  template <> auto &getStore<StructTypeObj>() { return stores.get<1>(); }
  template <> auto &getStore<EnumTypeObj>() { return stores.get<2>(); }
  template <> auto &getStore<ArrayTypeObj>() { return stores.get<3>(); }
};
template <> struct DialectContext<DIALECT_TYPE> {
  using t = TypeDialectContext;
};
}; // namespace dyno
