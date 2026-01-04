#pragma once

#include "dyno/CFG.h"
#include "dyno/Constant.h"
#include "dyno/DialectInfo.h"
#include "dyno/Interface.h"
#include "dyno/NewDeleteObjStore.h"
#include "dyno/Obj.h"
#include "hw/DebugInfo.h"
namespace dyno {

struct TypeErasedCtx {
  void *store;
};

template <> struct InterfaceTraits<TypeErasedCtx> {
  static const TypeErasedCtx dispatch1(DynObjRef ref,
                                       const TypeErasedCtx *interfaces) {
    return interfaces[ref.getDialectID()];
  }
  static const TypeErasedCtx dispatch2(DynObjRef, TypeErasedCtx store) {
    return store;
  }
  static const unsigned ID = ~0;
};

template <DialectID> struct DialectContext;
template <DialectID ID> using DialectContextT = DialectContext<ID>::t;

class Context {
  ArrayInterface<TypeErasedCtx> contexts;

public:
  template <typename T> T &get() {
    return *reinterpret_cast<T *>(contexts[T::ty.getDialectID()].store);
  }

  template <typename T> auto &getStore() {
    return get<DialectContextT<ObjTraits<T>::ty.getDialectID()>>()
        .template getStore<T>();
  }
};

class CoreDialectContext {
  using InstrStoreT = NewDeleteObjStore<Instr>;
  using ConstantStoreT = ConstantStore;

public:
  static constexpr DialectID dialect{DIALECT_CORE};
  InstrStoreT instrs;
  CFG cfg;
  ConstantStoreT constants;
  SourceLocInfo<Instr> instrSourceLocInfo;

  template <typename T> auto &getStore();
  template <> auto &getStore<Instr>() { return instrs; }
  template <> auto &getStore<Constant>() { return constants; }
};

template <> struct DialectContext<DialectID{DIALECT_CORE}> {
  using t = CoreDialectContext;
};

}; // namespace dyno
