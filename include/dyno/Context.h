#pragma once

#include "dyno/CFG.h"
#include "dyno/Constant.h"
#include "dyno/DialectInfo.h"
#include "dyno/Interface.h"
#include "dyno/NewDeleteObjStore.h"
#include "dyno/Obj.h"
#include "hw/DebugInfo.h"
#include "meta/MetaPassManager.h"
#include "support/CallableRef.h"
namespace dyno {

struct TypeErasedCtx {
  void *ctx;
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
  ArrayInterface<MemberRef<FatDynObjRef<>(void *, DynObjRef)>> resolvers;

  DialectInfos dialectInfos;
  PassRegistry passRegistry;

public:
  DialectInfos &getDialectInfos() { return dialectInfos; }
  PassRegistry &getPassRegistry() { return passRegistry; }

  template <typename T> T &getCtx() {
    return *reinterpret_cast<T *>(contexts[T::dialect].ctx);
  }

  template <typename T> auto &getStore() {
    return getCtx<DialectContextT<ObjTraits<T>::ty.getDialectID()>>()
        .template getStore<T>();
  }

  FatDynObjRef<> resolve(DynObjRef ref) { return resolvers[ref](ref); }

  template <typename T> void registerDialect(T &context) {
    // context pointer
    assert(!contexts[T::dialect].ctx && "already registered?");
    contexts.registerDialect(T::dialect,
                             TypeErasedCtx{reinterpret_cast<void *>(&context)});

    // dialect info registration (this can be overriden by the dialect)
    dyno::registerDialect<T::dialect>(dialectInfos.dialectInfoArr.data(),
                                      dialectInfos.typeInfoArr.data(),
                                      dialectInfos.opcodeInfoArr.data());

    // last thing: passes
    registerDialectPasses<T::dialect>(passRegistry);

    if constexpr (requires { context.resolverMethods; }) {
      resolvers.registerDialect(T::dialect, ArrayRef{context.resolverMethods});
    }
  }
};

// ref type for easy reassigning
// class ContextRef {
// private:
//   Context *context;
//   Context &self() { return *context; }

// public:
//   DialectInfos &getDialectInfos() { return self().getDialectInfos(); }
//   PassRegistry &getPassRegistry() { return self().getPassRegistry(); }
//   template <typename T> T &getCtx() { return self().getCtx<T>(); }
//   template <typename T> auto &getStore() { return self().getStore<T>(); }
//   FatDynObjRef<> resolve(DynObjRef ref) { return self().resolve(ref); }
// };

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
