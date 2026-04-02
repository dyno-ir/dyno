#pragma once

#include "dyno/CFG.h"
#include "dyno/Constant.h"
#include "dyno/DialectInfo.h"
#include "dyno/Interface.h"
#include "dyno/NewDeleteObjStore.h"
#include "dyno/Obj.h"
#include "dyno/Symbol.h"
#include "hw/DebugInfo.h"
#include "meta/MetaPassManager.h"
#include "support/CallableRef.h"
#include "support/TemplateUtil.h"
#include "support/Tuple.h"
#include <type_traits>
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

template <typename Derived> class ContextMixin {

  auto &self() { return *static_cast<Derived *>(this); }
  static constexpr size_t numStores =
      decltype(std::declval<Derived>().stores)::size;

  static unsigned getMaxTyID() {
    // find highest used type ID
    auto maxID = []<std::size_t... Is>(std::index_sequence<Is...>) {
      unsigned maxID = 0;
      (
          [&] {
            using StoreT = std::remove_reference_t<
                tuple_element_t<Is, decltype(std::declval<Derived>().stores)>>;
            maxID = std::max(
                maxID,
                unsigned(
                    ObjTraits<typename StoreT::value_type>::ty.getTypeID() &
                    127));
          }(),
          ...);
      return maxID;
    }(std::make_index_sequence<numStores>{});
    return maxID;
  }
  auto makeResolverMethods() {
    // create vector and assign elements
    Vec<CallableRef<FatDynObjRef<>(DynObjRef)>> arr(getMaxTyID() + 1);
    [&]<std::size_t... Is>(std::index_sequence<Is...>) {
      (
          [&] {
            auto &store = self().stores.template get<Is>();
            using StoreT = std::remove_reference_t<decltype(store)>;
            arr[ObjTraits<typename StoreT::value_type>::ty.getTypeID() & 127] =
                CallableRef{&store, BindMethod<&StoreT::resolveGeneric>::fv};
          }(),
          ...);
    }(std::make_index_sequence<numStores>{});
    return arr;
  }
  // convert method that takes/returns static arg into one taking/returning
  // dynamic arg
  template <auto Func>
  static FatDynObjRef<> castToSpecificRef(void *obj, FatDynObjRef<> ref) {
    using ArgT = std::remove_reference_t<
        tuple_element_t<1, function_args_t<decltype(Func)>>>;
    return Func(obj, ref.as<ArgT>());
  }
  auto makeCopyMethods() {
    // create vector and assign elements
    Vec<CallableRef<FatDynObjRef<>(FatDynObjRef<>)>> arr(getMaxTyID() + 1);
    [&]<std::size_t... Is>(std::index_sequence<Is...>) {
      (
          [&] {
            auto &store = self().stores.template get<Is>();
            using StoreT = std::remove_reference_t<decltype(store)>;
            using SignT = ObjTraits<typename StoreT::value_type>::FatRefT (
                StoreT::*)(FatObjRef<typename StoreT::value_type> &&);
            if constexpr (requires { (SignT)(&StoreT::create); }) {
              arr[ObjTraits<typename StoreT::value_type>::ty.getTypeID() &
                  127] = CallableRef{
                  &store,
                  castToSpecificRef<BindMethod<(SignT)(&StoreT::create)>::fv>};
            }
          }(),
          ...);
    }(std::make_index_sequence<numStores>{});
    return arr;
  }

public:
  Vec<CallableRef<FatDynObjRef<>(DynObjRef)>> resolverMethods =
      makeResolverMethods();
  Vec<CallableRef<FatDynObjRef<>(FatDynObjRef<>)>> copyMethods =
      makeCopyMethods();

  void reset() {
    self().stores.apply([](auto &...stores) { (stores.reset(), ...); });
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
  SymbolStore *symbols;

  template <typename T> auto &getStore();
  template <> auto &getStore<Instr>() { return instrs; }
  template <> auto &getStore<Constant>() { return constants; }
  template <> auto &getStore<Symbol>() { return *symbols; }

  BlockRef createBlock() { return cfg.blocks.create(cfg); };

  std::array<CallableRef<FatDynObjRef<>(DynObjRef)>, 5> resolverMethods;

  // clang-format off
  CoreDialectContext(SymbolStore* symbols = nullptr) : symbols(symbols), resolverMethods({
    // zeroth element is invalid in core dialect, forward to instr resolver which will assert
    CallableRef{&instrs, BindMethod<&InstrStoreT::resolveGeneric>::fv},
    CallableRef{&instrs, BindMethod<&InstrStoreT::resolveGeneric>::fv},
    CallableRef{&constants, BindMethod<&ConstantStoreT::resolveGeneric>::fv},
    CallableRef{&cfg.blocks, BindMethod<&decltype(cfg.blocks)::resolveGeneric>::fv},
    CallableRef{symbols, BindMethod<&SymbolStore::resolveGeneric>::fv},
  })
  // clang-format on
  {
    instrs.destroyHooks.emplace_back(
        [&](InstrRef instr) { instrSourceLocInfo.resetDebugInfo(instr); });
  }

  void reset() {
    instrs.reset();
    cfg.reset();
    constants.reset();
    instrSourceLocInfo.reset();
  }
};

template <> struct DialectContext<DialectID{DIALECT_CORE}> {
  using t = CoreDialectContext;
};

class Context {
  ArrayInterface<TypeErasedCtx> contexts;
  ArrayInterface<CallableRef<FatDynObjRef<>(DynObjRef)>> resolvers;
  ArrayInterface<CallableRef<FatDynObjRef<>(FatDynObjRef<>)>> copiers;
  Vec<CallableRef<void()>, MAX_NUM_DIALECTS> resets;

  DialectInfos dialectInfos;
  PassRegistry passRegistry;

public:
  Interface<DialectInfo> dialectInfosIF{dialectInfos.dialectInfoArr.data()};
  Interface<OpcodeInfo> opcodeInfosIF{dialectInfos.opcodeInfoArr.data()};
  Interface<TyInfo> typeInfosIF{dialectInfos.typeInfoArr.data()};

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

  // slow resolve
  FatDynObjRef<> resolve(DynObjRef ref) __attribute__((pure)) {
    return resolvers[ref](ref);
  }
  // slow copy
  FatDynObjRef<> copy(FatDynObjRef<> ref) {
    if (auto fn = copiers[ref])
      return fn(ref);
    return nullref;
  }

  template <typename T> ObjTraits<T>::FatRefT resolve(ObjRef<T> ref) {
    return getStore<T>().resolve(ref);
  }

  template <typename T> void registerDialect(T &context) {
    // context pointer
    assert(!contexts[T::dialect].ctx && "already registered?");
    contexts.registerDialect(T::dialect,
                             TypeErasedCtx{reinterpret_cast<void *>(&context)});

    if constexpr (requires { context.reset(); })
      resets.emplace_back(CallableRef{&context, &BindMethod<&T::reset>::fv});

    // dialect info registration (this can be overriden by the dialect)
    dyno::registerDialect<T::dialect>(this, dialectInfos.dialectInfoArr.data(),
                                      dialectInfos.typeInfoArr.data(),
                                      dialectInfos.opcodeInfoArr.data());
    assert(dialectInfos.dialectInfoArr[T::dialect] &&
           "no dialect registered? registerDialect template specialization "
           "header included?");

    // last thing: passes
    registerDialectPasses<T::dialect>(passRegistry);

    if constexpr (requires { context.resolverMethods; }) {
      resolvers.registerDialect(T::dialect, ArrayRef{context.resolverMethods});
    }
    if constexpr (requires { context.copyMethods; }) {
      copiers.registerDialect(T::dialect, ArrayRef{context.copyMethods});
    }
  }

  void reset() {
    for (auto reset : resets)
      reset();
  }

  CFG &getCFG() { return getCtx<CoreDialectContext>().cfg; }
  template <> auto &getStore<Block>() { return getCFG().blocks; }
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

}; // namespace dyno
