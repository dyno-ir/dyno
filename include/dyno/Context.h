#pragma once

#include "dyno/CFG.h"
#include "dyno/Constant.h"
#include "dyno/DialectInfo.h"
#include "dyno/IDs.h"
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

namespace detail {
// convert method that takes/returns static arg into one taking/returning
// dynamic arg
template <auto Func>
static auto castToSpecificRef(void *obj, FatDynObjRef<> ref) {
  using ArgT = std::remove_reference_t<
      tuple_element_t<1, function_args_t<decltype(Func)>>>;
  if constexpr (requires { FatDynObjRef<>{Func(obj, ref.as<ArgT>())}; })
    return FatDynObjRef<>{Func(obj, ref.as<ArgT>())};
  else
    return Func(obj, ref.as<ArgT>());
}
template <auto Func>

static auto castToSpecificRefThin(void *obj, DynObjRef ref) {
  using ArgT = std::remove_reference_t<
      tuple_element_t<1, function_args_t<decltype(Func)>>>;
  if constexpr (requires { FatDynObjRef<>{Func(obj, ref.as<ArgT>())}; })
    return FatDynObjRef<>{Func(obj, ref.as<ArgT>())};
  else
    return Func(obj, ref.as<ArgT>());
}
}; // namespace detail

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
    assert(maxID <= 127);
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
                CallableRef{&store, detail::castToSpecificRefThin<
                                        BindMethod<&StoreT::resolve>::fv>};
          }(),
          ...);
    }(std::make_index_sequence<numStores>{});
    return arr;
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
                  &store, detail::castToSpecificRef<
                              BindMethod<(SignT)(&StoreT::create)>::fv>};
            }
          }(),
          ...);
    }(std::make_index_sequence<numStores>{});
    return arr;
  }
  auto makeDestroyMethods() {
    // create vector and assign elements
    Vec<CallableRef<void(FatDynObjRef<>)>> arr(getMaxTyID() + 1);
    [&]<std::size_t... Is>(std::index_sequence<Is...>) {
      (
          [&] {
            auto &store = self().stores.template get<Is>();
            using StoreT = std::remove_reference_t<decltype(store)>;
            if constexpr (requires { (&StoreT::destroy); }) {
              arr[ObjTraits<typename StoreT::value_type>::ty.getTypeID() &
                  127] =
                  CallableRef{&store, detail::castToSpecificRef<
                                          BindMethod<(&StoreT::destroy)>::fv>};
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
  Vec<CallableRef<void(FatDynObjRef<>)>> destroyMethods = makeDestroyMethods();

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
  // set via setSymbols or constructor
  SymbolStore *symbols;

  template <typename T> auto &getStore();
  template <> auto &getStore<Instr>() { return instrs; }
  template <> auto &getStore<Constant>() { return constants; }
  template <> auto &getStore<Symbol>() { return *symbols; }

  BlockRef createBlock() { return cfg.blocks.create(cfg); };

  // clang-format off
  std::array<CallableRef<FatDynObjRef<>(DynObjRef)>, 5> resolverMethods = {
    CallableRef<FatDynObjRef<>(DynObjRef)>{},
    CallableRef{&instrs, BindMethod<&InstrStoreT::resolveGeneric>::fv},
    CallableRef{&constants, BindMethod<&ConstantStoreT::resolveGeneric>::fv},
    CallableRef{&cfg.blocks, BindMethod<&decltype(cfg.blocks)::resolveGeneric>::fv},
    CallableRef<FatDynObjRef<>(DynObjRef)>{},
  };
  std::array<CallableRef<FatDynObjRef<>(FatDynObjRef<>)>, 5> copyMethods = {
    CallableRef<FatDynObjRef<>(FatDynObjRef<>)>{},
    CallableRef<FatDynObjRef<>(FatDynObjRef<>)>{},
    CallableRef<FatDynObjRef<>(FatDynObjRef<>)>{
        &constants,
        &detail::castToSpecificRef<BindMethod<&ConstantStore::create>::fv>},
    CallableRef<FatDynObjRef<>(FatDynObjRef<>)>{},
    CallableRef<FatDynObjRef<>(FatDynObjRef<>)>{},
  };
  std::array<CallableRef<void(FatDynObjRef<>)>, 5> destroyMethods = {
    CallableRef<void(FatDynObjRef<>)>{},
    CallableRef{&instrs,
        &detail::castToSpecificRef<BindMethod<&InstrStoreT::destroy>::fv>},
    CallableRef{&constants,
        &detail::castToSpecificRef<BindMethod<&ConstantStoreT::destroy>::fv>},
    CallableRef{&cfg.blocks,
        &detail::castToSpecificRef<BindMethod<&decltype(cfg.blocks)::destroy>::fv>},
    CallableRef<void(FatDynObjRef<>)>{},
  };
  // clang-format on

  CoreDialectContext(SymbolStore *symbols = nullptr) : symbols(symbols) {
    instrs.destroyHooks.emplace_back(
        [&](InstrRef instr) { instrSourceLocInfo.resetDebugInfo(instr); });
  }

  void setSymbols(SymbolStore &symbolStore) {
    symbols = &symbolStore;
    resolverMethods[CORE_SYMBOL.type & 127] =
        CallableRef{symbols, BindMethod<&SymbolStore::resolveGeneric>::fv};
    copyMethods[CORE_SYMBOL.type & 127] = CallableRef{
        symbols,
        &detail::castToSpecificRef<BindMethod<&SymbolStore::create>::fv>};
    destroyMethods[CORE_SYMBOL.type & 127] = CallableRef{
        symbols,
        &detail::castToSpecificRef<BindMethod<&SymbolStore::destroy>::fv>};
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
  ArrayInterface<CallableRef<void(FatDynObjRef<>)>> destroyers;
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
  // slow destroy
  void destroy(FatDynObjRef<> ref) { destroyers[ref](ref); }

  template <typename T> ObjTraits<T>::FatRefT resolve(ObjRef<T> ref) {
    return getStore<T>().resolve(ref);
  }
  template <typename T> ObjTraits<T>::FatRefT copy(FatObjRef<T> ref) {
    return getStore<T>().create(ref);
  }
  template <typename T> void destroy(FatObjRef<T> ref) {
    return getStore<T>().destroy(ref);
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
    if constexpr (requires { context.destroyMethods; }) {
      destroyers.registerDialect(T::dialect, ArrayRef{context.destroyMethods});
    }
  }

  void reset() {
    for (auto reset : resets)
      reset();
  }

  CFG &getCFG() { return getCtx<CoreDialectContext>().cfg; }
  template <> auto &getStore<Block>() { return getCFG().blocks; }

  // destroy defs, removes others from instrDef use if tracked
  void destroyInstr(InstrRef instr) {
    for (auto oref : instr.defs()) {
      auto obj = oref->fat();
      if (Operand::isDefUseOperand(obj)) {
#ifdef DYNO_DBG
        reinterpret_cast<InstrDefUse *>(obj.getPtr())
            ->replaceAllUsesWith(nullref);
#endif
      }
      destroy(obj);
    }

    if (getCtx<CoreDialectContext>().cfg.contains(instr))
      getCtx<CoreDialectContext>().cfg[instr].erase();
    instr.destroyOthers();
    getStore<Instr>().destroy(instr);
  }
};

}; // namespace dyno
