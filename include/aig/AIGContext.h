#pragma once

#include "aig/AIG.h"
#include "dyno/Context.h"
#include "dyno/DialectInfo.h"
#include "dyno/IDImpl.h"
#include "dyno/Obj.h"
namespace dyno {

class AIGDialectContext {
public:
  using AIGStoreT = NewDeleteObjStore<AIGObj>;
  Tuple<AIGStoreT> stores;

  static constexpr DialectID dialect{DIALECT_AIG};

  template <typename T> auto &getStore();
  template <> auto &getStore<AIGObj>() { return stores.get<0>(); }

private:
  using AIGCopySignature = ObjTraits<typename AIGStoreT::value_type>::FatRefT (
      AIGStoreT::*)(FatObjRef<typename AIGStoreT::value_type> &&);

  // nops for now. Fat AIG node could get reference to AIG via instr to delete.
  // Thin is fully owned by AIG, so better just error
  void destroyFatAIGNode(FatDynObjRef<> ref) {}
  void destroyAIGNode(FatDynObjRef<>) {}

public:
  // clang-format off
  std::array<CallableRef<FatDynObjRef<>(DynObjRef)>, 3> resolverMethods = {
    CallableRef<FatDynObjRef<>(DynObjRef)>{&std::get<0>(stores),
      BindMethod<&AIGStoreT::resolveGeneric>::fv},

      // no context resolve support for (fat) AIG nodes
      CallableRef<FatDynObjRef<>(DynObjRef)>{},
      CallableRef<FatDynObjRef<>(DynObjRef)>{},
  };
  std::array<CallableRef<FatDynObjRef<>(FatDynObjRef<>)>, 3> copyMethods = {
    CallableRef<FatDynObjRef<>(FatDynObjRef<>)>{
        &std::get<0>(stores),
        &detail::castToSpecificRef<BindMethod<(AIGCopySignature)&AIGStoreT::create>::fv>},
    CallableRef<FatDynObjRef<>(DynObjRef)>{},
    CallableRef<FatDynObjRef<>(DynObjRef)>{},
  };
  std::array<CallableRef<void(FatDynObjRef<>)>, 3> destroyMethods = {
    CallableRef{&std::get<0>(stores),
        &detail::castToSpecificRef<BindMethod<&AIGStoreT::destroy>::fv>},
    CallableRef{this, BindMethod<&AIGDialectContext::destroyFatAIGNode>::fv},
    CallableRef{this, BindMethod<&AIGDialectContext::destroyAIGNode>::fv},
  };
  // clang-format on

  void reset() { std::get<0>(stores).reset(); }
};

template <> struct DialectContext<DialectID{DIALECT_AIG}> {
  using t = AIGDialectContext;
};
}; // namespace dyno
