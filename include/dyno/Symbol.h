#pragma once
#include "dyno/Instr.h"
#include "dyno/NewDeleteObjStore.h"
#include "dyno/Obj.h"
#include "dyno/Type.h"
#include "support/ErrorRecovery.h"
#include "support/Optional.h"
#include "support/StringRef.h"
#include "support/TwoLevelSet.h"

namespace dyno {

class Symbol {
public:
  InstrDefUse defUse;
  const std::string name;
  Optional<DialectType> type = nullopt;
  Context *defCtx = nullptr;

  Symbol(DynObjRef, SSOStringRef name, Optional<DialectType> type = nullopt)
      : name(name.begin(), name.end()), type(type) {}
  Symbol(DynObjRef, FatObjRef<Symbol> other)
      : name(other->name), type(other->type) {}
};

class SymbolRef : public FatObjRef<Symbol> {
public:
  using FatObjRef::FatObjRef;
};

template <> struct ObjTraits<Symbol> {
  static constexpr DialectType ty{CORE_SYMBOL};
  using FatRefT = SymbolRef;
};

class SymbolStore {
  NewDeleteObjStore<Symbol> store;
  TwoLevelMap<SSOStringRef, ObjRef<Symbol>> map;

public:
  SymbolRef create(FatObjRef<Symbol> other) {
    auto ref = findOrInsert(SSOStringRef{other->name});
    if (other->defCtx) {
      if (ref->defCtx && ref->defCtx != other->defCtx)
        report_fatal_error("symbol defined in multiple contexts");
      ref->defCtx = other->defCtx;
    }
    if (other->type) {
      if (ref->type && ref->type != other->type)
        report_fatal_error("symbol referenced with different types");
      ref->type = other->type;
    }
    return ref;
  }
  SymbolRef findOrInsert(SSOStringRef name) {
    // todo: set?
    auto [found, it] = map.findOrInsertPair(name, [&]() {
      auto ref = store.create(name);
      return std::make_pair(SSOStringRef{ref->name}, ref);
    });
    return store.resolve(it.val());
  }
  SymbolRef resolve(ObjRef<Symbol> ref) { return store.resolve(ref); }
  FatDynObjRef<> resolveGeneric(DynObjRef ref) {
    return store.resolve(ref.as<ObjRef<Symbol>>());
  }
  bool exists(ObjRef<Symbol> ref) { return store.exists(ref); }
  auto numIDs() { return store.numIDs(); }
  void reset() {
    store.reset();
    map.clear();
  }
  void destroy(SymbolRef symb) { return store.destroy(symb); }
  auto begin() { return store.begin(); }
  auto end() { return store.end(); }
};

/*

ctx a {
  EXPORT %my_mod:module
}

ctx b {
  IMPORT %my_mod
}

*/

/*

# IMPORT/EXPORT
```
// symbol uses hash-deduped store so auto-associated when in same store.
EXPORT :symbol("my_symbol"), %abc
EXPORT %my_symbol:symbol, %abc    // (equivalent, useful when symbol also used
in local parser)

IMPORT %ext_abc:objref, symbol("my_symbol")
// def'd value is up to type:
//  full shadow copy (for simple type like register)
//  non-owning pointer to original object with new ID
//  objref wrapper opaque object
```

Usage:
  - fully different contexts (e.g. concurrently parsed files)
    -> import/export resolve pass after contexts are merged
  - separate contexts but shared symbol store (or symbol dialect context)
    - useful for in-memory runtime splitting like for multithreading
    - can still reference other context's obj read-only via pointer (usually,
but up to type)
  - same context
    - IMPORT/EXPORT semantics for smth like yield values?

*/
}; // namespace dyno
