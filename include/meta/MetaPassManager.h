#pragma once

#include "dyno/DialectInfo.h"
#include "dyno/Interface.h"
#include "dyno/Lexer.h"
#include "dyno/Opcode.h"
#include "support/Debug.h"
#include "support/SlabAllocator.h"
#include "support/TwoLevelSet.h"
#include <cctype>
#include <cstring>
#include <map>
#include <optional>
#include <sstream>
#include <string_view>

namespace dyno {

struct TypeErasedPass {
  using create_fn = void *(ArrayRef<void *>);
  using destroy_fn = void(void *);
  using run_fn = bool(void *, ArrayRef<void *>);
  using config_fn = void(void *, std::map<std::string, std::string> &,
                         DynoLexer &);

  create_fn *create;
  destroy_fn *destroy;
  run_fn *run;
  config_fn *config;
};

struct TypeErasedPassObj {
  friend struct DenseMapInfo<dyno::TypeErasedPassObj>;

  TypeErasedPass *fns = nullptr;
  void *obj = nullptr;

  TypeErasedPassObj(TypeErasedPass *fns, ArrayRef<void *> args) : fns(fns) {
    obj = fns->create(args);
  }
  constexpr ~TypeErasedPassObj() {
    // constexpr if uninitialized, for Optional
    if (obj)
      fns->destroy(obj);
  }

  TypeErasedPassObj() = default;

  TypeErasedPassObj(const TypeErasedPassObj &) = delete;
  TypeErasedPassObj(TypeErasedPassObj &&o) : fns(o.fns), obj(o.obj) {
    o.obj = nullptr;
  }
  TypeErasedPassObj &operator=(const TypeErasedPassObj &) = delete;
  TypeErasedPassObj &operator=(TypeErasedPassObj &&o) {
    this->fns = o.fns;
    this->obj = o.obj;
    o.obj = nullptr;
    return *this;
  }
  bool run(ArrayRef<void *> args) { return fns->run(obj, args); }

  void config(std::map<std::string, std::string> &config, DynoLexer &lexer) {
    fns->config(obj, config, lexer);
  }

  auto operator<=>(const TypeErasedPassObj &other) const = default;

private:
  constexpr TypeErasedPassObj(TypeErasedPass *fns, void *obj)
      : fns(fns), obj(obj) {}
};

struct PassRegistry {
  Vec<OpcodeInfo> metaOpcodeInfoArr;
  Vec<TypeErasedPass> typeErasedPasses;
  MixedSizeSlabAllocator<> stringAlloc;
#ifdef DYNO_ENABLE_DEBUG
  Vec<uint16_t, MAX_NUM_DIALECTS> dialectPassIDCounter = (MAX_NUM_DIALECTS);
  Vec<uint32_t> opcodeDebugIDs;
#endif
  char *processString(std::string_view view) {
    std::stringstream str;

    bool lastUpper = false;
    auto it = view.begin();
    while (it != view.end()) {
      auto c = *it;
      std::optional<char> next = std::nullopt;
      if (it != view.end() - 1)
        next = *(it + 1);
      bool upper = std::isupper(c);
      if (!lastUpper && upper && it != view.begin())
        str << '_';
      else if (lastUpper && upper && next && !std::isupper(*next))
        str << '_';
      str << (char)std::toupper(c);

      lastUpper = upper;
      ++it;
    }

    auto v = str.view();
    char *ptr = reinterpret_cast<char *>(
        stringAlloc.resolve(stringAlloc.allocate_nbytes(v.size() + 1)));
    memcpy(ptr, v.data(), v.size());
    ptr[v.size()] = '\0';
    return ptr;
  }

  template <typename T> void registerPass(DialectID dialect) {
#ifdef DYNO_ENABLE_DEBUG
    auto id = (++dialectPassIDCounter[dialect]) * MAX_NUM_DIALECTS + dialect;
    assert((T::passID == 0 || T::passID == id) &&
           "different orders for passes in same dialect");
    T::passID = id;
    opcodeDebugIDs.emplace_back(id);
#endif
    metaOpcodeInfoArr.emplace_back(processString(T::passName));
    typeErasedPasses.emplace_back(T::typeErasedConstruct, T::typeErasedDestroy,
                                  T::typeErasedRun, T::typeErasedConfig);
  }
  // requires Pass::dialect.
  template <typename T> void registerPass() {
#ifdef DYNO_ENABLE_DEBUG
    auto id =
        ++dialectPassIDCounter[T::dialect] * MAX_NUM_DIALECTS + T::dialect;
    assert((T::passID == 0 || T::passID == id) &&
           "different orders for passes in same dialect");
    T::passID = id;
    opcodeDebugIDs.emplace_back(id);
#endif
    metaOpcodeInfoArr.emplace_back(processString(T::passName));
    typeErasedPasses.emplace_back(T::typeErasedConstruct, T::typeErasedDestroy,
                                  T::typeErasedRun, T::typeErasedConfig);
  }

#ifdef DYNO_ENABLE_DEBUG
  // note: This changes global debug state (_debugEnable). It does not
  // matter what instance of PassRegistry this is run on.
  void setDebugEnForPasses(ArrayRef<StringRef> passes, bool en) {
    TwoLevelSet<StringRef> passesMap(Range{passes});
    for (auto [info, id] : Range{metaOpcodeInfoArr}.zip(opcodeDebugIDs)) {
      if (passesMap.contains(info.name)) {
        if (en)
          dbg_enable_for_id(id);
        else
          dbg_disable_for_id(id);
      }
    }
  }
#endif

  TypeErasedPassObj constructPass(DialectOpcode opc, ArrayRef<void *> args) {
    assert(opc.getDialectID() == DIALECT_META);
    return TypeErasedPassObj{&typeErasedPasses[opc.getOpcodeID()], args};
  }
};

template <DialectID D> void registerDialectPasses(PassRegistry &) {}

}; // namespace dyno

template <> struct DenseMapInfo<dyno::TypeErasedPassObj> {
private:
  using T = dyno::TypeErasedPassObj;

public:
  static constexpr T getEmptyKey() {
    return dyno::TypeErasedPassObj{nullptr, nullptr};
  }
  // static constexpr T getTombstoneKey() { }
  // static unsigned getHashValue(const T &k) { }
  // static bool isEqual(const T &lhs, const T &rhs) { }
};
