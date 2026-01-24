#pragma once

#include "dyno/DialectInfo.h"
#include "dyno/Opcode.h"
#include "dyno/Parser.h"
#include "support/SlabAllocator.h"
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
  using run_fn = void(void *, ArrayRef<void *>);
  using config_fn = void(void *, std::map<std::string, std::string> &,
                         DynoLexer &);

  create_fn *create;
  destroy_fn *destroy;
  run_fn *run;
  config_fn *config;
};

struct TypeErasedPassObj {
  TypeErasedPass *fns = nullptr;
  void *obj = nullptr;

  TypeErasedPassObj(TypeErasedPass *fns, ArrayRef<void *> args) : fns(fns) {
    obj = fns->create(args);
  }
  ~TypeErasedPassObj() {
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
  void run(ArrayRef<void *> args) { fns->run(obj, args); }

  void config(std::map<std::string, std::string> &config, DynoLexer &lexer) {
    fns->config(obj, config, lexer);
  }
};
struct MetaPassManager {

  std::vector<OpcodeInfo> metaOpcodeInfoArr;
  SlabAllocator<TypeErasedPass> typeErasedPasses;
  MixedSizeSlabAllocator<> stringAlloc;

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

  template <typename T> void registerPass() {
    metaOpcodeInfoArr.emplace_back(processString(T::passName));
    typeErasedPasses.emplace_back(T::typeErasedConstruct, T::typeErasedDestroy,
                                  T::typeErasedRun, T::typeErasedConfig);
  }

  TypeErasedPassObj getPass(DialectOpcode opc, ArrayRef<void *> args) {
    assert(opc.getDialectID() == DIALECT_META);
    return TypeErasedPassObj{&typeErasedPasses[opc.getOpcodeID()], args};
  }
};

template <DialectID D> void registerDialectPasses(MetaPassManager &) {}

}; // namespace dyno
