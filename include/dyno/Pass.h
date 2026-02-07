#pragma once
#include "dyno/Obj.h"
#include "dyno/Opcode.h"
#include "dyno/Parser.h"
#include "dyno/Type.h"
#include "support/ArrayRef.h"
#include "support/ErrorRecovery.h"
#include "support/TemplateUtil.h"
#include "support/Tokenizer.h"
#include <charconv>
#include <map>
#include <string_view>
#include <strings.h>
#include <tuple>
#include <type_traits>
#include <utility>

namespace dyno {
// clang-format off
#define CONFIG_STRUCT_TYPES(lambda) \
    lambda(bool, BOOL) \
    lambda(uint8_t, U8) \
    lambda(int8_t, I8) \
    lambda(uint16_t, U16) \
    lambda(int16_t, I16) \
    lambda(uint32_t, U32) \
    lambda(int32_t, I32) \
    lambda(uint64_t, U64) \
    lambda(int64_t, I64) \
    lambda(const char*, STRING_CHAR_PTR) \
    lambda(ArrayRef<char>, STRING_REF) \
    lambda(std::string_view, STRING_VIEW) \
    lambda(std::string, STD_STRING) \
    lambda(dyno::DialectOpcode, DIALECT_OPCODE) \
    lambda(dyno::DialectType, DIALECT_TYPE) \
    lambda(dyno::DialectID, DIALECT)
// clang-format on

enum class ConfigStructType {
#define LAMBDA(_, enum_entry) enum_entry,
  CONFIG_STRUCT_TYPES(LAMBDA)
#undef LAMBDA
};

template <ConfigStructType T> struct ConfigStructEnumToType {};
#define LAMBDA(type, enum_entry)                                               \
  template <> struct ConfigStructEnumToType<ConfigStructType::enum_entry> {    \
    using t = type;                                                            \
  };

CONFIG_STRUCT_TYPES(LAMBDA)
#undef LAMBDA

template <typename T> struct ConfigStructTypeToEnum {};
#define LAMBDA(type, enum_entry)                                               \
  template <> struct ConfigStructTypeToEnum<type> {                            \
    static constexpr ConfigStructType v{ConfigStructType::enum_entry};         \
  };
CONFIG_STRUCT_TYPES(LAMBDA)
#undef LAMBDA

#define CONFIG_EXPAND_MEMBER(type, name, defaultV) type name{defaultV};
#define CONFIG_EXPAND_ENUM_MEMBER(name, defaultV, ...)                         \
  enum { __VA_ARGS__ } name{defaultV};

#define CONFIG_EXPAND_CALL(type, name, defaultV)                               \
  Func(reinterpret_cast<void *>(&this->name), ConfigStructTypeToEnum<type>::v, \
       #name);

#define STR_IMPL(x) #x
#define STR(x) STR_IMPL(x)
#define QUOTE_1(x) STR(x)
#define QUOTE_2(x, ...) STR(x), QUOTE_1(__VA_ARGS__)
#define QUOTE_3(x, ...) STR(x), QUOTE_2(__VA_ARGS__)
#define QUOTE_4(x, ...) STR(x), QUOTE_3(__VA_ARGS__)
#define QUOTE_5(x, ...) STR(x), QUOTE_4(__VA_ARGS__)
#define QUOTE_6(x, ...) STR(x), QUOTE_5(__VA_ARGS__)
#define QUOTE_7(x, ...) STR(x), QUOTE_6(__VA_ARGS__)
#define QUOTE_8(x, ...) STR(x), QUOTE_7(__VA_ARGS__)
#define GET_NTH_ARG(_1, _2, _3, _4, _5, _6, _7, _8, N, ...) N
#define COUNT_ARGS(...) GET_NTH_ARG(__VA_ARGS__, 8, 7, 6, 5, 4, 3, 2, 1)
#define GLUE_IMPL(x, y) x##y
#define GLUE(x, y) GLUE_IMPL(x, y)

// quote each element in list
#define QUOTE_LIST(...) GLUE(QUOTE_, COUNT_ARGS(__VA_ARGS__))(__VA_ARGS__)

#define CONFIG_EXPAND_ENUM_CALL(name, defaultV, ...)                           \
  EnumFunc(reinterpret_cast<void *>(&this->name), #name,                       \
           std::vector<const char *>{QUOTE_LIST(__VA_ARGS__)});

#define CONFIG_STRUCT(lambda)                                                  \
  struct Config {                                                              \
    lambda(CONFIG_EXPAND_MEMBER,                                               \
           CONFIG_EXPAND_ENUM_MEMBER) void for_fields(auto &&Func,             \
                                                      auto &&EnumFunc) {       \
      lambda(CONFIG_EXPAND_CALL, CONFIG_EXPAND_ENUM_CALL)                      \
    }                                                                          \
  };

// maybe at some point we'll have dyno wrappers for all these types
// and can move this parsing into the regular dyno parser. For now
// pass args are just std::map<string, string> and we parse everything
// here.

struct ConfigParser {
  DynoLexer &lexer;
  inline bool parseConfigType(void *ptr, ConfigStructType ty,
                              std::string_view data) {
    switch (ty) {
    case ConfigStructType::BOOL: {
      if (data == "true" || data == "1")
        *reinterpret_cast<bool *>(ptr) = true;
      else if (data == "false" || data == "0")
        *reinterpret_cast<bool *>(ptr) = false;
      else
        return false;
      break;
    }
#define PARSE_NUMERIC(type)                                                    \
  case type: {                                                                 \
    auto [end, ec] = std::from_chars(                                          \
        data.begin(), data.end(),                                              \
        *reinterpret_cast<ConfigStructEnumToType<type>::t *>(ptr));            \
    if (end != data.end() || ec != std::errc{})                                \
      return false;                                                            \
    break;                                                                     \
  }
      PARSE_NUMERIC(ConfigStructType::U8)
      PARSE_NUMERIC(ConfigStructType::I8)
      PARSE_NUMERIC(ConfigStructType::U16)
      PARSE_NUMERIC(ConfigStructType::I16)
      PARSE_NUMERIC(ConfigStructType::U32)
      PARSE_NUMERIC(ConfigStructType::I32)
      PARSE_NUMERIC(ConfigStructType::U64)
      PARSE_NUMERIC(ConfigStructType::I64)
#undef PARSE_NUMERIC
    case ConfigStructType::STRING_CHAR_PTR: {
      *reinterpret_cast<const char **>(ptr) = data.data();
      break;
    }
    case ConfigStructType::STRING_REF: {
      *reinterpret_cast<ArrayRef<char> *>(ptr) = ArrayRef<char>{data};
      break;
    }
    case ConfigStructType::STRING_VIEW: {
      *reinterpret_cast<std::string_view *>(ptr) = data;
      break;
    }
    case ConfigStructType::STD_STRING: {
      *reinterpret_cast<std::string *>(ptr) = data;
      break;
    }
    case ConfigStructType::DIALECT_OPCODE: {
      lexer.reset(data);
      auto opc = lexer.tryPopOpcode();
      if (!opc)
        return false;
      *reinterpret_cast<DialectOpcode *>(ptr) = *opc;
      break;
    }
    case ConfigStructType::DIALECT_TYPE: {
      lexer.reset(data);
      auto type = lexer.tryPopType();
      if (!type)
        return false;
      *reinterpret_cast<DialectType *>(ptr) = *type;
      break;
    }
    case ConfigStructType::DIALECT: {
      lexer.reset(data);
      auto dialect = lexer.tryPopDialect();
      if (!dialect)
        return false;
      *reinterpret_cast<DialectID *>(ptr) = *dialect;
    } break;
    default:
      return false;
    }

    return true;
  }
};

template <typename Derived> class Pass {
  static constexpr std::string_view getName() {
    for (auto t : Tokenizer{__PRETTY_FUNCTION__, ": <>"}) {
      // there's probably better logic, but format is different between
      // compilers
      if (t.length() > 4 && t.ends_with("Pass"))
        return std::string_view(t);
    }
    assert(0 && "pass class name must end with \"Pass\"");
  }

  template <typename Out, typename In> static Out castArg(In in) {
    if constexpr (std::is_pointer_v<In> && std::is_pointer_v<Out>) {
      return reinterpret_cast<Out>(in);
    } else if constexpr (std::is_pointer_v<In> && std::is_reference_v<Out>) {
      return *reinterpret_cast<std::remove_reference_t<Out> *>(in);
    }
    assert(0 && "no conversion");
  }

  template <typename T, std::size_t... Is>
  static auto createArgTuple(ArrayRef<void *> values,
                             std::index_sequence<Is...>) {
    return std::tuple<typename std::tuple_element<Is, T>::type...>{
        castArg<typename std::tuple_element<Is, T>::type>(values[Is])...};
  }

public:
  // construct with type-erased args.
  static void *typeErasedConstruct(ArrayRef<void *> args) {
    // make is only used to find out constructor arguments. with reflection we
    // can get rid of it.
    using arg_tuple = function_args_t<decltype(&Derived::make)>;
    constexpr auto sz = std::tuple_size_v<arg_tuple>;
    auto argTuple =
        createArgTuple<arg_tuple>(args, std::make_index_sequence<sz>{});
    return new Derived(std::make_from_tuple<Derived>(std::move(argTuple)));
  }

  static bool typeErasedRun(void *self, ArrayRef<void *> args) {
    using arg_tuple = function_args_t<decltype(&Derived::runGenericPtr)>;
    constexpr auto sz = std::tuple_size_v<arg_tuple>;
    auto argTuple =
        createArgTuple<arg_tuple>(args, std::make_index_sequence<sz>{});
    return std::apply(&BindMethod<&Pass::runGenericPtr>::fv,
                      std::tuple_cat(std::make_tuple(self), argTuple));
  }

  static void typeErasedDestroy(void *obj) {
    delete reinterpret_cast<Derived *>(obj);
  }

  static void typeErasedConfig(void *selfPtr,
                               std::map<std::string, std::string> &config,
                               DynoLexer &lexer) {
    if constexpr (requires(Derived &d) {
                    d.config.for_fields(
                        [](void *, ConfigStructType, const char *) {},
                        [](void *, const char *, std::vector<const char *>) {});
                  }) {
      auto &self = *reinterpret_cast<Derived *>(selfPtr);
      // reset to default config
      self.config = typename Derived::Config{};

      ConfigParser parser{lexer};
      self.config.for_fields(
          [&](void *ptr, ConfigStructType ty, const char *nm) {
            auto it = config.find(std::string(nm));
            if (it == config.end())
              return;
            if (!parser.parseConfigType(ptr, ty, it->second))
              report_fatal_error("invalid setting {}: {}", nm, it->second);
          },
          [&](void *ptr, const char *nm, std::vector<const char *> labels) {
            auto it = config.find(std::string(nm));
            if (it == config.end())
              return;
            auto it2 = Range{labels}.find_if(
                [&](const char *elem) { return it->second == elem; });
            if (it2 == labels.end())
              report_fatal_error("invalid setting {}: {}", nm, it->second);
            auto idx = it2 - labels.begin();
            *reinterpret_cast<int *>(ptr) = idx;
          });
    }
  }

private:
  template <typename T> bool tryRun(FatDynObjRef<> ref, T func) {
    auto &self = *reinterpret_cast<Derived *>(this);

    if constexpr (std::tuple_size_v<function_args_t<T>> == 0) {
      if (ref)
        return false;
      (self.*func)();
      return true;
    } else {
      static_assert(std::tuple_size_v<function_args_t<T>> == 1,
                    "expected 0 or 1 arg function");
      using arg_t = std::tuple_element_t<0, function_args_t<T>>;
      if (auto conv = ref.dyn_as<arg_t>()) {
        (self.*func)(conv);
        return true;
      }
      return false;
    }
  }

public:
  bool runGeneric(FatDynObjRef<> ref) {
    auto &self = *reinterpret_cast<Derived *>(this);
    if constexpr (requires { self.runFuncs; }) {
      return std::apply(
          [&](auto &&...funcs) { return (tryRun(ref, funcs) || ...); },
          self.runFuncs);
    } else if (!ref) {
      self.run();
      return true;
    }
    return false;
  }

  bool runGenericPtr(FatDynObjRef<> *ref) { return runGeneric(*ref); }

  static constexpr std::string_view passName = getName();
};
}; // namespace dyno
