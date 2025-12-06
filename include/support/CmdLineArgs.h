#pragma once
#include "support/ArrayRef.h"
#include "support/Bits.h"
#include "support/ErrorRecovery.h"
#include "support/SmallVec.h"
#include <array>
#include <cctype>
#include <charconv>
#include <cstdint>
#include <iostream>
#include <sstream>
#include <string>
#include <string_view>
#include <system_error>
#include <unordered_map>

// todo:
// - multiple arguments for regular options like -a=1,2,3
//   (currently only supported for positional options)
// - subcommand wrapper around CmdLineArgHandler.

struct CmdLineArgFlags {
  enum : uint32_t {
    MANDATORY = 1,
    POSITIONAL = 2,
    HIDDEN = 4,
    VALUE_REQUIRED = 8,
    NO_VALUE = 16,
    MULTIPLE = 32,
  };
};

class CmdLineArgBase;
class CmdLineArgHandler;

class CmdLineArgBase {
  friend class CmdLineArgHandler;

  std::optional<char> shortName;
  ArrayRef<char> longName;
  ArrayRef<char> description;

  uint32_t flags;
  auto mandatory() { return BitField<uint32_t, 1, 0>{flags}; }
  auto positional() { return BitField<uint32_t, 1, 1>{flags}; }
  auto hidden() { return BitField<uint32_t, 1, 2>{flags}; }
  auto valueRequired() { return BitField<uint32_t, 1, 3>{flags}; }
  auto noValue() { return BitField<uint32_t, 1, 4>{flags}; }
  auto multiple() { return BitField<uint32_t, 1, 5>{flags}; }

  using ParseFunc = void(CmdLineArgBase *, const char *);

  ParseFunc *parse;

public:
  CmdLineArgBase(std::optional<char> shortName, ArrayRef<char> longName,
                 ArrayRef<char> description, uint32_t flags, ParseFunc *parse)
      : shortName(shortName), longName(longName), description(description),
        flags(flags), parse(parse) {}
};

template <typename T> class CmdLineArg : public CmdLineArgBase {
  T value;

public:
  CmdLineArg(std::optional<char> shortName, ArrayRef<char> longName,
             ArrayRef<char> description, uint32_t flags)
      : CmdLineArgBase{shortName, longName, description, flags, parse} {}
  CmdLineArg(std::optional<char> shortName, ArrayRef<char> longName,
             ArrayRef<char> description, uint32_t flags, const T &initialValue)
      : CmdLineArgBase{shortName, longName, description, flags, parse},
        value(initialValue) {}
  const T &operator*() const { return value; }
  const T *operator->() const { return &value; }
  static void parse(CmdLineArgBase *, const char *);
};

// Parse specializations
struct CmdLineHelpObj {
  CmdLineArgHandler *parent;
};

template <>
inline void CmdLineArg<CmdLineHelpObj>::parse(CmdLineArgBase *self,
                                              const char *ptr);

class CmdLineArgHandler {
private:
  std::array<CmdLineArgBase *, 256> shortArgMap = {};
  std::unordered_map<std::string_view, CmdLineArgBase *> longArgMap;
  SmallVec<CmdLineArgBase *, 4> positionalArgs;
  SmallVec<CmdLineArgBase *, 128> allArgs;

  // registered as -h, --help, "parsing" this arg just prints help.
  // We need a ref to the handler to print help though.
  CmdLineArg<CmdLineHelpObj> helpArg{'h', "help", "Print help.", 0,
                                     CmdLineHelpObj{this}};

public:
  const char *executableName = "<executable>";

  void registerArg(CmdLineArgBase &c) {
    if (c.shortName) {
      auto &slot = shortArgMap[*c.shortName];
      if (slot != nullptr)
        report_fatal_error(
            "multiple arguments registered for: -{}: \"{}\" and \"{}\"",
            *c.shortName, slot->longName.data(), c.longName.data());
      slot = &c;
    }

    if (!c.longName.empty()) {
      auto &slot = longArgMap[std::string_view{c.longName.begin(),
                                               c.longName.end() - 1}];
      if (slot != nullptr)
        report_fatal_error(
            "multiple arguments registered with long name \"{}\"",
            c.longName.data());
      slot = &c;
    }

    allArgs.emplace_back(&c);
    if (c.positional())
      positionalArgs.emplace_back(&c);
  }

  void printHelpExit(bool isError = true) {
    std::ostream &os = isError ? std::cerr : std::cout;
    std::print(os, "usage: {}", executableName);

    if (allArgs.size() != positionalArgs.size())
      std::print(os, " [options]");

    for (auto &arg : Range{positionalArgs}.deref()) {
      std::print(os, " <{}>", arg.longName.data());
      if (!(arg.flags & CmdLineArgFlags::MANDATORY))
        std::print(os, "?");
      if (arg.flags & CmdLineArgFlags::MULTIPLE)
        std::print(os, "...");
    }

    std::print(os, "\n\noptions:\n");
    for (auto &arg : Range{allArgs}.deref()) {
      if (arg.flags & CmdLineArgFlags::POSITIONAL)
        continue;
      std::stringstream buf;
      std::print(buf, "  ");
      if (arg.shortName) {
        std::print(buf, "-{}", *arg.shortName);
        if (arg.flags & CmdLineArgFlags::VALUE_REQUIRED)
          std::print(buf, "=<value>");
        if (!arg.longName.empty())
          std::print(buf, ", ");
      }
      if (!arg.longName.empty()) {
        std::print(buf, "--{}", arg.longName.data());
        if (arg.flags & CmdLineArgFlags::VALUE_REQUIRED)
          std::print(buf, "=<value>");
        std::print(buf, " ");
      }

      auto str = buf.str();
      std::print(os, "{}", str);
      if (str.length() > 26) {
        std::print(os, "\n");
        str.clear();
      }
      for (unsigned i = str.length(); i < 26; i++)
        std::print(os, " ");
      std::print(os, "{}\n", arg.description.data());
    }

    exit(isError ? -1 : 0);
  }

  void parseLong(char **it, bool &argsEnd) {
    const char *end = *it + 2;
    while (isalnum(*end) || *end == '-' || *end == '_')
      ++end;
    std::string_view nm{*it + 2, end};
    if (nm.empty()) {
      if (*end == 0) {
        argsEnd = true;
        return;
      }
      printHelpExit();
    }
    auto mapIt = longArgMap.find(nm);
    if (mapIt == longArgMap.end())
      report_fatal_error("unknown argument: --{}", nm);
    auto &c = *mapIt->second;

    const char *arg;

    if (*end != 0) {
      // --flag=ARG
      if (*end != '=')
        report_fatal_error("expected \'=\'");
      end++;
      arg = end;
      if (c.noValue())
        report_fatal_error("flag --{} does not accept an argument",
                           c.longName.data());
    } else {
      // --flag ARG
      auto next = it + 1;
      if (*next && (*next)[0] != '-') {
        arg = *next;
        it = next;
      } else if (c.valueRequired())
        report_fatal_error("flag --{} requires an argument", c.longName.data());
    }
    c.parse(&c, arg);
  }

  void parseShort(char **it) {
    // short

    CmdLineArgBase *c;
    uint offset = 1;

    // loop for multiple flags. terminate when flag has required arg or
    // next isn't valid flag.
    while (true) {
      char nm = (*it)[offset];
      auto mapIt = shortArgMap[nm];
      if (!mapIt) {
        if (offset == 1) {
          // first char has to be a valid arg
          if (!isprint(nm))
            report_fatal_error("unknown argument");
          report_fatal_error("unknown argument: -{}", nm);
        }
        break;
      }
      // optionless flags (all but last)
      if (offset != 1)
        c->parse(c, nullptr);
      offset++;
      c = mapIt;
      // value required -> no additional options
      if (c->valueRequired())
        break;
    }

    const char *arg = nullptr;

    if ((*it)[offset] != 0) {
      // -aARG or -a=ARG
      if ((*it)[offset] == '=')
        arg = &(*it)[offset + 1];
      else
        arg = &(*it)[offset];
    } else {
      // -a ARG
      auto next = it + 1;
      if (*next && (*next)[0] != '-') {
        arg = *next;
        it = next;
      } else if (c->valueRequired())
        report_fatal_error("flag -{} requires an argument", *c->shortName);
    }
    if (c->noValue() && arg)
      report_fatal_error("flag -{} does not take an argument", *c->shortName);
    c->parse(c, arg);
  }

  void parsePositional(CmdLineArgBase &c, char *arg) { c.parse(&c, arg); }

  void parse(int argc, char **argv) {
    if (argc < 1)
      printHelpExit();
    executableName = argv[0];
    auto args = MutArrayRef<char *>{argv, size_t(argc)}.drop_front();
    bool argsEnd = false;

    size_t positionalIdx = 0;

    for (auto it = args.begin(); it != args.end(); ++it) {
      if ((*it)[0] == '-' && !argsEnd) {
        if (((*it)[1]) == '-')
          parseLong(it, argsEnd);
        else
          parseShort(it);
      } else {
        if (positionalIdx >= positionalArgs.size())
          printHelpExit();
        parsePositional(*positionalArgs[positionalIdx], *it);
        if (!positionalArgs[positionalIdx]->multiple())
          positionalIdx++;
      }
    }

    if (positionalIdx != positionalArgs.size())
      printHelpExit();
  }

  CmdLineArgHandler() { registerArg(helpArg); }
};

template <>
inline void CmdLineArg<CmdLineHelpObj>::parse(CmdLineArgBase *self,
                                              const char *ptr) {
  static_cast<CmdLineArg<CmdLineHelpObj> *>(self)
      ->value.parent->printHelpExit();
}
template <>
inline void CmdLineArg<ArrayRef<char>>::parse(CmdLineArgBase *self,
                                              const char *ptr) {
  static_cast<CmdLineArg *>(self)->value = std::string_view{ptr};
}

#define PARSE_STRING(TYPE)                                                     \
  template <>                                                                  \
  inline void CmdLineArg<TYPE>::parse(CmdLineArgBase *self, const char *ptr) { \
    static_cast<CmdLineArg *>(self)->value = ptr;                              \
  }                                                                            \
  template <>                                                                  \
  inline void CmdLineArg<std::vector<TYPE>>::parse(CmdLineArgBase *self,       \
                                                   const char *ptr) {          \
    static_cast<CmdLineArg *>(self)->value.emplace_back(ptr);                  \
  }

PARSE_STRING(std::string_view)
PARSE_STRING(std::string)
PARSE_STRING(const char *)

template <>
inline void CmdLineArg<bool>::parse(CmdLineArgBase *self, const char *ptr) {
  if (ptr == nullptr)
    // flag specified without argument sets it.
    static_cast<CmdLineArg *>(self)->value = true;
  else {
    if (strcasecmp(ptr, "1") || strcasecmp(ptr, "y") ||
        strcasecmp(ptr, "yes") || strcasecmp(ptr, "true"))
      static_cast<CmdLineArg *>(self)->value = true;
    else if (strcasecmp(ptr, "0") || strcasecmp(ptr, "n") ||
             strcasecmp(ptr, "no") || strcasecmp(ptr, "false"))
      static_cast<CmdLineArg *>(self)->value = false;
    else
      report_fatal_error("invalid bool value: {}", ptr);
  }
}

// can't specialize a method with a new template param, so preprocessor
#define PARSE_NUMERIC(TYPE)                                                    \
  template <>                                                                  \
  inline void CmdLineArg<TYPE>::parse(CmdLineArgBase *self, const char *ptr) { \
    std::string_view str{ptr};                                                 \
    auto [end, ec] = std::from_chars(str.begin(), str.end(),                   \
                                     static_cast<CmdLineArg *>(self)->value);  \
    if (ec != std::errc{} || ptr != str.end())                                 \
      report_fatal_error("expected number (" #TYPE "): {}", str);              \
  }                                                                            \
  template <>                                                                  \
  inline void CmdLineArg<std::vector<TYPE>>::parse(CmdLineArgBase *self,       \
                                                   const char *ptr) {          \
    std::string_view str{ptr};                                                 \
    TYPE val;                                                                  \
    auto [end, ec] = std::from_chars(str.begin(), str.end(), val);             \
    if (ec != std::errc{} || ptr != str.end())                                 \
      report_fatal_error("expected number (" #TYPE "): {}", str);              \
    static_cast<CmdLineArg *>(self)->value.emplace_back(val);                  \
  }

PARSE_NUMERIC(uint8_t)
PARSE_NUMERIC(int8_t)
PARSE_NUMERIC(uint16_t)
PARSE_NUMERIC(int16_t)
PARSE_NUMERIC(uint32_t)
PARSE_NUMERIC(int32_t)
PARSE_NUMERIC(uint64_t)
PARSE_NUMERIC(int64_t)