#pragma once
#include "support/MacroUtil.h"
#include "support/Ranges.h"
#include "support/SmallVec.h"
#include <algorithm>
#include <cstring>
#include <iostream>
#include <ostream>

#if DYNO_ENABLE_DEBUG
#define DYNO_DBG_RUN(x)                                                        \
  do {                                                                         \
    x;                                                                         \
  } while (0);

#define _DYNO_DBG_1(x)                                                         \
  do {                                                                         \
    if (dbg_is_enabled_for_id(debugID)) {                                      \
      if (dyno::lastDebugID != debugID)                                        \
        dyno::dbgs() << "====" << debugName << "====\n";                       \
      lastDebugID = debugID;                                                   \
      do {                                                                     \
        x;                                                                     \
      } while (0);                                                             \
    }                                                                          \
  } while (0);

#define _DYNO_DBG_2(name, x)                                                   \
  do {                                                                         \
    if (dbg_is_enabled_for_id(debugID)) {                                      \
      dyno::dbgs() << "====" << name << "====\n";                              \
      lastDebugID = -1;                                                        \
      do {                                                                     \
        x;                                                                     \
      } while (0);                                                             \
    }                                                                          \
  } while (0);

#define DYNO_DBG(...) DYNO_VA_MACRO(_DYNO_DBG, __VA_ARGS__)
#else
#define DYNO_DBG(...)
#define DYNO_DBG_RUN(...)
#endif

#define DYNO_DBGV(...) DYNO_DBG(__VA_ARGS__)
#define DEBUG(...) static_assert(false, "Use DYNO_DBG")

namespace dyno {

constexpr uint32_t debugID = 0;
constexpr const char *debugName = "GlobalDebug";
inline uint8_t _debugEnable[2048] = {1};
inline uint32_t lastDebugID = 0;
// 0 is default ID, 1-255 are custom.
//
// IDs after 256 are auto-assigned to passes,
// 256-511 is first pass for all 256 dialects,
// 512-767 is second pass for all 256 dialects,
// ...
// (this is so passes only need a fixed order within a dialect)
inline void dbg_enable_for_id(uint32_t id) {
  _debugEnable[id / 8] |= 1 << (id % 8);
}
inline void dbg_disable_for_id(uint32_t id) {
  _debugEnable[id / 8] &= ~(1 << (id % 8));
}
inline bool dbg_is_enabled_for_id(uint32_t id) {
  return _debugEnable[id / 8] & (1 << (id % 8));
}
inline void dbg_enable_all() {
  memset(reinterpret_cast<uint8_t *>(_debugEnable), 255, sizeof(_debugEnable));
}
inline void dbg_disable_all() {
  memset(reinterpret_cast<uint8_t *>(_debugEnable), 0, sizeof(_debugEnable));
}

inline std::ostream &dbgs() {
  // todo: wrapper around ostream.
  return std::cerr;
}

} // namespace dyno
template <typename It> std::ostream &operator<<(std::ostream &os, Range<It> r) {
  bool first = true;
  for (auto &&val : r) {
    if (!first) {
      os << ", ";
    }
    first = false;
    os << val;
  }
  return os;
}
template <typename T>
inline std::ostream &operator<<(std::ostream &os, const SmallVecImpl<T> &vec) {
  return os << Range{vec};
}
template <typename T, size_t N>
inline std::ostream &operator<<(std::ostream &os, const std::array<T, N> &vec) {
  return os << Range{vec};
}

template <typename It> struct std::formatter<Range<It>> {
  std::string sep = ", ";
  std::string deco = "";

  template <typename _Out>
  constexpr typename basic_format_context<_Out, char>::iterator
  format(const Range<It> &u, basic_format_context<_Out, char> &fc) const {
    auto out = fc.out();
    for (auto [back, elem] : u.mark_back())
      out = std::format_to(out, "{}{}{}{}", deco, elem, deco, back ? "" : sep);
    return out;
  }
  // {:<sep>:<deco>}
  template <typename _In>
  constexpr typename basic_format_parse_context<_In>::iterator
  parse(basic_format_parse_context<_In> &ctx) {
    auto sectStart = ctx.begin();
    auto end = Range{ctx}.find('}');
    uint32_t i = 0;
    auto strings = std::to_array({&sep, &deco});
    while (true) {
      auto sectEnd = Range{sectStart, end}.find(':');

      if (sectEnd != sectStart)
        *strings.at(i) = {sectStart, sectEnd};

      if (sectEnd == end)
        break;
      sectStart = std::next(sectEnd);
      ++i;
    }
    return end;
  }
};
