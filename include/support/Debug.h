#pragma once
#include "support/MacroUtil.h"
#include "support/Ranges.h"
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
  _debugEnable[id / 8] |= ~(1 << (id % 8));
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

template <typename It> std::ostream &operator<<(std::ostream &os, Range<It> r) {
  os << '[';
  bool first = true;
  for (auto &&val : r) {
    if (!first) {
      os << ", ";
    }
    first = false;
    os << val;
  }
  os << ']';
  return os;
}
} // namespace dyno
