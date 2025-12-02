#pragma once

#include "support/MacroUtil.h"
#include "support/Ranges.h"
#include <iostream>
#include <ostream>

#if DYNO_ENABLE_DEBUG
#define _DYNO_DBG_1(x)                                                         \
  do {                                                                         \
    if (dyno::debugType) {                                                     \
      do {                                                                     \
        x;                                                                     \
      } while (0);                                                             \
    }                                                                          \
  } while (0);
#define _DYNO_DBG_2(category, x)                                               \
  do {                                                                         \
    if (dyno::debugType) {                                                     \
      dyno::dbgs() << "====" << category << "====\n";                          \
      do {                                                                     \
        x;                                                                     \
      } while (0);                                                             \
    }                                                                          \
  } while (0);
#define DYNO_DBG(...) DYNO_VA_MACRO(_DYNO_DBG, __VA_ARGS__)
#else
#define DYNO_DBG(...)
#endif

#define DYNO_DBGV(...) DYNO_DBG(__VA_ARGS__)
#define DEBUG(...) static_assert(false, "Use DYNO_DBG")

namespace dyno {

inline uint64_t debugType = 1;

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
