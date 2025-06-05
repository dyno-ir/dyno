#pragma once
#include <iostream>
#include <ostream>

inline uint64_t debugType = 1;

#ifdef _DEBUG_
#define DEBUG(category, x)                                                     \
  do {                                                                         \
    if (debugType) {                                                           \
                                                                               \
      dbgs() << "====" << category << "====\n";                                \
      do {                                                                     \
        x                                                                      \
      } while (0);                                                             \
    }                                                                          \
  } while (0);
#else
#define DEBUG(x, y)
#endif

inline std::ostream &dbgs() {
  // todo: wrapper around ostream.
  return std::cerr;
}
