#pragma once
#include <iostream>
#include <ostream>

inline uint64_t debugType = 0;

#ifdef _DEBUG_
#define DEBUG(x)                                                               \
  do {                                                                         \
    if (debugType) {                                                           \
      x                                                                        \
    }                                                                          \
  } while (0);
#else
#define DEBUG(x)
#endif

inline std::ostream &dbgs() {
  // todo: wrapper around ostream.
  return std::cerr;
}
