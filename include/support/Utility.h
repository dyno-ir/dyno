#pragma once

#include <cassert>

[[noreturn]] inline void unreachable() { __builtin_unreachable(); }

#define UNREACHABLE(msg)                                                       \
  assert(false && msg);                                                        \
  unreachable();

template <typename...> inline constexpr bool dependent_false_v = false;

template <typename It> class Range {
public:
  template <typename U> Range(U &o) : Range(o.begin(), o.end()) {}
  Range(It beginIt, It endIt) : beginIt(beginIt), endIt(endIt) {}

  It begin() const { return beginIt; }
  It end() const { return endIt; }

private:
  It beginIt, endIt;
};
