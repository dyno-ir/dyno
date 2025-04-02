#pragma once

#include <cassert>

[[noreturn]] inline void unreachable() { __builtin_unreachable(); }

#define dyno_unreachable(msg)                                                  \
  do {                                                                         \
    assert(false && (msg));                                                    \
    unreachable();                                                             \
  } while (0);

template <typename...> inline constexpr bool dependent_false_v = false;

struct none_t {};
