#pragma once

#include <cassert>

[[noreturn]] inline void unreachable() { __builtin_unreachable(); }

#define UNREACHABLE(msg)                                                       \
  assert(false && msg);                                                        \
  unreachable();

template <typename...> inline constexpr bool dependent_false_v = false;

struct none_t {};
