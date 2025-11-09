#pragma once

#define _DYNO_CAT(A, B) A##B
#define DYNO_CAT(A, B) _DYNO_CAT(A, B)
#define _DYNO_VA_COUNT(_4, _3, _2, _1, COUNT, ...) COUNT
#define DYNO_VA_COUNT(...) _DYNO_VA_COUNT(__VA_ARGS__, 4, 3, 2, 1)
#define DYNO_VA_MACRO(NAME, ...)                                               \
  DYNO_CAT(NAME##_, DYNO_VA_COUNT(__VA_ARGS__))(__VA_ARGS__)
