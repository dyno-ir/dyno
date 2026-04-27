#pragma once

#define _DYNO_CAT(A, B) A##B
#define DYNO_CAT(A, B) _DYNO_CAT(A, B)

#define _DYNO_STR(x) #x
#define DYNO_STR(x) _DYNO_STR(x)
#define DYNO_QUOTE_1(x) DYNO_STR(x)
#define DYNO_QUOTE_2(x, ...) DYNO_STR(x), DYNO_QUOTE_1(__VA_ARGS__)
#define DYNO_QUOTE_3(x, ...) DYNO_STR(x), DYNO_QUOTE_2(__VA_ARGS__)
#define DYNO_QUOTE_4(x, ...) DYNO_STR(x), DYNO_QUOTE_3(__VA_ARGS__)
#define DYNO_QUOTE_5(x, ...) DYNO_STR(x), DYNO_QUOTE_4(__VA_ARGS__)
#define DYNO_QUOTE_6(x, ...) DYNO_STR(x), DYNO_QUOTE_5(__VA_ARGS__)
#define DYNO_QUOTE_7(x, ...) DYNO_STR(x), DYNO_QUOTE_6(__VA_ARGS__)
#define DYNO_QUOTE_8(x, ...) DYNO_STR(x), DYNO_QUOTE_7(__VA_ARGS__)

#define _DYNO_VA_COUNT(_1, _2, _3, _4, _5, _6, _7, _8, N, ...) N
#define DYNO_VA_COUNT(...) _DYNO_VA_COUNT(__VA_ARGS__, 8, 7, 6, 5, 4, 3, 2, 1)

#define DYNO_VA_MACRO(NAME, ...)                                               \
  DYNO_CAT(NAME##_, DYNO_VA_COUNT(__VA_ARGS__))(__VA_ARGS__)

// quote each element in list
#define DYNO_QUOTE_LIST(...) DYNO_VA_MACRO(DYNO_QUOTE, __VA_ARGS__)
