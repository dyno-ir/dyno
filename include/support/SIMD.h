#pragma once

#include "support/TemplateUtil.h"
#include <bit>
#include <cstddef>
#include <utility>
#ifdef __AVX512F__
constexpr inline size_t SIMD_WIDTH = 64;
#elifdef __AVX__
constexpr inline size_t SIMD_WIDTH = 32;
#else
constexpr inline size_t SIMD_WIDTH = 16;
#endif

template <typename T> struct vector_derived {
  static constexpr size_t width = sizeof(T) / sizeof(std::declval<T>()[0]);
  using mask_t = bool __attribute__((ext_vector_type(width)));
  using mask_unsigned_t = uint_of_size<sizeof(mask_t)>::type;
};

template <typename T> using vec_mask_t = vector_derived<T>::mask_t;
template <typename T>
using vec_mask_unsigned_t = vector_derived<T>::mask_unsigned_t;

template <typename T> vec_mask_t<T> inline vec_to_maskreg(T vec) {
  vec_mask_t<T> matchMask = __builtin_convertvector(vec, vec_mask_t<T>);
  return matchMask;
}

// SIMD vector to mask register, every element converted to boolean (ie nonzero
// is 1)
template <typename T> vec_mask_unsigned_t<T> inline vec_to_bitmask(T vec) {
  vec_mask_t<T> matchMask = vec_to_maskreg(vec);
  auto match = std::bit_cast<vec_mask_unsigned_t<T>>(matchMask);
  return match;
}
