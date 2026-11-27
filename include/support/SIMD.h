#pragma once

#include "support/Bits.h"
#include "support/TemplateUtil.h"
#include <bit>
#include <cstddef>
#include <utility>

#ifdef __AVX512BW__
#define SIMD_SUPPORTS_MASKED_MEM
#endif

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

template <typename T>
using vec_elem_t = std::remove_cvref_t<decltype(std::declval<T>()[0])>;

template <typename T> inline T vec_load(const vec_elem_t<T> *ptr) {
  T rv;
  __builtin_memcpy(&rv, ptr, sizeof(rv));
  return rv;
}

template <typename T> vec_mask_t<T> inline vec_to_maskreg(T vec) {
  vec_mask_t<T> matchMask = __builtin_convertvector(vec, vec_mask_t<T>);
  return matchMask;
}
template <typename T>
inline T vec_masked_load(const vec_elem_t<T> *ptr,
                         vec_mask_unsigned_t<T> mask) {
#ifdef SIMD_SUPPORTS_MASKED_MEM
  return __builtin_masked_load(std::bit_cast<vec_mask_t<T>>(mask), ptr);
#else
  if (mask == bit_mask_ones<vec_mask_unsigned_t<T>>())
    return vec_load<T>(ptr);
  return __builtin_masked_load(std::bit_cast<vec_mask_t<T>>(mask), ptr);
#endif
}
template <typename T>
inline T vec_masked_load(const vec_elem_t<T> *ptr, vec_mask_unsigned_t<T> mask,
                         T fill) {
#ifdef SIMD_SUPPORTS_MASKED_MEM
  return __builtin_masked_load(std::bit_cast<vec_mask_t<T>>(mask), ptr, fill);
#else
  if (mask == bit_mask_ones<vec_mask_unsigned_t<T>>())
    return vec_load<T>(ptr);
  return __builtin_masked_load(std::bit_cast<vec_mask_t<T>>(mask), ptr, fill);
#endif
}

// SIMD vector to mask register, every element converted to boolean (ie nonzero
// is 1)
template <typename T> vec_mask_unsigned_t<T> inline vec_to_bitmask(T vec) {
  vec_mask_t<T> matchMask = vec_to_maskreg(vec);
  auto match = std::bit_cast<vec_mask_unsigned_t<T>>(matchMask);
  return match;
}

template <typename T>
inline T vec_select(vec_mask_unsigned_t<T> mask, T a, T b) {
  vec_mask_t<T> m = std::bit_cast<vec_mask_t<T>>(mask);
  return m ? a : b;
}

template <size_t Shift, typename T, size_t... I>
inline T vec_shift_right_impl(T v, std::index_sequence<I...>) {
  return __builtin_shufflevector(v, v, (I < Shift ? 0 : I - Shift)...);
}

template <size_t Shift, typename T>
inline T vec_shift_right(T v) {
  return vec_shift_right_impl<Shift, T>(
      v, std::make_index_sequence<vector_derived<T>::width>{});
}

template <size_t Shift, typename T, size_t... I>
inline T vec_shift_left_impl(T v, std::index_sequence<I...>) {
  constexpr size_t width = vector_derived<T>::width;
  return __builtin_shufflevector(v, T(0),
                                 (I + Shift < width ? I + Shift : width)...);
}

template <size_t Shift, typename T>
inline T vec_shift_left(T v) {
  return vec_shift_left_impl<Shift, T>(
      v, std::make_index_sequence<vector_derived<T>::width>{});
}

template <typename T> struct vec_wrapper {
  using elem_t = vec_elem_t<T>;
  T vec;
  vec_wrapper() = default;
  vec_wrapper(elem_t elem) : vec(elem) {}
  vec_wrapper(T vec) : vec(vec) {}

#define FOR_EWISE(x)                                                           \
  x(==) x(!=) x(<) x(<=) x(>) x(>=) x(+) x(-) x(*) x(/) x(%) x(<<) x(>>)       \
      x(&) x(|) x(^)

#define OPERATOR(op)                                                           \
  vec_wrapper operator op(const vec_wrapper &o) const {                        \
    return T(vec op o.vec);                                                    \
  }
  FOR_EWISE(OPERATOR)
#undef OPERATOR

#define FOR_REDUCE(x) x(and) x(or) x(xor) x(add) x(mul) x(maximum) x(minimum)

#define REDUCTION_OPERATOR(fn)                                                 \
  elem_t reduce_##fn() const { return __builtin_reduce_##fn(vec); }
  FOR_REDUCE(REDUCTION_OPERATOR)
#undef REDUCTION_OPERATOR

  explicit operator bool() const { return reduce_or(); }
  elem_t operator[](size_t i) const { return vec[i]; }

  static constexpr size_t size() { return vector_derived<T>::width; }
  T &raw() { return vec; }
  const T &raw() const { return vec; }

  static vec_wrapper load(const elem_t *ptr) {
    return vec_wrapper(vec_load<T>(ptr));
  }
  static vec_wrapper load(const elem_t *ptr, vec_mask_unsigned_t<T> mask) {
    return vec_masked_load<T>(ptr, mask);
  }
  static vec_wrapper load(const elem_t *ptr, vec_mask_unsigned_t<T> mask,
                          vec_wrapper fill) {
    return vec_masked_load<T>(ptr, mask, fill.vec);
  }
  static vec_wrapper select(vec_mask_unsigned_t<T> mask, vec_wrapper a,
                            vec_wrapper b) {
    return vec_wrapper(vec_select<T>(mask, a.vec, b.vec));
  }
  vec_mask_unsigned_t<T> mask() const { return vec_to_bitmask(vec); }
  vec_mask_unsigned_t<T> first_nonzero_lane() const {
    return std::countr_zero(vec_to_bitmask(vec));
  }
};

template <typename T>
inline void vec_store(vec_elem_t<T> *ptr, T v) {
  __builtin_memcpy(ptr, &v, sizeof(v));
}

template <typename T>
inline void vec_masked_store(vec_elem_t<T> *ptr,
                             vec_mask_unsigned_t<T> mask, T v) {
#ifdef SIMD_SUPPORTS_MASKED_MEM
  __builtin_masked_store(std::bit_cast<vec_mask_t<T>>(mask), v, ptr);
#else
  if (mask == bit_mask_ones<vec_mask_unsigned_t<T>>()) [[likely]] {
    vec_store<T>(ptr, v);
  } else {
    for (size_t i = 0; i < vector_derived<T>::width; i++)
      if (mask & (vec_mask_unsigned_t<T>(1) << i))
        ptr[i] = v[i];
  }
#endif
}

template <typename T>
inline void vec_compress_store(vec_elem_t<T> *ptr,
                               vec_mask_unsigned_t<T> mask, T v) {
  __builtin_masked_compress_store(std::bit_cast<vec_mask_t<T>>(mask), v, ptr);
}

template <typename T>
inline T vec_expand_load(const vec_elem_t<T> *ptr,
                         vec_mask_unsigned_t<T> mask) {
  vec_mask_t<T> m = std::bit_cast<vec_mask_t<T>>(mask);
  T expanded = __builtin_masked_expand_load(m, ptr);
  return m ? expanded : T(0);
}

template <typename T>
inline T vec_compress(T v, vec_mask_unsigned_t<T> mask) {
  vec_elem_t<T> buf[vector_derived<T>::width] = {};
  vec_compress_store<T>(buf, mask, v);
  return vec_load<T>(buf);
}

template <typename T>
inline T vec_expand(T v, vec_mask_unsigned_t<T> mask) {
  vec_elem_t<T> buf[vector_derived<T>::width];
  vec_store<T>(buf, v);
  return vec_expand_load<T>(buf, mask);
}

template <typename T>
struct vector_derived<vec_wrapper<T>> : public vector_derived<T> {};

#define FOR_VEC_TS(x)                                                          \
  x(u8, uint8_t) x(u16, uint16_t) x(u32, uint32_t) x(u64, uint64_t)            \
      x(i8, int8_t) x(i16, int16_t) x(i32, int32_t) x(i64, int64_t)            \
          x(char, char)
#define TYPE_ALIAS(short, ty)                                                  \
  using vec_##short##_raw_t =                                                  \
      ty [[clang::ext_vector_type(SIMD_WIDTH / sizeof(ty))]];                  \
  using vec_##short##_t = vec_wrapper<vec_##short##_raw_t>;
FOR_VEC_TS(TYPE_ALIAS)
#undef TYPE_ALIAS
#undef FOR_VEC_TS
