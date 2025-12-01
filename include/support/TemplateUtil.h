#pragma once
#include <cstdint>
#include <type_traits>
#include <utility>

template <typename First, typename... Rest>
constexpr auto getFirst(First &&first, Rest &&...) {
  return std::forward<First>(first);
}

template <typename First, typename... Rest> struct get_first {
  using t = First;
};

template <std::size_t Bytes> struct uint_of_size;

template <> struct uint_of_size<1> {
  using type = uint8_t;
};
template <> struct uint_of_size<2> {
  using type = uint16_t;
};
template <> struct uint_of_size<4> {
  using type = uint32_t;
};
template <> struct uint_of_size<8> {
  using type = uint64_t;
};
template <> struct uint_of_size<16> {
  using type = __uint128_t;
};

template <class T, class... U>
concept any_of = std::disjunction_v<std::is_convertible<T, U>...>;
