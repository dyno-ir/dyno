#pragma once
#include <utility>


template <typename First, typename... Rest>
constexpr auto getFirst(First &&first, Rest &&...) {
  return std::forward<First>(first);
}
