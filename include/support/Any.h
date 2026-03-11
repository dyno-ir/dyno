#pragma once

#include "support/Tuple.h"
template <typename... Vals> struct Any {

  Tuple<Vals &&...> tuple;
  constexpr Any(Vals &&...args) : tuple(std::forward<Vals>(args)...) {}

  template <typename T> constexpr bool operator==(T &&other) {
    return tuple.apply(
        [&](auto &&...vals) { return ((other == vals) || ...); });
  }
};

template <typename... Args> Any(Args &&...args) -> Any<Args...>;
