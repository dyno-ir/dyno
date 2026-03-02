#pragma once

#include <tuple>
template <typename... Vals> struct Any {

  std::tuple<Vals &&...> tuple;
  constexpr Any(Vals &&...args) : tuple(std::forward<Vals>(args)...) {}

  template <typename T> constexpr bool operator==(T&& other) {
    return std::apply([&](auto &&...vals) { return ((other == vals) || ...); },
                      tuple);
  }
};

template <typename... Args> Any(Args &&...args) -> Any<Args...>;
