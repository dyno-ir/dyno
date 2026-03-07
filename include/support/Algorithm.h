#pragma once

#include <algorithm>
template <typename T, typename Pred>
T exp_search(T zero, T begin, T end, Pred pred) {

  T prev = begin;

  while (!pred(*begin)) {
    prev = begin;
    begin = zero + (begin - zero) * 2;
    if (begin >= end)
      return end;
  }

  return std::lower_bound(prev, begin, 0,
                          [&](auto &&v, int) { return !pred(v); });
}

template <typename T, typename Pred>
auto exp_search(Range<T> universe, Range<T> searchSpace, Pred pred) {
  return exp_search(universe.begin(), searchSpace.begin(), searchSpace.end(),
                    pred);
}
