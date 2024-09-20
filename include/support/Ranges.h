#pragma once

#include <iterator>
#include <ranges>

template <typename T> class earlyincr_iterator {
public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = T::value_type;
  using pointer = T::pointer;
  using reference = T::reference;
  using difference_type = T::difference_type;

  earlyincr_iterator() = default;
  earlyincr_iterator(T it) : it(it) {}

  reference operator*() {
    reference ref = *it;
    ++it;
    return ref;
  }

  earlyincr_iterator &operator++() { return *this; }

  earlyincr_iterator operator++(int) { return *this; }

  friend bool operator==(const earlyincr_iterator &a,
                         const earlyincr_iterator &b) {
    return a.it == b.it;
  }

  friend bool operator!=(const earlyincr_iterator &a,
                         const earlyincr_iterator &b) {
    return a.it != b.it;
  }

private:
  T it;
};

inline auto make_earlyincr_range(std::ranges::forward_range auto &&rg) {
  return std::ranges::subrange{earlyincr_iterator{rg.begin()},
                               earlyincr_iterator{rg.end()}};
}

template <typename T> class deref_iterator {
public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = T::value_type;
  using pointer = T::pointer;
  using reference = T::reference;
  using difference_type = T::difference_type;

  deref_iterator() = default;
  deref_iterator(T it) : it(it) {}

  reference operator*() { return **it; }

  deref_iterator &operator++() {
    ++it;
    return *this;
  }

  deref_iterator operator++(int) {
    deref_iterator tmp(*this);
    ++(*this);
    return tmp;
  }

  friend bool operator==(const deref_iterator &a, const deref_iterator &b) {
    return a.it == b.it;
  }

  friend bool operator!=(const deref_iterator &a, const deref_iterator &b) {
    return a.it != b.it;
  }

private:
  T it;
};
