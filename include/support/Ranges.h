#pragma once

#include <iterator>

template <typename It> class Range;

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
  return Range{earlyincr_iterator{rg.begin()}, earlyincr_iterator{rg.end()}};
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

template <typename T, typename TransformT> class transform_iterator {
  T it;
  size_t i;
  TransformT transformF;

public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = decltype(transformF(i, *it));
  using pointer = value_type *;
  using reference = value_type &;
  using difference_type = T::difference_type;

  transform_iterator() = default;
  transform_iterator(T it) : it(it) {}
  transform_iterator(T it, TransformT transformF)
      : it(it), i(0), transformF(transformF) {}

  auto operator*() { return transformF(i, *it); }

  transform_iterator &operator++() {
    ++it;
    ++i;
    return *this;
  }

  transform_iterator operator++(int) {
    deref_iterator tmp(*this);
    ++(*this);
    return tmp;
  }

  friend bool operator==(const transform_iterator &a,
                         const transform_iterator &b) {
    return a.it == b.it;
  }

  friend bool operator!=(const transform_iterator &a,
                         const transform_iterator &b) {
    return a.it != b.it;
  }
};

template <typename T> class enumerate_iterator {
  T it;
  size_t i;

public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = std::pair<size_t, decltype(*it)>;
  using pointer = value_type *;
  using reference = value_type &;
  using difference_type = T::difference_type;

  enumerate_iterator() = default;
  enumerate_iterator(T it) : it(it), i(0) {}

  value_type operator*() { return {i, *it}; }

  enumerate_iterator &operator++() {
    ++it;
    ++i;
    return *this;
  }

  enumerate_iterator operator++(int) {
    deref_iterator tmp(*this);
    ++(*this);
    return tmp;
  }

  friend bool operator==(const enumerate_iterator &a,
                         const enumerate_iterator &b) {
    return a.it == b.it;
  }

  friend bool operator!=(const enumerate_iterator &a,
                         const enumerate_iterator &b) {
    return a.it != b.it;
  }
};

template <typename T, typename FilterT> class filter_iterator {
  T it;
  T itEnd;
  FilterT filterF;

  void prime() {
    while (it != itEnd && !filterF(*it)) {
      ++it;
    }
  }

public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = decltype(*it);
  using pointer = value_type *;
  using reference = value_type &;
  using difference_type = T::difference_type;

  filter_iterator() = default;
  filter_iterator(T it, T itEnd, FilterT filterF)
      : it(it), itEnd(itEnd), filterF(filterF) {
    prime();
  }
  filter_iterator(T it) : it(it) {}

  auto operator*() { return *it; }

  filter_iterator &operator++() {
    ++it;
    prime();
    return *this;
  }

  filter_iterator operator++(int) {
    filter_iterator tmp(*this);
    ++(*this);
    return tmp;
  }

  friend bool operator==(const filter_iterator &a, const filter_iterator &b) {
    return a.it == b.it;
  }

  friend bool operator!=(const filter_iterator &a, const filter_iterator &b) {
    return a.it != b.it;
  }
};

template <typename T> class discard_optional_iterator {
  T it;
  T itEnd;

  void prime() {
    while (it != itEnd && !*it) {
      ++it;
    }
  }

public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = decltype(*it);
  using pointer = value_type *;
  using reference = value_type &;
  using difference_type = T::difference_type;

  discard_optional_iterator() = default;
  discard_optional_iterator(T it, T itEnd) : it(it), itEnd(itEnd) { prime(); }
  discard_optional_iterator(T it) : it(it) {}

  auto operator*() { return **it; }

  discard_optional_iterator &operator++() {
    ++it;
    prime();
    return *this;
  }

  discard_optional_iterator operator++(int) {
    discard_optional_iterator tmp(*this);
    ++(*this);
    return tmp;
  }

  friend bool operator==(const discard_optional_iterator &a,
                         const discard_optional_iterator &b) {
    return a.it == b.it;
  }

  friend bool operator!=(const discard_optional_iterator &a,
                         const discard_optional_iterator &b) {
    return a.it != b.it;
  }
};

template <typename It> class Range {
public:
  using iterator = It;

  template <typename U> Range(U &u) : Range(u.begin(), u.end()) {}
  Range(It beginIt, It endIt) : beginIt(beginIt), endIt(endIt) {}

  It begin() const { return beginIt; }
  It end() const { return endIt; }

  auto earlyincr() {
    return ::Range{earlyincr_iterator{beginIt}, earlyincr_iterator{endIt}};
  }

  auto deref() {
    return ::Range{deref_iterator{beginIt}, deref_iterator{endIt}};
  }

  auto enumerate() {
    return ::Range{enumerate_iterator{beginIt}, enumerate_iterator{endIt}};
  }

  auto discard_optional() {
    return ::Range{discard_optional_iterator{beginIt, endIt},
                   discard_optional_iterator{endIt}};
  }

  template <typename TransformT> auto transform(TransformT transformF) {
    return ::Range{transform_iterator<It, TransformT>(beginIt, transformF),
                   transform_iterator<It, TransformT>(endIt)};
  }

  template <typename FilterT> auto filter(FilterT filterF) {
    return ::Range{filter_iterator<It, FilterT>(beginIt, endIt, filterF),
                   filter_iterator<It, FilterT>(endIt)};
  }

private:
  It beginIt, endIt;
};

template <typename U> Range(U &u) -> Range<decltype(u.begin())>;
