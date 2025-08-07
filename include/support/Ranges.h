#pragma once

#include <algorithm>
#include <initializer_list>
#include <iterator>
#include <type_traits>

template <typename It> class Range;

template <typename T> class earlyincr_iterator {
public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = std::iterator_traits<T>::value_type;
  using pointer = std::iterator_traits<T>::pointer;
  using reference = std::iterator_traits<T>::reference;
  using difference_type = std::iterator_traits<T>::difference_type;

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
  T it;

public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = std::remove_reference_t<decltype(**it)>;
  using reference = value_type &;
  using difference_type = std::iterator_traits<T>::difference_type;

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
  using difference_type = std::iterator_traits<T>::difference_type;

  transform_iterator()
    requires std::is_default_constructible_v<TransformT>
  = default;

  transform_iterator(T it)
    requires std::is_default_constructible_v<TransformT>
      : it(it) {}

  transform_iterator(T it, TransformT transformF)
      : it(it), i(0), transformF(transformF) {}

  auto operator*() { return transformF(i, *it); }

  transform_iterator &operator++() {
    ++it;
    ++i;
    return *this;
  }

  transform_iterator operator++(int) {
    transform_iterator tmp(*this);
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
  using difference_type = std::iterator_traits<T>::difference_type;

  enumerate_iterator() = default;
  enumerate_iterator(T it) : it(it), i(0) {}

  value_type operator*() { return {i, *it}; }

  enumerate_iterator &operator++() {
    ++it;
    ++i;
    return *this;
  }

  enumerate_iterator operator++(int) {
    enumerate_iterator tmp(*this);
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

template <typename T> class mark_back_iterator {
  T it;
  T end;

public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = std::pair<bool, decltype(*it)>;
  using pointer = value_type *;
  using reference = value_type &;
  using difference_type = std::iterator_traits<T>::difference_type;

  mark_back_iterator() = default;
  mark_back_iterator(T it, T end) : it(it), end(end) {}

  value_type operator*() { return {std::next(it) == end, *it}; }

  mark_back_iterator &operator++() {
    ++it;
    return *this;
  }

  mark_back_iterator operator++(int) {
    mark_back_iterator tmp(*this);
    ++(*this);
    return tmp;
  }

  friend bool operator==(const mark_back_iterator &a,
                         const mark_back_iterator &b) {
    return a.it == b.it;
  }

  friend bool operator!=(const mark_back_iterator &a,
                         const mark_back_iterator &b) {
    return a.it != b.it;
  }
};

template <typename T> class mark_front_iterator {
  T it;
  bool first;

public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = std::pair<bool, decltype(*it)>;
  using pointer = value_type *;
  using reference = value_type &;
  using difference_type = std::iterator_traits<T>::difference_type;

  mark_front_iterator() = default;
  mark_front_iterator(T it, bool first) : it(it), first(first) {}

  value_type operator*() { return {first, *it}; }

  mark_front_iterator &operator++() {
    first = false;
    ++it;
    return *this;
  }

  mark_front_iterator operator++(int) {
    mark_back_iterator tmp(*this);
    ++(*this);
    return tmp;
  }

  friend bool operator==(const mark_front_iterator &a,
                         const mark_front_iterator &b) {
    return a.it == b.it;
  }

  friend bool operator!=(const mark_front_iterator &a,
                         const mark_front_iterator &b) {
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
  using value_type = std::iterator_traits<T>::value_type;
  using pointer = std::iterator_traits<T>::pointer;
  using reference = std::iterator_traits<T>::reference;
  using difference_type = std::iterator_traits<T>::difference_type;

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
  using value_type = std::iterator_traits<T>::value_type;
  using pointer = std::iterator_traits<T>::pointer;
  using reference = std::iterator_traits<T>::reference;
  using difference_type = std::iterator_traits<T>::difference_type;

  discard_optional_iterator() = default;
  discard_optional_iterator(T it, T itEnd) : it(it), itEnd(itEnd) { prime(); }
  discard_optional_iterator(T it) : it(it), itEnd(it) {}

  auto operator*() {
    assert(*it && "base iterator's operator* is not idempotent");
    return **it;
  }

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

  template <typename U> Range(const U &u) : Range(u.begin(), u.end()) {}
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

  auto reverse() {
    return ::Range{std::reverse_iterator<It>{end()},
                   std::reverse_iterator<It>{begin()}};
  }

  auto drop_front() {
    auto rv = Range{*this};
    ++rv.beginIt;
    return rv;
  }
  auto drop_back() {
    auto rv = Range{*this};
    --rv.endIt;
    return rv;
  }

  auto mark_front() {
    return ::Range{mark_front_iterator{beginIt, true},
                   mark_front_iterator{endIt, false}};
  }
  auto mark_back() {
    return ::Range{mark_back_iterator{beginIt, endIt},
                   mark_back_iterator{endIt, endIt}};
  }

  template <typename TransformT> auto transform(TransformT transformF) {
    return ::Range{transform_iterator<It, TransformT>(beginIt, transformF),
                   transform_iterator<It, TransformT>(endIt, transformF)};
  }

  template <typename T> auto as() {
    auto lambda = [](size_t, const auto &src) { return src.template as<T>(); };
    return ::Range{transform_iterator<It, decltype(lambda)>(beginIt, lambda),
                   transform_iterator<It, decltype(lambda)>(endIt)};
  }

  template <typename FilterT> auto filter(FilterT filterF) {
    return ::Range{filter_iterator<It, FilterT>(beginIt, endIt, filterF),
                   filter_iterator<It, FilterT>(endIt)};
  }

  template <typename T> void sort(T func) { std::sort(begin(), end(), func); }
  template <typename T> It find(const T &val) {
    return std::find(begin(), end(), val);
  }
  template <typename T> It find_if(T func) {
    return std::find_if(begin(), end(), func);
  }

private:
  It beginIt, endIt;
};

template <typename U> Range(U &u) -> Range<decltype(u.begin())>;
template <typename U> Range(const U &u) -> Range<decltype(u.begin())>;

template <typename T>
class InitListRange
    : public Range<typename std::initializer_list<T>::iterator> {
public:
  InitListRange(std::initializer_list<T> ilist)
      : Range<typename std::initializer_list<T>::iterator>(ilist.begin(),
                                                           ilist.end()) {}
};

template <typename It> class RefRange {
public:
  using iterator = It;
  using value_type = std::iter_value_t<iterator>;
  using reference = value_type &;
  using pointer = value_type *;

  RefRange(It &beginIt, It &endIt) : beginIt(beginIt), endIt(endIt) {}
  It begin() { return beginIt; }
  It end() { return endIt; }

  RefRange &operator=(Range<It> other) {
    beginIt = other.begin();
    endIt = other.end();
    return *this;
  }

  RefRange &operator=(reference other) {
    beginIt = &other;
    endIt = &other + 1;
    return *this;
  }

  auto drop_front() {
    auto rv = RefRange{*this};
    ++rv.beginIt;
    return rv;
  }
  auto drop_back() {
    auto rv = RefRange{*this};
    --rv.endIt;
    return rv;
  }

private:
  It &beginIt;
  It &endIt;
};

template <typename T>
concept IsRange = (requires {
  typename T::iterator;
} && std::is_same_v<std::remove_cv_t<T>, Range<typename T::iterator>>);
