#pragma once

#include "support/Bits.h"
#include "support/Tuple.h"
#include <algorithm>
#include <cassert>
#include <concepts>
#include <initializer_list>
#include <iterator>
#include <numeric>
#include <optional>
#include <type_traits>
#include <utility>

template <typename It> class Range;

// Iterator boilerplate
// - Forward: in base implement operator++(), operator==
// - Bidir:   in base implement operator++(), operator--(), operator==
// - Random:  in base implement operator+=(), operator-(), operator<=>
template <typename derived, typename iterator_category,
          typename difference_type>
class base_iterator {
private:
  static constexpr bool isRandom =
      std::is_same_v<iterator_category, std::random_access_iterator_tag>;
  static constexpr bool isBidir =
      std::is_same_v<iterator_category, std::bidirectional_iterator_tag> ||
      isRandom;

  derived &self() { return *static_cast<derived *>(this); }
  const derived &cself() const { return *static_cast<const derived *>(this); }

  struct Empty {};
  using difference_type_safe =
      std::conditional_t<isRandom, difference_type, Empty>;

public:
  derived &operator++()
    requires(isRandom)
  {
    self() += 1;
    return self();
  }

  derived operator++(int) {
    derived tmp(self());
    ++(self());
    return tmp;
  }

  derived &operator--()
    requires(isRandom)
  {
    (self()) -= 1;
    return self();
  }

  derived operator--(int)
    requires(isBidir)
  {
    derived tmp(self());
    --(self());
    return tmp;
  }

  derived &operator-=(difference_type_safe n)
    requires(isRandom)
  {
    self() += -n;
    return self();
  }

  friend derived operator+(derived a, difference_type_safe n)
    requires(isRandom)
  {
    a += n;
    return a;
  }

  friend derived operator+(difference_type_safe n, derived a)
    requires(isRandom)
  {
    a += n;
    return a;
  }

  friend derived operator-(derived a, difference_type_safe n)
    requires(isRandom)
  {
    a -= n;
    return a;
  }

  decltype(auto) operator[](difference_type_safe n) const
    requires(isRandom)
  {
    derived tmp(cself());
    tmp += n;
    return *tmp;
  }

  friend bool operator!=(const derived &a, const derived &b) {
    return !(a == b);
  }
};

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

  reference operator*() const { return **it; }
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

template <typename T>
class no_deref_iterator
    : public base_iterator<no_deref_iterator<T>,
                           typename std::iterator_traits<T>::iterator_category,
                           typename std::iterator_traits<T>::difference_type> {
  T it;

public:
  using iterator_category = typename std::iterator_traits<T>::iterator_category;
  using value_type = T;
  using difference_type = typename std::iterator_traits<T>::difference_type;

private:
  static constexpr bool isRandom =
      std::is_same_v<iterator_category, std::random_access_iterator_tag>;
  static constexpr bool isBidir =
      std::is_same_v<iterator_category, std::bidirectional_iterator_tag> ||
      isRandom;

public:
  no_deref_iterator() = default;
  no_deref_iterator(T it) : it(it) {}

  value_type operator*() { return it; }

  no_deref_iterator &operator++() {
    ++it;
    return *this;
  }
  no_deref_iterator &operator--()
    requires(isBidir)
  {
    --it;
    return *this;
  }
  no_deref_iterator &
  operator+=(std::conditional_t<isRandom, difference_type, int> n)
    requires(isRandom)
  {
    it += n;
    return *this;
  }

  difference_type operator-(const no_deref_iterator &o)
    requires(isRandom)
  {
    return it - o.it;
  }

  friend bool operator==(const no_deref_iterator &a,
                         const no_deref_iterator &b) {
    return a.it == b.it;
  }

  friend bool operator!=(const no_deref_iterator &a,
                         const no_deref_iterator &b) {
    return a.it != b.it;
  }
  friend auto operator<=>(const no_deref_iterator &a,
                          const no_deref_iterator &b)
    requires(isRandom)
  {
    return a.it <=> b.it;
  }
};

template <std::integral T>
class integral_iterator : public base_iterator<integral_iterator<T>,
                                               std::random_access_iterator_tag,
                                               std::make_signed_t<T>> {
  T it;

public:
  using iterator_category = std::random_access_iterator_tag;
  using value_type = T;
  using difference_type = std::make_signed_t<T>;

public:
  integral_iterator() = default;
  integral_iterator(T it) : it(it) {}

  value_type operator*() { return it; }

  integral_iterator &operator+=(difference_type n) {
    it += n;
    return *this;
  }

  difference_type operator-(const integral_iterator &o) { return it - o.it; }

  friend auto operator==(const integral_iterator &a,
                         const integral_iterator &b) {
    return a.it == b.it;
  }
  friend auto operator<=>(const integral_iterator &a,
                          const integral_iterator &b) {
    return a.it <=> b.it;
  }
};

template <typename T, typename TransformT>
class transform_iterator
    : public base_iterator<transform_iterator<T, TransformT>,
                           typename std::iterator_traits<T>::iterator_category,
                           typename std::iterator_traits<T>::difference_type> {
  T it;
  size_t i;
  TransformT transformF;

public:
  using iterator_category = std::iterator_traits<T>::iterator_category;
  using value_type = decltype(transformF(i, *it));
  using pointer = value_type *;
  using reference = value_type &;
  using difference_type = std::iterator_traits<T>::difference_type;

private:
  static constexpr bool isRandom =
      std::is_same_v<iterator_category, std::random_access_iterator_tag>;
  static constexpr bool isBidir =
      std::is_same_v<iterator_category, std::bidirectional_iterator_tag> ||
      isRandom;

public:
  transform_iterator()
    requires std::is_default_constructible_v<TransformT>
  = default;

  transform_iterator(T it)
    requires std::is_default_constructible_v<TransformT>
      : it(it) {}

  transform_iterator(T it, TransformT transformF)
      : it(it), i(0), transformF(transformF) {}

  auto operator*() const { return transformF(i, *it); }
  auto operator*() { return transformF(i, *it); }

  transform_iterator &operator++() {
    ++it;
    ++i;
    return *this;
  }
  transform_iterator &operator--()
    requires(isBidir)
  {
    --it;
    --i;
    return *this;
  }
  transform_iterator &
  operator+=(std::conditional_t<isRandom, difference_type, int> n)
    requires(isRandom)
  {
    it += n;
    i += n;
    return *this;
  }

  difference_type operator-(const transform_iterator &o)
    requires(isRandom)
  {
    return it - o.it;
  }

  friend bool operator==(const transform_iterator &a,
                         const transform_iterator &b) {
    return a.it == b.it;
  }
  friend auto operator<=>(const transform_iterator &a,
                          const transform_iterator &b)
    requires(isRandom)
  {
    return a.it <=> b.it;
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

template <typename T, typename U> class zip_iterator {
  T it;
  U it2;

public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = std::pair<decltype(*it), decltype(*it2)>;
  using pointer = value_type *;
  using reference = value_type &;
  using difference_type = std::iterator_traits<T>::difference_type;

  zip_iterator() = default;
  zip_iterator(T it, U it2) : it(it), it2(it2) {}

  value_type operator*() {
    return std::pair<decltype(*it), decltype(*it2)>(*it, *it2);
  }

  zip_iterator &operator++() {
    ++it;
    ++it2;
    return *this;
  }

  zip_iterator operator++(int) {
    zip_iterator tmp(*this);
    ++(*this);
    return tmp;
  }

  friend bool operator==(const zip_iterator &a, const zip_iterator &b) {
    auto rv = a.it == b.it;
    assert(rv == (a.it2 == b.it2));
    return rv;
  }

  friend bool operator!=(const zip_iterator &a, const zip_iterator &b) {
    return !(a == b);
  }
};

template <typename T, typename U> class sorted_intersect_iterator {
  T it;
  U it2;

  T itEnd;
  U it2End;

  void prime() {
    while (it != itEnd && it2 != it2End) {
      if (*it == *it2) {
        break;
      } else if (*it < *it2) {
        ++it;
      } else
        ++it2;
    }
    if (it == itEnd)
      it2 == it2End;
    else if (it2 == it2End)
      it == itEnd;
  }

public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = std::iterator_traits<T>::value_type;
  using pointer = value_type *;
  using reference = value_type &;
  using difference_type = std::iterator_traits<T>::difference_type;

  sorted_intersect_iterator() = default;
  sorted_intersect_iterator(T itEnd, U it2End)
      : it(itEnd), it2(it2End), itEnd(itEnd), it2End(it2End) {}
  sorted_intersect_iterator(T it, T itEnd, U it2, U it2End)
      : it(it), it2(it2), itEnd(itEnd), it2End(it2End) {}

  value_type operator*() {
    auto rv = *it;
    assert(rv == *it2);
    return rv;
  }

  sorted_intersect_iterator &operator++() {
    ++it;
    ++it2;
    prime();
  }

  sorted_intersect_iterator operator++(int) {
    sorted_intersect_iterator tmp(*this);
    ++(*this);
    return tmp;
  }

  friend bool operator==(const sorted_intersect_iterator &a,
                         const sorted_intersect_iterator &b) {
    auto rv = a.it == b.it;
    assert(rv == (a.it2 == b.it2));
    return rv;
  }

  friend bool operator!=(const sorted_intersect_iterator &a,
                         const sorted_intersect_iterator &b) {
    return !(a == b);
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
  std::optional<FilterT> filterF;

  void prime() {
    while (it != itEnd && !(*filterF)(*it)) {
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
  filter_iterator(T it, T itEnd) : it(it), itEnd(itEnd) {}

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

template <typename T> class pairwise_iterator {
  T it;

public:
  using iterator_category = std::iterator_traits<T>::iterator_category;
  using underlying_value_type = std::iterator_traits<T>::value_type;
  using underlying_reference = std::iterator_traits<T>::reference;
  using underlying_pointer = std::iterator_traits<T>::pointer;

  using value_type = std::pair<underlying_value_type, underlying_value_type>;
  using reference = std::pair<underlying_reference, underlying_reference>;

  using pointer = underlying_pointer;
  using difference_type = std::iterator_traits<T>::difference_type;

  pairwise_iterator() = default;
  explicit pairwise_iterator(T it) : it(it) {}

  std::pair<decltype(*it), decltype(*it)> operator*() const {
    return {*it, *std::next(it)};
  }

  pairwise_iterator &operator++() {
    std::advance(it, 2);
    return *this;
  }

  pairwise_iterator operator++(int) {
    pairwise_iterator tmp(*this);
    ++(*this);
    return tmp;
  }

  friend bool operator==(const pairwise_iterator &a,
                         const pairwise_iterator &b) {
    return a.it == b.it;
  }

  friend bool operator!=(const pairwise_iterator &a,
                         const pairwise_iterator &b) {
    return a.it != b.it;
  }

  static constexpr bool isBidir =
      std::is_same_v<iterator_category, std::bidirectional_iterator_tag> ||
      std::is_same_v<iterator_category, std::random_access_iterator_tag>;

  pairwise_iterator &operator--()
    requires(isBidir)
  {
    std::advance(it, -2);
    return *this;
  }

  pairwise_iterator operator--(int)
    requires(isBidir)
  {
    pairwise_iterator tmp(*this);
    --(*this);
    return tmp;
  }

  static constexpr bool isRandom =
      std::is_same_v<iterator_category, std::random_access_iterator_tag>;

  pairwise_iterator &operator+=(difference_type n)
    requires(isRandom)
  {
    it += n * 2;
    return *this;
  }

  pairwise_iterator &operator-=(difference_type n)
    requires(isRandom)
  {
    it -= n * 2;
    return *this;
  }

  friend pairwise_iterator operator+(pairwise_iterator a, difference_type n)
    requires(isRandom)
  {
    a += n;
    return a;
  }

  friend pairwise_iterator operator+(difference_type n, pairwise_iterator a)
    requires(isRandom)
  {
    a += n;
    return a;
  }

  friend pairwise_iterator operator-(pairwise_iterator a, difference_type n)
    requires(isRandom)
  {
    a -= n;
    return a;
  }

  friend difference_type operator-(const pairwise_iterator &a,
                                   const pairwise_iterator &b)
    requires(isRandom)
  {
    return (b.it - a.it) / 2;
  }

  reference operator[](difference_type n) const
    requires(isRandom)
  {
    return *(*this + n);
  }

  friend auto operator<=>(const pairwise_iterator &a,
                          const pairwise_iterator &b)
    requires(isRandom)
  {
    return a.it <=> b.it;
  }
};

template <typename T>
class step_iterator
    : public base_iterator<step_iterator<T>,
                           typename std::iterator_traits<T>::iterator_category,
                           typename std::iterator_traits<T>::difference_type> {
  T it;
  unsigned n;

public:
  using iterator_category = std::iterator_traits<T>::iterator_category;
  using value_type = std::iterator_traits<T>::value_type;
  using pointer = value_type *;
  using reference = value_type &;
  using difference_type = std::iterator_traits<T>::difference_type;

private:
  static constexpr bool isRandom =
      std::is_same_v<iterator_category, std::random_access_iterator_tag>;
  static constexpr bool isBidir =
      std::is_same_v<iterator_category, std::bidirectional_iterator_tag> ||
      isRandom;

public:
  step_iterator() = default;
  step_iterator(T it, unsigned n) : it(it), n(n) {}

  value_type operator*() const { return *it; }

  step_iterator &operator+=(difference_type d)
    requires(isRandom)
  {
    it += n * d;
    return *this;
  }

  step_iterator &operator++() {
    std::advance(it, n);
    return *this;
  }

  difference_type operator-(step_iterator other)
    requires(isRandom)
  {
    assert(n == other.n);
    return (it - other.it) / n;
  }

  friend bool operator==(const step_iterator &a, const step_iterator &b) {
    assert(a.n == b.n);
    return a.it == b.it;
  }

  friend auto operator<=>(const step_iterator &a, const step_iterator &b)
    requires(isRandom)
  {
    return a.it <=> b.it;
  }
};

template <typename T, typename Seq> struct tuple_n_helper;
template <typename T, std::size_t... Is>
struct tuple_n_helper<T, std::index_sequence<Is...>> {
  template <std::size_t> using wrap = T;
  using type = std::tuple<wrap<Is>...>;
};
template <typename T, std::size_t N>
using tuple_n_t = typename tuple_n_helper<T, std::make_index_sequence<N>>::type;

template <unsigned N, typename T>
class tuple_iterator
    : public base_iterator<tuple_iterator<N, T>,
                           typename std::iterator_traits<T>::iterator_category,
                           typename std::iterator_traits<T>::difference_type> {
  T it;

public:
  using iterator_category = std::iterator_traits<T>::iterator_category;
  using value_type = tuple_n_t<typename std::iterator_traits<T>::value_type, N>;
  using pointer = void;
  using reference = tuple_n_t<typename std::iterator_traits<T>::reference, N>;
  using difference_type = std::iterator_traits<T>::difference_type;

private:
  static constexpr bool isRandom =
      std::is_same_v<iterator_category, std::random_access_iterator_tag>;
  static constexpr bool isBidir =
      std::is_same_v<iterator_category, std::bidirectional_iterator_tag> ||
      isRandom;

public:
  tuple_iterator() = default;
  tuple_iterator(T it) : it(it) {}

  reference operator*() const {
    return [this]<std::size_t... Is>(std::index_sequence<Is...>) {
      auto itCopy(it);
      std::array<T, N> its;
      ((its[Is] = itCopy++), ...);
      return reference(*its[Is]...);
    }(std::make_index_sequence<N>());
  }

  tuple_iterator &operator+=(difference_type d)
    requires(isRandom)
  {
    it += N * d;
    return *this;
  }

  tuple_iterator &operator++() {
    std::advance(it, N);
    return *this;
  }

  difference_type operator-(tuple_iterator other)
    requires(isRandom)
  {
    return (it - other.it) / N;
  }

  friend bool operator==(const tuple_iterator &a, const tuple_iterator &b) {
    return a.it == b.it;
  }

  friend auto operator<=>(const tuple_iterator &a, const tuple_iterator &b)
    requires(isRandom)
  {
    return a.it <=> b.it;
  }
};

template <typename WordIt, bool Inv = false> class set_bits_iterator {
  WordIt word;
  WordIt wordsEnd;
  size_t symb;
  using WordT = std::iterator_traits<WordIt>::value_type;
  static constexpr size_t WordBits = bit_mask_sz<WordT>;

public:
  using iterator_category = std::forward_iterator_tag;
  using reference = size_t;
  using value_type = size_t;
  using difference_type = int;

  bool curSymb() {
    return Inv ^ DynBitField<WordT>{*word, symb & (WordBits - 1), 1};
  }

  auto getWord() { return Inv ? ~*word : *word; }

  void prime() {
    if (word == wordsEnd || curSymb())
      return;
    if (auto rem = getWord() >> (symb & (WordBits - 1))) {
      symb += std::countr_zero(rem);
      return;
    }
    symb += (-symb) & (WordBits - 1);
    do {
      ++word;
    } while (getWord() && word != wordsEnd);

    if (word == wordsEnd)
      return;

    symb += std::countr_zero(getWord());

    assert(curSymb());
  }

  set_bits_iterator &operator++() {
    auto mod = symb & (WordBits - 1);
    symb++;
    word += (mod == (WordBits - 1));

    prime();
    return *this;
  }
  set_bits_iterator operator++(int) {
    auto temp{*this};
    ++(*this);
    return temp;
  }

  // a bit hacky, only rely on this for comparing against end(). To do this
  // properly would also have to track symbsEnd.
  bool operator==(const set_bits_iterator &o) const {
    return word == o.word || (word == (o.word - 1) && symb >= o.symb);
  }

  size_t operator*() { return symb; }

  set_bits_iterator(WordIt word, WordIt wordsEnd, size_t symb)
      : word(word), wordsEnd(wordsEnd), symb(symb) {
    prime();
  }
};
template <typename WordIt>
using unset_bits_iterator = set_bits_iterator<WordIt, true>;

template <typename It> class Range {
public:
  using iterator = It;
  using iterator_category = std::iterator_traits<It>::iterator_category;
  using value_type = std::iterator_traits<It>::value_type;

  template <typename U> Range(const U &u) : Range(u.begin(), u.end()) {}
  template <typename U> Range(U &u) : Range(u.begin(), u.end()) {}

  Range(It beginIt, It endIt) : beginIt(beginIt), endIt(endIt) {}
  Range() = default;

  It begin() const { return beginIt; }
  It end() const { return endIt; }

  auto earlyincr() {
    return ::Range{earlyincr_iterator{beginIt}, earlyincr_iterator{endIt}};
  }

  auto deref() {
    return ::Range{deref_iterator{beginIt}, deref_iterator{endIt}};
  }

  auto no_deref() {
    return ::Range{no_deref_iterator{beginIt}, no_deref_iterator{endIt}};
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

  template <typename T> auto resolve(T &resolver) {
    auto lambda = [&resolver](size_t, auto &&obj) {
      return resolver.resolve(obj);
    };
    return ::Range{transform_iterator<It, decltype(lambda)>(beginIt, lambda),
                   transform_iterator<It, decltype(lambda)>(endIt, lambda)};
  }

  template <typename T> auto cast() {
    auto lambda = [](size_t, const auto &src) { return static_cast<T>(src); };
    return ::Range{transform_iterator<It, decltype(lambda)>(beginIt, lambda),
                   transform_iterator<It, decltype(lambda)>(endIt)};
  }

  template <typename FilterT> auto filter(FilterT filterF) {
    return ::Range{filter_iterator<It, FilterT>(beginIt, endIt, filterF),
                   filter_iterator<It, FilterT>(endIt, endIt)};
  }

  auto do_reverse() { return std::reverse(beginIt, endIt); }

  template <typename T> void sort(T func) { std::sort(begin(), end(), func); }
  template <typename T> void stable_sort(T func) {
    std::stable_sort(begin(), end(), func);
  }
  template <typename T> It find(T &&val) {
    return std::find(begin(), end(), val);
  }
  template <typename T> std::optional<size_t> find_idx(T &&val) {
    for (auto [i, elem] : (*this).enumerate()) {
      if (elem == val)
        return i;
    }
    return std::nullopt;
  }
  template <typename T> It find_if(T func) {
    return std::find_if(begin(), end(), func);
  }
  template <typename T> bool all(T func) {
    return std::all_of(begin(), end(), func);
  }
  template <typename T> bool any(T func) {
    return std::any_of(begin(), end(), func);
  }
  template <typename T> auto count_if(T func) {
    return std::count_if(begin(), end(), func);
  }
  template <typename T> T for_each(T func) {
    return std::for_each(begin(), end(), func);
  }
  template <typename T> T is_sorted(T func) {
    return std::is_sorted(begin(), end(), func);
  }
  auto max() { return std::max_element(begin(), end()); }
  template <typename T> auto max(T func) {
    return std::max_element(begin(), end(), func);
  }
  auto min() { return std::min_element(begin(), end()); }
  template <typename T> auto min(T func) {
    return std::min_element(begin(), end(), func);
  }
  auto sum() { return std::reduce(begin(), end()); }
  auto lcm() {
    if (empty())
      return typename It::value_type(0);
    typename It::value_type val = front();
    for (auto &&e : drop_front())
      val = std::lcm(val, e);
    return val;
  }
  auto gcd() {
    if (empty())
      return typename It::value_type(0);
    typename It::value_type val = front();
    for (auto &&e : drop_front())
      val = std::gcd(val, e);
    return val;
  }
  template <typename T> bool equals(Range<T> other) {
    if constexpr (requires() {
                    this->size();
                    other.size();
                  })
      if (this->size() != other.size())
        return false;
    for (auto [a, b] : (*this).zip(other)) {
      if (a != b)
        return false;
    }
    return true;
  }
  template <typename T, typename Comp> bool equals(Range<T> other, Comp comp) {
    if constexpr (requires() {
                    this->size();
                    other.size();
                  })
      if (this->size() != other.size())
        return false;
    for (auto [a, b] : (*this).zip(other)) {
      if (!comp(a, b))
        return false;
    }
    return true;
  }

  template <typename T> auto zip(const T &other) {
    return ::Range{zip_iterator{begin(), other.begin()},
                   zip_iterator{end(), other.end()}};
  }
  template <typename T> auto zip(T &other) {
    return ::Range{zip_iterator{begin(), other.begin()},
                   zip_iterator{end(), other.end()}};
  }
  auto pairwise() {
    if constexpr (requires { endIt - beginIt; }) {
      assert((endIt - beginIt) % 2 == 0);
    }
    return ::Range{pairwise_iterator{beginIt}, pairwise_iterator{endIt}};
  }

  auto step(unsigned n) {
    if constexpr (requires { endIt - beginIt; }) {
      assert((endIt - beginIt) % n == 0);
    }
    return ::Range{step_iterator{beginIt, n}, step_iterator{endIt, n}};
  }

  template <unsigned N> auto tuple() {
    if constexpr (requires { endIt - beginIt; }) {
      assert((endIt - beginIt) % N == 0);
    }
    return ::Range{tuple_iterator<N, It>{beginIt},
                   tuple_iterator<N, It>{endIt}};
  }

  template <typename T> auto sorted_intersect(T &other) {
    return ::Range{
        sorted_intersect_iterator{begin(), end(), other.begin(), other.end()},
        sorted_intersect_iterator{end(), other.end()},
    };
  }

  bool empty() const { return begin() == end(); }
  static Range emptyRange() { return Range(); }

  auto size() const
    requires(requires(It a, It b) { b - a; })
  {
    return end() - begin();
  }
  decltype(auto) operator[](size_t i) const
    requires(requires(It a) { a[i]; })
  {
    if constexpr (requires { size(); })
      assert(i < size_t(size()));
    return beginIt[i];
  }

  Range subrange(size_t start)
    requires(requires(It a) { a + start; })
  {
    assert(start <= size_t(size()));
    return {beginIt + start, endIt};
  }

  Range subrange(size_t start, size_t len)
    requires(requires(It a) { a + start; })
  {
    assert(start + len <= size_t(size()));
    return {beginIt + start, beginIt + start + len};
  }

  // operator ArrayRef<std::remove_cvref_t<decltype(*std::declval<It>())>>()
  //   requires(std::is_reference_v<decltype(*std::declval<It>())>)
  // {
  //   return ArrayRef{&*beginIt, &*endIt};
  // }
  // operator MutArrayRef<std::remove_cvref_t<decltype(*std::declval<It>())>>()
  //   requires(std::is_reference_v<decltype(*std::declval<It>())> &&
  //            !std::is_const_v<decltype(*std::declval<It>())>)
  // {
  //   return MutArrayRef{&*beginIt, &*endIt};
  // }

  decltype(auto) front() {
    assert(!empty());
    return *beginIt;
  }
  decltype(auto) back()
    requires(requires(It it) { std::prev(it); })
  {
    assert(!empty());
    return *std::prev(endIt);
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

template <std::integral T> class IntRange : public Range<integral_iterator<T>> {
public:
  IntRange(T end) : Range<integral_iterator<T>>(T(0), end) {}
  IntRange(T start, T end) : Range<integral_iterator<T>>(start, end) {}
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
