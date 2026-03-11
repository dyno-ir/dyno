#pragma once

#include <utility>
template <std::size_t I, typename T> struct TupleElem {
  T value;
};

template <typename IndexSeq, typename... Ts> struct TupleBase;

namespace detail {
template <std::size_t I, typename T>
constexpr T &extract_leaf(TupleElem<I, T> &leaf) noexcept {
  return leaf.value;
}

template <std::size_t I, typename T>
constexpr const T &extract_leaf(const TupleElem<I, T> &leaf) noexcept {
  return leaf.value;
}

template <std::size_t I, typename T>
constexpr T &&extract_leaf(TupleElem<I, T> &&leaf) noexcept {
  return std::move(leaf.value);
}

template <typename T, std::size_t I>
constexpr T &extract_leaf_by_type(TupleElem<I, T> &leaf) noexcept {
  return leaf.value;
}

template <std::size_t I, typename T> T element_type_extractor(TupleElem<I, T>);

}; // namespace detail

// Flat, tuple elemns implemented via inheritance
template <std::size_t... Is, typename... Ts>
struct TupleBase<std::index_sequence<Is...>, Ts...> : TupleElem<Is, Ts>... {
  template <typename... Us>
  constexpr TupleBase(Us &&...args)
      : TupleElem<Is, Ts>{std::forward<Us>(args)}... {}

  constexpr TupleBase() : TupleElem<Is, Ts>{}... {}

  // TupleBase(const TupleBase &) = default;
  // TupleBase(TupleBase &&) = default;

  template <std::size_t I> constexpr decltype(auto) get() & noexcept {
    return detail::extract_leaf<I>(*this);
  }

  template <std::size_t I> constexpr decltype(auto) get() const & noexcept {
    return detail::extract_leaf<I>(*this);
  }

  template <std::size_t I> constexpr decltype(auto) get() && noexcept {
    return detail::extract_leaf<I>(std::move(*this));
  }

  template <typename T> constexpr T &get() noexcept {
    T *t;
    (
        [&] {
          if constexpr (std::is_same_v<Ts, T>)
            t = &this->get<Is>();
        }(),
        ...);
    return *t;
  }

  template <typename F> constexpr decltype(auto) apply(F &&f) {
    return std::forward<F>(f)(static_cast<TupleElem<Is, Ts> &>(*this).value...);
  }

  template <typename F> constexpr decltype(auto) apply(F &&f) const {
    return std::forward<F>(f)(
        static_cast<const TupleElem<Is, Ts> &>(*this).value...);
  }

  static constexpr std::size_t size = sizeof...(Is);

  // TupleBase &operator=(const TupleBase &o) {
  //   ((this->get<Is>() = o.get<Is>()), ...);
  //   return *this;
  // }
  // TupleBase &operator=(TupleBase &&o) {
  //   ((this->get<Is>() = std::move(o.get<Is>())), ...);
  //   return *this;
  // }
};

template <typename... Ts>
struct Tuple : public TupleBase<std::index_sequence_for<Ts...>, Ts...> {
  using Base = TupleBase<std::index_sequence_for<Ts...>, Ts...>;
  using Base::Base;
};

template <typename... Args>
constexpr Tuple<std::unwrap_ref_decay_t<Args>...> mk_tuple(Args &&...args) {
  return {std::forward<Args>(args)...};
}

template <std::size_t... IsA, typename... TsA, std::size_t... IsB,
          typename... TsB>
auto tuple_concat(TupleBase<std::index_sequence<IsA...>, TsA...> &&a,
                  TupleBase<std::index_sequence<IsB...>, TsB...> &&b) {
  using ResT = TupleBase<std::index_sequence<IsA..., (sizeof...(IsA) + IsB)...>,
                         TsA..., TsB...>;
  return ResT(std::move(a.template get<IsA>())...,
              std::move(b.template get<IsB>())...);
}
template <std::size_t... IsA, typename... TsA, std::size_t... IsB,
          typename... TsB>
auto tuple_concat(const TupleBase<std::index_sequence<IsA...>, TsA...> &a,
                  const TupleBase<std::index_sequence<IsB...>, TsB...> &b) {
  using ResT = TupleBase<std::index_sequence<IsA..., (sizeof...(IsA) + IsB)...>,
                         TsA..., TsB...>;
  return ResT(a.template get<IsA>()..., b.template get<IsB>()...);
}

template <std::size_t I, typename Tuple> struct tuple_element;

template <std::size_t I, typename... Ts> struct tuple_element<I, Tuple<Ts...>> {
  using type =
      decltype(detail::element_type_extractor<I>(std::declval<Tuple<Ts...>>()));
};

template <std::size_t I, typename T>
using tuple_element_t = typename tuple_element<I, T>::type;

#include <utility> // for std::tuple_size and std::tuple_element

namespace std {
template <typename... Ts>
struct tuple_size<Tuple<Ts...>>
    : std::integral_constant<std::size_t, sizeof...(Ts)> {};

template <std::size_t I, typename... Ts>
constexpr decltype(auto) get(Tuple<Ts...> &t) noexcept {
  return t.template get<I>();
}

template <std::size_t I, typename... Ts>
constexpr decltype(auto) get(const Tuple<Ts...> &t) noexcept {
  return t.template get<I>();
}

template <std::size_t I, typename... Ts>
constexpr decltype(auto) get(Tuple<Ts...> &&t) noexcept {
  return std::move(t).template get<I>();
}
template <std::size_t I, typename... Ts> struct tuple_element<I, Tuple<Ts...>> {
  using type = ::tuple_element_t<I, Tuple<Ts...>>;
};
} // namespace std
