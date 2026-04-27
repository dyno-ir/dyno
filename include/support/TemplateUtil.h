#pragma once
#include "support/Tuple.h"
#include <tuple>
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

template <typename T> struct function_info;

template <typename R, typename C, typename... Args>
struct function_info<R (C::*)(Args...)> {
  using type = C;
  using args = Tuple<Args...>;
  using ret = R;
};
template <typename R, typename C, typename... Args>
struct function_info<R (C::*)(Args...) const> {
  using type = C;
  using args = Tuple<Args...>;
  using ret = R;
};
template <typename R, typename C, typename... Args>
struct function_info<R (C::*)(Args...) volatile> {
  using type = C;
  using args = Tuple<Args...>;
  using ret = R;
};

template <typename R, typename C, typename... Args>
struct function_info<R (C::*)(Args...) const volatile> {
  using type = C;
  using args = Tuple<Args...>;
  using ret = R;
};

template <typename R, typename... Args> struct function_info<R (*)(Args...)> {
  using args = Tuple<Args...>;
  using ret = R;
};

template <typename M> using member_obj_t = typename function_info<M>::type;
template <typename M> using function_args_t = typename function_info<M>::args;
template <typename M> using function_ret_t = typename function_info<M>::ret;

template <auto fn, typename Tuple> struct BindMethodImpl;
template <auto fn, typename... Args> struct BindMethodImpl<fn, Tuple<Args...>> {
  static auto f(member_obj_t<decltype(fn)> &obj, Args... args) {
    return (obj.*fn)(std::forward<decltype(args)>(args)...);
  }
  static auto fv(void *obj, Args... args) {
    auto ptr = reinterpret_cast<member_obj_t<decltype(fn)> *>(obj);
    return (*ptr.*fn)(std::forward<decltype(args)>(args)...);
  }
};

template <auto fn>
using BindMethod = BindMethodImpl<fn, function_args_t<decltype(fn)>>;

// emulate [&obj, fn](...) { return obj.*fn(...); } without lambda object by
// aliasing obj with this struct which reinterpret casts this back to obj. Very
// UB, don't use.
template <auto Fn> struct BoundMethodCallable {
  function_ret_t<decltype(Fn)> operator()(auto &&...args) {
    return BindMethod<Fn>::fv(reinterpret_cast<void *>(this),
                              std::forward<decltype(args)>(args)...);
  }
};

template <typename T> struct is_pair : std::false_type {};
template <typename T, typename U>
struct is_pair<std::pair<T, U>> : std::true_type {};
template <typename T> inline constexpr bool is_pair_v = is_pair<T>::value;

template <typename T> struct is_tuple : std::false_type {};
template <typename... Ts>
struct is_tuple<std::tuple<Ts...>> : std::true_type {};
template <typename T> inline constexpr bool is_tuple_v = is_tuple<T>::value;

template <typename T, typename Seq> struct tuple_n_helper;
template <typename T, std::size_t... Is>
struct tuple_n_helper<T, std::index_sequence<Is...>> {
  template <std::size_t> using wrap = T;
  using type = std::tuple<wrap<Is>...>;
};
template <typename T, std::size_t N>
using tuple_n_t = typename tuple_n_helper<T, std::make_index_sequence<N>>::type;
