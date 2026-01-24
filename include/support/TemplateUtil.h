#pragma once
#include <cstdint>
#include <tuple>
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
  using args = std::tuple<Args...>;
  using ret = R;
};
template <typename R, typename C, typename... Args>
struct function_info<R (C::*)(Args...) const> {
  using type = C;
  using args = std::tuple<Args...>;
  using ret = R;
};
template <typename R, typename C, typename... Args>
struct function_info<R (C::*)(Args...) volatile> {
  using type = C;
  using args = std::tuple<Args...>;
  using ret = R;
};

template <typename R, typename C, typename... Args>
struct function_info<R (C::*)(Args...) const volatile> {
  using type = C;
  using args = std::tuple<Args...>;
  using ret = R;
};

template <typename R, typename... Args> struct function_info<R (*)(Args...)> {
  using args = std::tuple<Args...>;
  using ret = R;
};

template <typename M> using member_obj_t = typename function_info<M>::type;
template <typename M> using function_args_t = typename function_info<M>::args;
template <typename M> using function_ret_t = typename function_info<M>::ret;

template <auto fn, typename Tuple> struct BindMethodImpl;
template <auto fn, typename... Args>
struct BindMethodImpl<fn, std::tuple<Args...>> {
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
