#pragma once

#include <cassert>
#include <type_traits>

template<typename T>
struct IsByValueRTTI : std::false_type {};

template<typename T>
concept ByValueRTTI = IsByValueRTTI<T>::value;

template <typename T, typename U> T *as(U *ptr) {
  assert((!ptr || T::is_impl(*ptr)) && "Illegal cast");
  return static_cast<T *>(ptr);
}
template <typename T, typename U> T &as(U &ref) {
  assert(T::is_impl(ref) && "Illegal cast");
  return static_cast<T &>(ref);
}
template <typename T, ByValueRTTI U> T as(U &ref) {
  assert(T::is_impl(ref) && "Illegal cast");
  return static_cast<T>(ref);
}

// changed these to dyn_as to avoid confusion with "as DynObj"
template <typename T, typename U> T *dyn_as(U *ptr) {
  return ptr && T::is_impl(*ptr) ? static_cast<T *>(ptr) : nullptr;
}
template <typename T, typename U> T *dyn_as(U &ref) {
  return T::is_impl(ref) ? static_cast<T *>(&ref) : nullptr;
}
template <typename T, ByValueRTTI U> T *dyn_as(U &ref) {
  //return T::is_impl(ref) ? static_cast<T>(ref) : T;
}

template <typename T, typename U> bool is(U *ptr) {
  return ptr && T::is_impl(*ptr);
}
template <typename T, typename U> bool is(U &ref) { return T::is_impl(ref); }

/*
template<typename Derived>
class ByValueRTTIMixin
{
  template<typename T> bool is() {
    return
  }
};
*/
