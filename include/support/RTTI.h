#pragma once

#include <cassert>

template <typename T, typename U> T *as(U *ptr) {
  assert((!ptr || T::is_impl(*ptr)) && "Illegal cast");
  return static_cast<T *>(ptr);
}
template <typename T, typename U> T &as(U &ref) {
  assert(T::is_impl(ref) && "Illegal cast");
  return static_cast<T &>(ref);
}

template <typename T, typename U> T *as_dyn(U *ptr) {
  return ptr && T::is_impl(*ptr) ? static_cast<T *>(ptr) : nullptr;
}
template <typename T, typename U> T *as_dyn(U &ref) {
  return T::is_impl(ref) ? static_cast<T *>(&ref) : nullptr;
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

// do trait by value RTTI default false, specify true for types
