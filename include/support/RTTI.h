#pragma once

#include <cassert>
#include <type_traits>

template <typename T> struct IsByValueRTTI : std::false_type {};

template <typename T>
concept ByValueRTTI = IsByValueRTTI<T>::value;

template <typename T, typename U>
  requires(!ByValueRTTI<T>)
T *as(U *ptr) {
  assert((!ptr || T::is_impl(*ptr)) && "Illegal cast");
  return static_cast<T *>(ptr);
}
template <typename T, typename U>
  requires(!ByValueRTTI<T>)
T &as(U &ref) {
  assert(T::is_impl(ref) && "Illegal cast");
  return static_cast<T &>(ref);
}
template <typename T, ByValueRTTI U> T as(const U &ref) {
  assert(T::is_impl(ref) && "Illegal cast");
  return static_cast<T>(ref);
}

namespace dyno {
struct nullref_t {};
inline constexpr nullref_t nullref{};
}; // namespace dyno

// changed these to dyn_as to avoid confusion with "as DynObj"
template <typename T, typename U>
  requires(!ByValueRTTI<T>)
T *dyn_as(U *ptr) {
  return ptr && T::is_impl(*ptr) ? static_cast<T *>(ptr) : nullptr;
}
template <typename T, typename U>
  requires(!ByValueRTTI<T>)
T *dyn_as(U &ref) {
  return T::is_impl(ref) ? static_cast<T *>(&ref) : nullptr;
}
template <typename T, ByValueRTTI U> T dyn_as(const U &ref) {
  return T::is_impl(ref) ? static_cast<T>(ref) : dyno::nullref;
}

template <typename T, typename U>
  requires(!ByValueRTTI<T>)
bool is(const U *ptr) {
  return ptr && T::is_impl(*ptr);
}
template <typename T, typename U> bool is(const U &ref) {
  return T::is_impl(ref);
}

template <typename Derived> class RTTIUtilMixin {
public:
  template <typename T> auto is() const {
    return ::is<T, Derived>(*static_cast<const Derived *>(this));
  }
  template <typename T> auto as() const {
    return ::as<T, Derived>(*static_cast<const Derived *>(this));
  }
  template <typename T> auto dyn_as() const {
    return ::dyn_as<T, Derived>(*static_cast<const Derived *>(this));
  }
};
