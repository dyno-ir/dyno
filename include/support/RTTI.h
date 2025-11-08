#pragma once

#include <cassert>
#include <concepts>
#include <tuple>
#include <type_traits>

// empty base opt fails if the tag ends up in the hierarchy twice (every
// (obj,type) needs unique addr). work around by providing multiple tags.
struct ByValueRTTITag {};
struct ByValueRTTITag2 {};

template <typename T>
concept ByValueRTTI = std::is_base_of_v<ByValueRTTITag, std::remove_cv_t<T>> ||
                      std::is_base_of_v<ByValueRTTITag2, std::remove_cv_t<T>>;

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
  return *static_cast<T *>(&ref);
}
template <typename T, typename U>
  requires(!ByValueRTTI<T>)
const T *as(const U *ptr) {
  assert((!ptr || T::is_impl(*ptr)) && "Illegal cast");
  return static_cast<const T *>(ptr);
}
template <typename T, typename U>
  requires(!ByValueRTTI<T>)
const T &as(const U &ref) {
  assert(T::is_impl(ref) && "Illegal cast");
  return *static_cast<const T *>(&ref);
}

template <typename T, ByValueRTTI U>
  requires(ByValueRTTI<T>)
T as(const U &ref) {
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

template <typename Derived> class ByValueRTTIUtilMixin {
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

template <typename Derived> class RTTIUtilMixin {
public:
  template <typename T> auto is() const {
    return ::is<T, Derived>(*static_cast<const Derived *>(this));
  }
  template <typename T> const T &as() const {
    return ::as<T, Derived>(*static_cast<const Derived *>(this));
  }
  template <typename T> const T *dyn_as() const {
    return ::dyn_as<T, Derived>(*static_cast<const Derived *>(this));
  }
  template <typename T> T &as() {
    return ::as<T, Derived>(*static_cast<Derived *>(this));
  }
  template <typename T> T *dyn_as() {
    return ::dyn_as<T, Derived>(*static_cast<Derived *>(this));
  }
};
