#pragma once

#include <memory>

template <typename T> struct RAIIToken {
  RAIIToken(const RAIIToken &) = delete;
  RAIIToken &operator=(const RAIIToken &) = delete;
  RAIIToken(RAIIToken &&other) { this->RAIIToken::operator=(std::move(other)); }
  RAIIToken &operator=(RAIIToken &&other) {
    this->parent = other.parent;
    other.parent = nullptr;
    return *this;
  };
  RAIIToken(T &parent) : parent(&parent) {}

  T *parent;
  ~RAIIToken() {
    if (parent)
      parent->unbind();
  }
};

template <typename T> class TempBind {
  T &assigned;

public:
  TempBind(T &assigned, const T &value) { assigned = value; }
  ~TempBind() { assigned = T(); }
};

template <typename T> class TempBindPtr {
  T *val = nullptr;

public:
  [[nodiscard]] RAIIToken<TempBindPtr> bind(T *toVal) {
    val = toVal;
    return RAIIToken<TempBindPtr>{*this};
  }
  void unbind() { val = nullptr; }
  T *operator->() { return val; }
  T &operator*() { return *val; }
  explicit operator bool() const { return !!val; }
};

// unsafe memory aquisition without init wrapper
template <typename T> class TempBindVal {
  union {
    T val;
  };

public:
  template <typename... Args>
  [[nodiscard]] RAIIToken<TempBindVal> emplace(Args... args) {
    std::construct_at(&val, std::forward<Args>(args)...);
    return RAIIToken<TempBindVal>{*this};
  }
  void unbind() { std::destroy_at(&val); }
  T *operator->() { return &val; }
  T &operator*() { return val; }
  const T *operator->() const { return &val; }
  const T &operator*() const { return val; }

  TempBindVal(){};
  ~TempBindVal(){};
};
