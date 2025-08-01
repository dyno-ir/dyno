#pragma once

#include <memory>
template <typename T> class TempBind {
  T &assigned;

public:
  TempBind(T &assigned, const T &value) { assigned = value; }
  ~TempBind() { assigned = T(); }
};

template <typename T> class TempBindPtr {
  T *val = nullptr;
  struct Token {
    TempBindPtr &parent;
    ~Token() { parent.unbind(); }
  };

public:
  [[nodiscard]] Token bind(T *toVal) {
    val = toVal;
    return Token{*this};
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
  struct Token {
    TempBindVal &parent;
    ~Token() { parent.unbind(); }
  };

public:
  template <typename... Args> [[nodiscard]] Token emplace(Args... args) {
    std::construct_at(&val, std::forward<Args>(args)...);
    return Token{*this};
  }
  void unbind() { std::destroy_at(&val); }
  T *operator->() { return &val; }
  T &operator*() { return val; }

  TempBindVal() {};
};
