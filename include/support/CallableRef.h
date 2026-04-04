#pragma once

#include <cassert>
#include <type_traits>
#include <utility>

template <typename Fn> class CallableRef;
template <typename Ret, typename... Params> class CallableRef<Ret(Params...)> {
  Ret (*callback)(void *callable, Params... params) = nullptr;
  void *callable;

  template <typename Callable>
  static Ret callOnObject(void *callable, Params &&...params) {
    return (*reinterpret_cast<Callable *>(callable))(
        std::forward<Params>(params)...);
  }

public:
  CallableRef() = default;
  CallableRef(const CallableRef &) = default;
  CallableRef(CallableRef &&) = default;
  CallableRef &operator=(const CallableRef &) = default;
  CallableRef &operator=(CallableRef &&) = default;

  Ret operator()(Params... params) const {
    assert(*this && "called uninitialized CallableRef");
    return callback(callable, std::forward<Params>(params)...);
  }

  template <typename Callable>
    requires(!std::is_same_v<std::remove_cvref_t<Callable>, CallableRef>)
  CallableRef(Callable &&callable)
      : callback(callOnObject<std::remove_reference_t<Callable>>),
        callable(reinterpret_cast<void *>(&callable)) {}

  // for use with BindMethod<>
  template <typename Obj>
  CallableRef(Obj *callable, Ret (*callback)(void *, Params...))
      : callback(callback), callable(reinterpret_cast<void *>(callable)) {}

  explicit operator bool() const { return callback; }
  bool operator==(const CallableRef<Ret(Params...)> &other) const {
    return callable == other.callable && callback == other.callback;
  }
};

template <typename Obj, typename R, typename... Args>
CallableRef(Obj *obj, R (*fn)(void *, Args...)) -> CallableRef<R(Args...)>;

// For referencing methods when object is known/stored elsewhere.
// Single function pointer.
template <typename Fn> class MethodRef;
template <typename Ret, typename... Params> class MethodRef<Ret(Params...)> {
  Ret (*callback)(void *callable, Params... params) = nullptr;

public:
  MethodRef() = default;
  MethodRef(const MethodRef &) = default;
  MethodRef(MethodRef &&) = default;
  MethodRef &operator=(const MethodRef &) = default;
  MethodRef &operator=(MethodRef &&) = default;

  // for use with BindMethod<>
  template <typename Obj>
  MethodRef(Ret (*callback)(void *, Params...)) : callback(callback) {}

  explicit operator bool() const { return callback; }
  bool operator==(const MethodRef<Ret(Params...)> &other) const {
    return callback == other.callback;
  }

  CallableRef<Ret(Params...)> bind(void *self) { return {self, callback}; }
};

template <typename R, typename... Args>
MethodRef(R (*fn)(void *, Args...)) -> MethodRef<R(Args...)>;
