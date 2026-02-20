#pragma once

#include <type_traits>
#include <utility>

template <typename Fn> class CallableRef;
template <typename Ret, typename... Params> class CallableRef<Ret(Params...)> {
  Ret (*callback)(void *callable, Params... params) = nullptr;
  void *callable;

  template <typename Callable>
  static Ret callOnObject(void *callable, Params... params) {
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
  bool operator==(const CallableRef<Ret(Params...)> &Other) const {
    return callable == Other.callable;
  }
};

template <typename Obj, typename R, typename... Args>
CallableRef(Obj *obj, R (*fn)(void *, Args...)) -> CallableRef<R(Args...)>;
