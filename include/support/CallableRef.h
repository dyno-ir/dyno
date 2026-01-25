#pragma once

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

  Ret operator()(Params... params) const {
    assert(*this && "called uninitialized CallableRef");
    return callback(callable, std::forward<Params>(params)...);
  }

  template <typename Callable>
  CallableRef(Callable &&callable)
      : callback(callOnObject<std::remove_reference_t<Callable>>),
        callable(reinterpret_cast<void *>(&callable)) {}

  explicit operator bool() const { return callback; }
  bool operator==(const CallableRef<Ret(Params...)> &Other) const {
    return callable == Other.callable;
  }
};

// directly refer to a member method bound with BindMethod<Obj::Member>.
// The difference from CallableRef is essentially that the this-pointer
// is part of (First, Params...) here while it is not for CallableRef.
template <typename Fn> class MemberRef;
template <typename Ret, typename First, typename... Params>
class MemberRef<Ret(First, Params...)> {
  Ret (*callback)(First, Params... params) = nullptr;
  void *callable;

public:
  MemberRef() = default;
  MemberRef(void *callable, Ret (*callback)(First, Params...))
      : callback(callback), callable(callable) {}

  Ret operator()(Params... params) const {
    assert(*this && "called uninitialized MemberRef");
    return callback(callable, std::forward<decltype(params)>(params)...);
  }

  explicit operator bool() const { return callback; }
  bool operator==(const MemberRef<Ret(First, Params...)> &Other) const {
    return callable == Other.callable;
  }
};

template <typename Obj, typename Fn>
MemberRef(Obj *obj, Fn *fn) -> MemberRef<Fn>;