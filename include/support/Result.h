#pragma once
#include <cassert>
#include <memory>
#include <optional>
#include <type_traits>
#include <utility>

template <typename Res, typename Err> class Result {
  union {
    Res res;
    Err err;
  };
  bool isRes;

public:
  Result(const Result &o) : isRes(o.isRes) {
    if (isRes) {
      std::construct_at(&res, o.res);
    } else {
      std::construct_at(&err, o.err);
    }
  }
  Result(Result &&o) : isRes(o.isRes) {
    if (isRes) {
      std::construct_at(&res, std::move(o.res));
    } else {
      std::construct_at(&err, std::move(o.err));
    }
  }
  Result &operator=(const Result &o) {
    this->~Result();
    std::construct_at(this, o);
    return *this;
  }
  Result &operator=(Result &&o) {
    this->~Result();
    std::construct_at(this, std::move(o));
    return *this;
  }
  ~Result() {
    if (isRes) {
      std::destroy_at(&res);
    } else {
      std::destroy_at(&err);
    }
  }

  constexpr Result(Res &&res) : res(std::move(res)), isRes(true) {}
  constexpr Result(const Res &res) : res(res), isRes(true) {}
  constexpr Result(Err &&err) : err(std::move(err)), isRes(false) {}
  constexpr Result(const Err &err) : err(err), isRes(false) {}

  // Allow construction from anything convertible to Res (e.g. via an explicit
  // conversion), mirroring std::expected's forwarding value constructor.
  template <typename U>
    requires(!std::is_same_v<std::remove_cvref_t<U>, Result> &&
             std::is_constructible_v<Res, U> &&
             !std::is_constructible_v<Err, U>)
  constexpr Result(U &&val) : res(std::forward<U>(val)), isRes(true) {}

  explicit operator bool() const { return isRes; }
  bool has_value() const { return isRes; }

  const Res &value() const {
    assert(isRes);
    return res;
  }
  Res &value() {
    assert(isRes);
    return res;
  }
  const Err &error() const {
    assert(!isRes);
    return err;
  }
  Err &error() {
    assert(!isRes);
    return err;
  }

  Res &operator*() { return value(); }
  const Res &operator*() const { return value(); }

  Res *operator->() { return &value(); }
  const Res *operator->() const { return &value(); }
};

template <typename Err> class Result<void, Err> {
  std::optional<Err> err;

public:
  Result() : err(std::nullopt) {}
  Result(Err &&err) : err(std::move(err)) {}
  Result(const Err &err) : err(err) {}

  explicit operator bool() const { return !err; }
  bool has_value() const { return !err; }

  const Err &error() const {
    assert(err);
    return *err;
  }
  Err &error() {
    assert(err);
    return *err;
  }
};
