#pragma once
#include "support/Debug.h"
#include "support/StringRef.h"
#include <memory>
#include <ostream>
#include <type_traits>
#include <utility>

class FormatObjBase {
protected:
  using destroy_f = void(FormatObjBase *);
  using print_f = void(FormatObjBase *, std::ostream &str);
  using funcs_t = std::pair<print_f *, destroy_f *>;

public:
  FormatObjBase *next;
  const funcs_t *funcs;

  ~FormatObjBase() { funcs->second(this); }
};

template <typename T> class FormatObj : public FormatObjBase {
  T t;

  static void destroy(FormatObjBase *obj) {
    std::destroy_at(&static_cast<FormatObj *>(obj)->t);
  }
  static void print(FormatObjBase *obj, std::ostream &str) {
    str << static_cast<FormatObj *>(obj)->t;
  }
  static constexpr funcs_t funcs{&print, &destroy};

public:
  FormatObj(T &&obj, FormatObjBase *next)
      : FormatObjBase(next, &funcs), t(std::forward<T>(obj)) {}
  FormatObj(const T &obj, FormatObjBase *next)
      : FormatObjBase(next, &funcs), t(obj) {}
};

// Slow but owning twine, mostly for error messages
class Format {
  const char *fmt;
  FormatObjBase *list = nullptr;

  template <typename T, typename... Rest> void add(T &&t, Rest &&...rest) {
    add(std::forward<Rest>(rest)...);
    addSingle(std::forward<T>(t));
  }
  void add() {}

  template <typename T> void addSingle(T &&t) {
    using Arg = std::decay_t<T>;
    auto *newNode =
        reinterpret_cast<FormatObj<Arg> *>(malloc(sizeof(FormatObj<Arg>)));
    list = std::construct_at(newNode, std::forward<T>(t), list);
  }

public:
  Format(const Format &) = delete;
  Format(Format &&o) { *this = std::move(o); }
  Format &operator=(const Format &) = delete;
  Format &operator=(Format &&o) {
    memcpy((void *)this, &o, sizeof(Format));
    o.list = nullptr;
    return *this;
  }

  template <typename... Args>
  Format(const char *fmt, Args &&...args) : fmt(fmt) {
    add(std::forward<Args>(args)...);
  }
  // template <typename It> Format(const char *fmt, Range<It> args) : fmt(fmt) {
  //   for (auto arg : args.reverse())
  //     add(arg);
  // }

  void toStream(std::ostream &str) const {
    auto listP = list;
    auto fmtR = StringRef{fmt};
    while (1) {
      auto it = fmtR.find("{}");
      std::print(str, "{}", StringRef{fmtR.begin(), it});
      if (it == fmtR.end())
        return;
      listP->funcs->first(listP, str);
      listP = listP->next;
      it += 2;
      fmtR = StringRef{it, fmtR.end()};
    }
  }

  friend std::ostream &operator<<(std::ostream &os, const Format &fmt) {
    fmt.toStream(os);
    return os;
  }

  ~Format() {
    while (list) {
      auto elem = list;
      list = list->next;
      std::destroy_at(elem);
      free(elem);
    }
  }
};

class LazyFormat {
  std::function<void(std::ostream &str)> callback;

public:
  explicit LazyFormat(std::function<void(std::ostream &)> callback)
      : callback(std::move(callback)) {}
  friend std::ostream &operator<<(std::ostream &os, const LazyFormat &fmt) {
    fmt.callback(os);
    return os;
  }
};
