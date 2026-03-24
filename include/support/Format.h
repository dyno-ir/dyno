#pragma once
#include "support/StringRef.h"
#include <memory>
#include <ostream>
#include <utility>

class FormatObjBase {
protected:
  using destroy_f = void(void *);
  using print_f = void(void *, std::ostream &str);
  using funcs_t = std::pair<print_f *, destroy_f *>;

public:
  FormatObjBase *next;
  const funcs_t *funcs;

  ~FormatObjBase() { funcs->second(reinterpret_cast<void *>(this)); }
};

template <typename T> class FormatObj : public FormatObjBase {
  T t;

  static void destroy(void *obj) {
    std::destroy_at(&reinterpret_cast<FormatObj *>(obj)->t);
  }
  static void print(void *obj, std::ostream &str) {
    str << reinterpret_cast<FormatObj *>(obj)->t;
  }
  static constexpr funcs_t funcs{&print, &destroy};

public:
  FormatObj(T &&obj, FormatObjBase *next)
      : FormatObjBase(next, &funcs), t(obj) {}
};

// Slow but owning twine, mostly for error messages
class Format {
  const char *fmt;
  FormatObjBase *list = nullptr;

  template <typename... Rest, typename T> void add(Rest &&...rest, T &&t) {
    list->next = new FormatObj<T>(t, list->next);
    add(std::forward<Rest>(rest)...);
  }
  void add() {}

public:
  template <typename... Args>
  Format(const char *fmt, Args &&...args) : fmt(fmt) {
    add(args...);
  }

  void toStream(std::ostream &str) const {
    auto listP = list;
    auto fmtR = StringRef{fmt};
    while (1) {
      auto it = fmtR.find("{}");
      std::print(str, "{}", StringRef{fmtR.begin(), it});
      if (it == fmtR.end())
        return;
      listP->funcs->first(reinterpret_cast<void *>(list), str);
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
      delete elem;
    }
  }
};