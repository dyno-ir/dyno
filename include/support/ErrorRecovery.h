#pragma once

#include <format>
#include <stdexcept>
#include <vector>

template <typename T> class FrameLogger {
public:
  class logger_error : public std::runtime_error {};

  class LogFrame {
  public:
    LogFrame(FrameLogger &logger, T data) : logger(logger) {
      logger.push(data);
    }
    LogFrame(const LogFrame &) = delete;
    LogFrame &operator=(const LogFrame &) = delete;
    LogFrame(LogFrame &&) noexcept = delete;
    LogFrame &operator=(LogFrame &&) noexcept = delete;
    ~LogFrame() { logger.pop(); }

  private:
    FrameLogger &logger;
  };

  LogFrame logFrame(T data) { return LogFrame(*this, data); }

  void freeze() { isFreezed = true; }

  void push(T data) { trace.push_back(data); }
  void pop() {
    if (isFreezed) {
      return;
    }
    trace.pop_back();
  }

  std::vector<T> trace;

private:
  bool isFreezed = false;
};

void report_fatal_error(const char *reason) __attribute__((noreturn));
void report_fatal_error(const std::string &reason) __attribute__((noreturn));
template <typename... Args>
__attribute__((noreturn)) void
report_fatal_error(std::format_string<Args...> fmt, Args &&...args) {
  report_fatal_error(std::format(fmt, std::forward<Args>(args)...));
}
