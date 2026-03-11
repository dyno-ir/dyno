#pragma once

#include <format>

void report_fatal_error(const char *reason) __attribute__((noreturn));
void report_fatal_error(const std::string &reason) __attribute__((noreturn));
template <typename... Args>
__attribute__((noreturn)) void
report_fatal_error(std::format_string<Args...> fmt, Args &&...args) {
  report_fatal_error(std::format(fmt, std::forward<Args>(args)...));
}
