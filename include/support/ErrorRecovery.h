#pragma once
#include "dyno/DebugInfo.h"
#include "support/CallableRef.h"
#include <format>

void print_note(const char *reason,
                std::optional<dyno::DebugSourceLoc> loc = std::nullopt);
void print_note(const std::string &reason,
                std::optional<dyno::DebugSourceLoc> loc = std::nullopt);
template <typename... Args>
void print_note(std::optional<dyno::DebugSourceLoc> loc,
                std::format_string<Args...> fmt, Args &&...args) {
  print_note(std::format(fmt, std::forward<Args>(args)...), loc);
}

void report_fatal_error() __attribute__((noreturn));
void report_fatal_error(const char *reason) __attribute__((noreturn));
void report_fatal_error(const std::string &reason) __attribute__((noreturn));
template <typename... Args>
__attribute__((noreturn)) void
report_fatal_error(std::format_string<Args...> fmt, Args &&...args) {
  report_fatal_error(std::format(fmt, std::forward<Args>(args)...));
}

namespace dyno {
class InstrRef;
class Context;
}; // namespace dyno

// Try to print source location as a compiler-style error message. Fall back to
// IDs/raw printout.
__attribute__((noreturn)) void report_fatal_error(dyno::Context &ctx,
                                                  const dyno::InstrRef &atInstr,
                                                  const char *reason);
__attribute__((noreturn)) void report_fatal_error(dyno::Context &ctx,
                                                  const dyno::InstrRef &atInstr,
                                                  const std::string &reason);
template <typename... Args>
__attribute__((noreturn)) void
report_fatal_error(dyno::Context &ctx, const dyno::InstrRef &atInstr,
                   std::format_string<Args...> fmt, Args &&...args) {
  report_fatal_error(ctx, atInstr,
                     std::format(fmt, std::forward<Args>(args)...));
}

void push_fatal_error_callback(CallableRef<void()>);
void pop_fatal_error_callback();

const char *last_fatal_error_reason();
