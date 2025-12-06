#include "support/ErrorRecovery.h"
#include "support/Debug.h"
#include <format>

__attribute__((noreturn)) void report_fatal_error(const char *reason) {
  // todo: handlers, stack trace, ...
  fputs(reason, stderr);
  DYNO_DBG(abort());
  exit(-1);
}

__attribute__((noreturn)) void report_fatal_error(const std::string &reason) {
  report_fatal_error(reason.c_str());
}
