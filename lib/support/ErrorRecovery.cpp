#include "support/ErrorRecovery.h"
#include "support/Debug.h"
#include <cstdio>

__attribute__((noreturn)) void report_fatal_error(const char *reason) {
  // todo: handlers, stack trace, ...
  fputs(reason, stderr);
  fputc('\n', stderr);
  fflush(stderr);
  DYNO_DBG(abort());
  exit(-1);
}

__attribute__((noreturn)) void report_fatal_error(const std::string &reason) {
  report_fatal_error(reason.c_str());
}
