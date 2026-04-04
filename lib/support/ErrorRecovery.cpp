#include "support/ErrorRecovery.h"
#include "support/Debug.h"
#include "support/SmallVec.h"
#include <cstdio>

SmallVec<CallableRef<void()>, 16> fatalErrorCallbacks;

__attribute__((noreturn)) void report_fatal_error(const char *reason) {
  for (auto cb : Range{fatalErrorCallbacks}.reverse())
    cb();
  fputs(reason, stderr);
  fputc('\n', stderr);
  fflush(stderr);
  DYNO_DBG(abort());
  exit(-1);
}

__attribute__((noreturn)) void report_fatal_error(const std::string &reason) {
  report_fatal_error(reason.c_str());
}

void push_fatal_error_callback(CallableRef<void()> cb) {
  fatalErrorCallbacks.emplace_back(cb);
}
void pop_fatal_error_callback() { fatalErrorCallbacks.pop_back(); }
