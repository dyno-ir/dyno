#include "support/ErrorRecovery.h"
#include <iostream>

void report_fatal_error(const char *reason) {
  // todo: handlers, stack trace, ...
  std::cerr << "error: " << reason << std::endl;
  exit(-1);
}
