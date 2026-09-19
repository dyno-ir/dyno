#include "support/ErrorRecovery.h"
#include "dyno/Context.h"
#include "dyno/DebugInfo.h"
#include "dyno/InstrPrinter.h"
#include "support/ANSITerminal.h"
#include "support/Debug.h"
#include "support/SmallVec.h"
#include <cstdio>

SmallVec<CallableRef<void()>, 16> fatalErrorCallbacks;

const char *lastFatalReason;
const char *last_fatal_error_reason() { return lastFatalReason; }

void print_note(const char *reason, std::optional<dyno::DebugSourceLoc> loc) {
  if (loc) {
    std::print(std::cerr, "{}{}{}: {}note:{} {}{}\n", stderrBold(),
               stderrWhite(), *loc, stderrCyan(), stderrWhite(), reason,
               stderrReset());
  } else {
    std::print(std::cerr, "{}{}note:{} {}{}\n", stderrBold(), stderrCyan(),
               stderrWhite(), reason, stderrReset());
  }
}
void print_note(const std::string &reason,
                std::optional<dyno::DebugSourceLoc> loc) {
  print_note(reason.c_str(), loc);
}

__attribute__((noreturn)) void report_fatal_error(const char *reason) {
  lastFatalReason = reason;
  fprintf(stderr, "%s%serror: %s%s\n%s", stderrBold(), stderrRed(),
          stderrWhite(), reason, stderrReset());
  fflush(stderr);
  for (auto cb : Range{fatalErrorCallbacks}.reverse())
    cb();
  using namespace dyno;
  DYNO_DBG_RUN(abort());
  exit(-1);
}

__attribute__((noreturn)) void report_fatal_error() {
  for (auto cb : Range{fatalErrorCallbacks}.reverse())
    cb();
  using namespace dyno;
  DYNO_DBG_RUN(abort());
  exit(-1);
}

__attribute__((noreturn)) void report_fatal_error(const std::string &reason) {
  report_fatal_error(reason.c_str());
}

__attribute__((noreturn)) void report_fatal_error(dyno::Context &ctx,
                                                  const dyno::InstrRef &atInstr,
                                                  const std::string &reason) {
  report_fatal_error(ctx, atInstr, reason.c_str());
}

__attribute__((noreturn)) void report_fatal_error(dyno::Context &ctx,
                                                  const dyno::InstrRef &atInstr,
                                                  const char *reason) {
  auto &srcLocInfo = ctx.getCtx<dyno::CoreDialectContext>().instrSourceLocInfo;
  auto locs = srcLocInfo.getSourceLocs(atInstr);
  std::stringstream str;
  std::print(str, "{}{}", stderrBold(), stderrWhite());
  if (locs.empty())
    std::print(str, "<instr {}>: ", atInstr.getObjID().num);
  else
    std::print(str, "{}: ", locs.front());

  // todo
  //  - optionally print dyno IR (print w/o handlers or handlers in ctx?)
  //  - best effort print source file if exists

  std::print(str, "{}error:{} {}\n", stderrRed(), stderrWhite(), reason);
  for (auto loc : locs.drop_front())
    print_note("additional source location", loc);
  std::print(str, "{}", stderrReset());

  std::cerr << std::move(str).str();
  report_fatal_error();
}

void push_fatal_error_callback(CallableRef<void()> cb) {
  fatalErrorCallbacks.emplace_back(cb);
}
void pop_fatal_error_callback() { fatalErrorCallbacks.pop_back(); }
