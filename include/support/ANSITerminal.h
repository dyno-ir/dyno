#pragma once

bool stdoutIsTerminal();
bool stderrIsTerminal();

#define FOR_ANSI_COLORS(LAMBDA)                                                \
  LAMBDA(Reset, "\033[0m")                                                     \
  LAMBDA(Bold, "\033[1m")                                                      \
  LAMBDA(Dim, "\033[2m")                                                       \
  LAMBDA(Italic, "\033[3m")                                                    \
  LAMBDA(Underline, "\033[4m")                                                 \
  LAMBDA(Blink, "\033[5m")                                                     \
  LAMBDA(Reverse, "\033[7m")                                                   \
  LAMBDA(Black, "\033[30m")                                                    \
  LAMBDA(Red, "\033[31m")                                                      \
  LAMBDA(Green, "\033[32m")                                                    \
  LAMBDA(Yellow, "\033[33m")                                                   \
  LAMBDA(Blue, "\033[34m")                                                     \
  LAMBDA(Magenta, "\033[35m")                                                  \
  LAMBDA(Cyan, "\033[36m")                                                     \
  LAMBDA(White, "\033[37m")                                                    \
  LAMBDA(BgBlack, "\033[40m")                                                  \
  LAMBDA(BgRed, "\033[41m")                                                    \
  LAMBDA(BgGreen, "\033[42m")                                                  \
  LAMBDA(BgYellow, "\033[43m")                                                 \
  LAMBDA(BgBlue, "\033[44m")                                                   \
  LAMBDA(BgMagenta, "\033[45m")                                                \
  LAMBDA(BgCyan, "\033[46m")                                                   \
  LAMBDA(BgWhite, "\033[47m")

#define STDERR_COLOR(nm, str)                                                  \
  inline const char *stderr##nm() { return stderrIsTerminal() ? str : ""; }

#define STDOUT_COLOR(nm, str)                                                  \
  inline const char *stdout##nm() { return stdoutIsTerminal() ? str : ""; }

FOR_ANSI_COLORS(STDERR_COLOR)
FOR_ANSI_COLORS(STDOUT_COLOR)

#undef FOR_ANSI_COLORS
#undef STDERR_COLOR
#undef STDOUT_COLOR
