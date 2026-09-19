#include "support/ANSITerminal.h"
#include <cstdio>

#ifdef _WIN32
#include <io.h>
#define ISATTY _isatty
#define FILENO _fileno
#else
#include <unistd.h>
#define ISATTY isatty
#define FILENO fileno
#endif

bool stdoutIsTerminal() { return ISATTY(FILENO(stdout)) != 0; }
bool stderrIsTerminal() { return ISATTY(FILENO(stderr)) != 0; }
