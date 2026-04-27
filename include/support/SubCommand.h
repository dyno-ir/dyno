#pragma once
#include "support/CmdLineArgs.h"
#include "support/SmallVec.h"
#include "support/StringRef.h"

class SubCommand {
  friend class SubCommandHandler;
  StringRef name;
  CmdLineArgHandler handler;
  const unsigned idx;

public:
  void registerArg(CmdLineArgBase &c) { handler.registerArg(c); }
  SubCommand(StringRef name, unsigned idx) : name(name), idx(idx) {}
};

class SubCommandHandler {
  SmallVec<SubCommand *, 8> subCommands;
  SubCommand *defaultSubCommand = nullptr;
  SubCommand *activeSubCommand = nullptr;
  const char *executableName = "<executable>";

  SubCommand *findSubCmd(StringRef str) {
    auto it = Range{subCommands}.find_if(
        [&](SubCommand *sub) { return sub->name == str; });
    if (it != subCommands.end())
      return *it;
    return nullptr;
  }

  void printHelpExit(bool isError) {
    std::ostream &os = isError ? std::cerr : std::cout;
    std::print(os, "usage: {} [subcommand]\n", executableName);

    std::print(os, "possible subcommands:");

    for (auto &cmd : Range{subCommands}.deref()) {
      std::string fakeName = executableName + std::string(" ") +
                             std::string(cmd.name.begin(), cmd.name.end());
      cmd.handler.executableName = fakeName.data();
      std::print(os, "\n\nsubcommand \"{}\"\n", cmd.name);
      cmd.handler.printHelp(isError, 1);
      cmd.handler.executableName = nullptr;
    }

    exit(isError ? -1 : 0);
  }

public:
  void registerSubCmd(SubCommand &s) { subCommands.emplace_back(&s); }
  unsigned parse(int argc, char **argv) {
    if (argc < 1)
      printHelpExit(true);
    executableName = argv[0];
    if (argc == 1)
      printHelpExit(true);

    if (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0)
      printHelpExit(false);

    bool includeFirst = false;
    activeSubCommand = [&]() {
      if (argc == 1 || argv[1][0] == '-' || StringRef{argv[1]}.contains('.')) {
        includeFirst = true;
        return defaultSubCommand;
      }
      return findSubCmd(argv[1]);
    }();

    if (!activeSubCommand)
      printHelpExit(true);

    auto name = argv[0] + std::string(" ") +
                std::string(activeSubCommand->name.begin(),
                            activeSubCommand->name.end());

    size_t firstIdx = !includeFirst;
    auto old = argv[firstIdx];
    argv[firstIdx] = const_cast<char *>(name.c_str());
    activeSubCommand->handler.parse(argc - firstIdx, argv + firstIdx);
    argv[firstIdx] = old;

    return activeSubCommand->idx;
  }

  SubCommand *active() const { return activeSubCommand; }
  SubCommandHandler(SubCommand *defaultSubCommand = nullptr)
      : defaultSubCommand(defaultSubCommand) {}
};
