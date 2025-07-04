#pragma once

#include "hw/HWContext.h"
namespace dyno {

// Maintains a debug info stack and automatically adds debug info(s) on the top
// of the stack to all newly created instructions via a hook.
class AutoDebugInfoStack {
  HWContext &ctx;
  struct LazyDebugInfo {
    std::string_view name;
    uint32_t beginLine;
    uint32_t beginCol;
    uint32_t endLine;
    uint32_t endCol;
    bool isNewEntry;

    static LazyDebugInfo empty() {
      return LazyDebugInfo{std::string_view{}, 0, 0, 0, 0, true};
    }

    explicit operator bool() { return beginLine != 0; }
  };

  void instrCreateHook(InstrRef instr) {
    for (size_t i = stack.size(); i-- > 0;) {
      auto entry = stack[i];
      if (entry)
        ctx.dbgInfo.addSrcLoc(instr, entry.name, entry.beginLine,
                              entry.beginCol, entry.endLine, entry.endCol);
      if (entry.isNewEntry)
        break;
    }
  }

  std::function<void(InstrRef)> getBoundHook() {
    return std::bind(&AutoDebugInfoStack::instrCreateHook, this,
                     std::placeholders::_1);
  }

  void registerHook() {
    ctx.getInstrs().createHooks.emplace_back(getBoundHook());
  }

  void removeHook() { ctx.getInstrs().createHooks.pop_back(); }

  SmallVec<LazyDebugInfo, 16> stack;

public:
  void pushDebugInfo(std::string_view name, uint32_t beginLine,
                     uint32_t beginCol, uint32_t endLine, uint32_t endCol) {
    stack.emplace_back(name, beginLine, beginCol, endLine, endCol, true);
  }
  void pushEmpty() { stack.emplace_back(LazyDebugInfo::empty()); }

  void addDebugInfo(std::string_view name, uint32_t beginLine,
                    uint32_t beginCol, uint32_t endLine, uint32_t endCol,
                    bool isNew = false) {
    stack.emplace_back(name, beginLine, beginCol, endLine, endCol, isNew);
  }

  void popDebugInfo() {
    while (!stack.empty()) {
      auto first = stack.pop_back_val().isNewEntry;
      if (first)
        break;
    }
  }

  explicit AutoDebugInfoStack(HWContext &ctx) : ctx(ctx) { registerHook(); }
  ~AutoDebugInfoStack() { removeHook(); }
};

}; // namespace dyno
