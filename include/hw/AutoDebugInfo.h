#pragma once

#include "hw/HWContext.h"
namespace dyno {

// Maintains a debug info stack and automatically adds debug info(s) on the top
// of the stack to all newly created instructions via a hook.

struct SimpleDebugInfo {
  bool isNewEntry;
  std::string_view name;
  uint32_t beginLine;
  uint32_t beginCol;
  uint32_t endLine;
  uint32_t endCol;

  static SimpleDebugInfo empty() {
    return SimpleDebugInfo{true, std::string_view{}, 0, 0, 0, 0};
  }

  void addToInstr(HWContext &ctx, InstrRef instr) {
    ctx.dbgInfo.addSrcLoc(instr, name, beginLine, beginCol, endLine, endCol);
  }

  bool isStartEntry() { return isNewEntry; }

  explicit operator bool() { return beginLine != 0; }
};

struct CopyDebugInfo {
  bool isNewEntry;
  InstrRef src;

  static CopyDebugInfo empty() { return CopyDebugInfo{true, nullref}; }

  void addToInstr(HWContext &ctx, InstrRef instr) {
    ctx.dbgInfo.copyDebugInfo(src, instr);
  }

  bool isStartEntry() { return isNewEntry; }

  explicit operator bool() { return !!src; }
};

template <typename TempInfoT> class AutoDebugInfoStackBase {
  HWContext &ctx;

  void instrCreateHook(InstrRef instr) {
    for (size_t i = stack.size(); i-- > 0;) {
      auto entry = stack[i];
      if (entry)
        entry.addToInstr(ctx, instr);
      if (entry.isStartEntry())
        break;
    }
  }

  std::function<void(InstrRef)> getBoundHook() {
    return std::bind(&AutoDebugInfoStackBase::instrCreateHook, this,
                     std::placeholders::_1);
  }

  void registerHook() {
    ctx.getInstrs().createHooks.emplace_back(getBoundHook());
  }

  void removeHook() { ctx.getInstrs().createHooks.pop_back(); }

  SmallVec<TempInfoT, 16> stack;

public:
  template <typename... Args> void pushDebugInfo(Args... args) {
    stack.emplace_back(true, std::forward<Args>(args)...);
  }
  template <typename... Args>
  void addDebugInfo(bool isNew = false, Args... args) {
    stack.emplace_back(isNew, std::forward<Args>(args)...);
  }
  void pushEmpty() { stack.emplace_back(TempInfoT::empty()); }

  void popDebugInfo() {
    while (!stack.empty()) {
      auto first = stack.pop_back_val().isStartEntry();
      if (first)
        break;
    }
  }

  struct ScopeToken {
    ScopeToken(const ScopeToken &) = delete;
    ScopeToken(ScopeToken &&) = default;
    ScopeToken &operator=(const ScopeToken &) = delete;
    ScopeToken &operator=(ScopeToken &&) = default;

    AutoDebugInfoStackBase &base;
    ~ScopeToken() { base.popDebugInfo(); }
    ScopeToken(AutoDebugInfoStackBase &base) : base(base) {}
  };

  template <typename... Args> ScopeToken addWithToken(Args... args) {
    addDebugInfo(true, std::forward<Args>(args)...);
    return ScopeToken{*this};
  }

  explicit AutoDebugInfoStackBase(HWContext &ctx) : ctx(ctx) { registerHook(); }
  ~AutoDebugInfoStackBase() { removeHook(); }
};

using AutoDebugInfoStack = AutoDebugInfoStackBase<SimpleDebugInfo>;
using AutoCopyDebugInfoStack = AutoDebugInfoStackBase<CopyDebugInfo>;

}; // namespace dyno
