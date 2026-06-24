#pragma once

#include "CFG.h"
#include "Context.h"
#include "Instr.h"
#include "support/TempBind.h"

namespace dyno {

class CoreBuilder {
  BlockRef::iterator insertIt{nullref};

public:
  Context &ctx;
  InstrStore &instrs;
  CFG &cfg;

  CoreBuilder(Context &ctx)
      : ctx(ctx), instrs(ctx.getStore<Instr>()),
        cfg(ctx.getCtx<CoreDialectContext>().cfg) {}

  template <typename It> CoreBuilder(Context &ctx, It it) : CoreBuilder(ctx) {
    setInsertIt(it);
  }

  BlockRef::iterator getInsertIt() { return insertIt; }
  void setInsertIt(BlockRef::iterator it) { insertIt = it; }
  void setInsertIt(InstrRef ref) { insertIt = cfg[ref]; }
  void setInsertIt(BlockRef ref) { insertIt = ref.end(); }

  void insertInstr(InstrRef instr) {
    if (!insertIt) [[unlikely]]
      return;
    insertIt.insertPrev(instr);
  }

  InstrBuilder buildInstrRaw(DialectOpcode opc, unsigned numOperands) {
    auto instr = InstrRef{instrs.create(numOperands, opc)};
    insertInstr(instr);
    return InstrBuilder{instr};
  }

  template <typename Def = FatDynObjRef<>, typename Other = FatDynObjRef<>>
  InstrBuilder buildInstrRaw(DialectOpcode opc, ArrayRef<Def> defs,
                             ArrayRef<Other> others,
                             unsigned numExtraOperands = 0) {
    auto instr = InstrRef{
        instrs.create(defs.size() + others.size() + numExtraOperands, opc)};
    InstrBuilder instrB{instr};
    instrB.addRefs(defs);
    instrB.other();
    instrB.addRefs(others);
    insertInstr(instr);
    return instrB;
  }

  template <typename Def = FatDynObjRef<>>
  InstrBuilder buildInstrRaw(DialectOpcode opc, ArrayRef<Def> defs,
                             unsigned numExtraOperands = 0) {
    auto instr = InstrRef{instrs.create(defs.size() + numExtraOperands, opc)};
    InstrBuilder instrB{instr};
    instrB.addRefs(defs);
    insertInstr(instr);
    return instrB;
  }
};

template <typename Base> class BuilderStack : public Base {
  friend class RAIIToken<BuilderStack>;

  SmallVec<BlockRef::iterator, 16> stack;

public:
  using Base::Base;

  template <typename T> void pushInsertIt(T newInsertIt) {
    stack.emplace_back(Base::getInsertIt());
    Base::setInsertIt(newInsertIt);
  }
  void popInsertIt() { Base::setInsertIt(stack.pop_back_val()); }

  template <typename It>
  [[nodiscard]] RAIIToken<BuilderStack> changeInsertIt(It newInsertIt) {
    pushInsertIt(newInsertIt);
    return RAIIToken{*this};
  }

protected:
  void unbind() { popInsertIt(); }
};
} // namespace dyno
