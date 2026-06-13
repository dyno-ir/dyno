#pragma once
#include "dyno/CFG.h"
#include "dyno/Context.h"
#include "dyno/IDs.h"
#include "dyno/Obj.h"
#include "support/DenseMap.h"

namespace dyno {

class DeepCopier;

template <typename T>
concept IsCopyHook =
    std::is_invocable_v<T, DeepCopier *, InstrRef, BlockRef_iterator<true>>;

class DeepCopier {
public:
  // destination context
  Context &ctx;
  Context &srcCtx;

  // we actually don't need to store dialect/ty twice so could just store
  // objID for new. Then we need another type switch to get the ptr though, so
  // just store a full FatDynObjRef.
  DenseMap<DynObjRef, FatDynObjRef<>> oldToNewMap;
  uint32_t blockDepth = 0;

  static constexpr auto emptyCallback =
      [](DeepCopier *, InstrRef, BlockRef_iterator<true>) { return false; };

  template <IsCopyHook InstrHook>
  FatDynObjRef<> deepCopyObj(FatDynObjRef<> obj, InstrHook &&instrCallback) {

    if (!obj)
      return obj;

    switch (*obj.getType()) {
    case *CORE_BLOCK: {
      auto asBlock = obj.as<BlockRef>();
      BlockRef blockRef = ctx.getCtx<CoreDialectContext>().createBlock();
      blockRef.reserve(asBlock.size());
      blockDepth++;
      deepCopyInstrsImpl(asBlock.begin(), blockRef.begin(), instrCallback);
      blockDepth--;
      return blockRef;
    }
    case *CORE_CONSTANT: {
      if (&srcCtx == &ctx)
        // do not copy constants in same ctx
        return obj;
    }
      [[fallthrough]];
    default: {
      auto copy = ctx.copy(obj);
      assert(copy && "copy not implemented");
      return copy;
    }
    }
  }
  template <IsCopyHook InstrHook>
  InstrRef copyInstr(InstrRef srcInstr, BlockRef_iterator<true> dstIt,
                     InstrHook instrCallback) {
    auto copyInstr = InstrRef{ctx.getStore<Instr>().create(
        srcInstr.getNumOperands(), srcInstr.getDialectOpcode())};
    if (dstIt != BlockRef_iterator<true>::invalid())
      dstIt.insertPrev(copyInstr);
    InstrBuilder build{copyInstr};

    for (size_t i = 0; i < srcInstr.getNumDefs(); i++) {
      auto oldObj = srcInstr.def(i)->fat();
      auto newObj = deepCopyObj(oldObj, instrCallback);
      oldToNewMap.insert(oldObj, newObj);
      build.addRef(newObj);
    }

    build.other();
    for (size_t i = 0; i < srcInstr.getNumOthers(); i++) {
      auto ref = srcInstr.other(i)->fat();
      if (auto it = oldToNewMap.find(ref)) {
        ref = it.val();
      }
      assert(ref != nullref);
      build.addRef(ref);
    }

    ctx.getCtx<CoreDialectContext>().instrSourceLocInfo.copyDebugInfoOOC(
        srcCtx.getCtx<CoreDialectContext>().instrSourceLocInfo, srcInstr,
        copyInstr);
    return copyInstr;
  }

  InstrRef copyInstr(InstrRef srcInstr, BlockRef_iterator<true> dstIt) {
    return copyInstr(srcInstr, dstIt, emptyCallback);
  }

  template <IsCopyHook InstrHook>
  InstrRef moveInstr(InstrRef srcInstr, BlockRef_iterator<true> dstIt,
                     InstrHook instrCallback) {
    assert(&srcCtx == &ctx);
    dstIt.blockRef().getCFG()[srcInstr].erase();
    dstIt.insertPrev(srcInstr);
    return srcInstr;
  }

private:
  template <auto Func, IsCopyHook InstrHook>
  void copyInstrsImpl(BlockRef_iterator<true> srcIt,
                      BlockRef_iterator<true> dstIt, InstrHook &instrCallback) {
    // insert after the passed iter
    dstIt++;

    while (srcIt != srcIt.blockRef().end()) {
      auto srcInstr = srcIt.instr();

      if (instrCallback(this, srcInstr, dstIt)) {
        srcIt++;
        continue;
      }
      (this->*Func)(srcInstr, dstIt, instrCallback);
      srcIt++;
    }
  }

  template <auto Func, IsCopyHook InstrHook>
  void moveInstrsImpl(BlockRef_iterator<true> srcIt,
                      BlockRef_iterator<true> dstIt, InstrHook &instrCallback) {
    if (srcIt == srcIt.blockRef().end())
      return;
    // insert after the passed iter
    dstIt++;

    while (1) {
      auto srcInstr = srcIt.instr();
      auto nextInstr = srcIt.succ() == srcIt.blockRef().end()
                           ? nullref
                           : srcIt.succ().instr();

      if (instrCallback(this, srcInstr, dstIt)) {
        ;
      } else {
        (this->*Func)(srcInstr, dstIt, instrCallback);
      }

      if (!nextInstr)
        break;
      srcIt = ctx.getCFG()[nextInstr];
    }
  }

  template <IsCopyHook InstrHook>
  void deepCopyInstrsImpl(BlockRef_iterator<true> srcIt,
                          BlockRef_iterator<true> dstIt,
                          InstrHook &instrCallback) {
    return copyInstrsImpl<&DeepCopier::copyInstr<InstrHook>, InstrHook>(
        srcIt, dstIt, instrCallback);
  }

  template <IsCopyHook InstrHook>
  void moveInstrsImpl(BlockRef_iterator<true> srcIt,
                      BlockRef_iterator<true> dstIt, InstrHook &instrCallback) {
    return moveInstrsImpl<&DeepCopier::moveInstr<InstrHook>, InstrHook>(
        srcIt, dstIt, instrCallback);
  }

public:
  void deepCopyInstrs(BlockRef_iterator<true> srcIt,
                      BlockRef_iterator<true> dstIt) {
    oldToNewMap.clear();
    deepCopyInstrsImpl(srcIt, dstIt, emptyCallback);
  }

  template <IsCopyHook InstrHook>
  void deepCopyInstrs(BlockRef_iterator<true> srcIt,
                      BlockRef_iterator<true> dstIt, InstrHook &&instrHook) {
    oldToNewMap.clear();
    deepCopyInstrsImpl(srcIt, dstIt, instrHook);
  }

  template <IsCopyHook InstrHook>
  void moveInstrs(BlockRef_iterator<true> srcIt, BlockRef_iterator<true> dstIt,
                  InstrHook &&instrCallback) {
    moveInstrsImpl(srcIt, dstIt, instrCallback);
  }

  void moveInstrs(BlockRef_iterator<true> srcIt,
                  BlockRef_iterator<true> dstIt) {
    moveInstrsImpl(srcIt, dstIt, emptyCallback);
  }

public:
  DeepCopier(Context &ctx) : ctx(ctx), srcCtx(ctx) {}
  DeepCopier(Context &dstCtx, Context &srcCtx) : ctx(dstCtx), srcCtx(srcCtx) {}
};

}; // namespace dyno
