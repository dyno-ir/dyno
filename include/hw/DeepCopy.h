#pragma once
#include "dyno/CFG.h"
#include "dyno/Obj.h"
#include "hw/HWAbstraction.h"
#include "support/DenseMap.h"
#include "hw/HWPrinter.h"

namespace dyno {

class DeepCopier {
public:
  HWContext &ctx;

  // we actually don't need to store dialect/ty twice so could just store
  // objID for new. Then we need another type switch to get the ptr though, so
  // just store a full FatDynObjRef.
  DenseMap<DynObjRef, FatDynObjRef<>> oldToNewMap;

  template <
      std::invocable<DeepCopier *, InstrRef, BlockRef_iterator<true>> InstrHook>
  FatDynObjRef<> deepCopyObj(FatDynObjRef<> obj, InstrHook &&instrCallback) {

    if (!obj)
      return obj;

    switch (*obj.getType()) {
    case *CORE_BLOCK: {
      auto asBlock = obj.as<BlockRef>();
      BlockRef blockRef = ctx.createBlock();
      blockRef.reserve(asBlock.size());
      deepCopyInstrsImpl(asBlock.begin(), blockRef.begin(), instrCallback);
      return blockRef;
    }
    case *OP_FUNC: {
      // do not clone functions
      return obj;
    }
    case *HW_WIRE: {
      auto asWire = obj.as<WireRef>();
      return ctx.getWires().create(asWire->numBits);
    }
    case *HW_REGISTER: {
      auto asReg = obj.as<RegisterRef>();
      return ctx.getWires().create(asReg->numBits);
    }
    default:
      dyno_unreachable("copying is not supported");
    }
  }

private:
  template <
      std::invocable<DeepCopier *, InstrRef, BlockRef_iterator<true>> InstrHook>
  void deepCopyInstrsImpl(BlockRef_iterator<true> srcIt,
                          BlockRef_iterator<true> dstIt,
                          InstrHook &&instrCallback) {
    // insert after the passed iter
    dstIt++;

    while (srcIt != srcIt.blockRef().end()) {
      auto srcInstr = srcIt.instr();

      if (instrCallback(this, srcInstr, dstIt)) {
        srcIt++;
        continue;
      }

      auto copyInstr = InstrRef{ctx.getInstrs().create(
          srcInstr.getNumOperands(), srcInstr.getDialectOpcode())};
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
        build.addRef(ref);
      }

      srcIt++;
    }
  }

public:
  void deepCopyInstrs(BlockRef_iterator<true> srcIt,
                      BlockRef_iterator<true> dstIt) {
    oldToNewMap.clear();
    deepCopyInstrsImpl(
        srcIt, dstIt,
        [](DeepCopier *, InstrRef, BlockRef_iterator<true>) { return false; });
  }

  template <std::invocable<DeepCopier *, InstrRef, BlockRef_iterator<true>> InstrHook>
  void deepCopyInstrs(BlockRef_iterator<true> srcIt,
                      BlockRef_iterator<true> dstIt, InstrHook &&instrHook) {
    oldToNewMap.clear();
    deepCopyInstrsImpl(srcIt, dstIt, instrHook);
  }

public:
  DeepCopier(HWContext &ctx) : ctx(ctx) {}
};

}; // namespace dyno
