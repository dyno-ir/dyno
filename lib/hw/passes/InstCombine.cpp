#include "hw/passes/InstCombine.h"
#include "hw/HWAbstraction.h"
#include "hw/Wire.h"

using namespace dyno;

// static void appendCopy(SmallVecImpl<FatDynObjRef<>> &list, FatDynObjRef<>
// obj) {
//   list.emplace_back(obj);
// }
static void appendCopy(SmallVecImpl<FatDynObjRef<>> &list,
                       InstrRef::iterator obj) {
  list.emplace_back((*obj)->fat());
}
static void copyOperands(InstrBuilder &build, InstrRef::iterator begin,
                         InstrRef::iterator end) {
  for (auto it = begin; it != end; it++)
    build.addRef((*it)->fat());
}
static void copyOperands(InstrBuilder &build, FatDynObjRef<> *begin,
                         FatDynObjRef<> *end) {

  for (auto it = begin; it != end; it++)
    build.addRef(*it);
}
static void copyOperand(InstrBuilder &build, FatDynObjRef<> ref) {
  build.addRef(ref);
}

static InstrRef getDefInstr(InstrRef::iterator use) {
  return (*use)->as<WireRef>().getDefI();
}

static void deleteF(SmallVecImpl<InstrRef> &matched, HWContext &ctx,
                    InstrRef ref) {
  //for (auto op : ref)
  //  op.replace(FatDynObjRef<>{nullref});
  //InstCombinePass::TaggedIRef{ref}.get() = 1;
  matched.emplace_back(ref);
}

static void deleteIfSingleUse(SmallVecImpl<InstrRef> &matched, HWContext &ctx,
                              InstrRef ref) {
  if (ref.def(0)->as<WireRef>().getNumUses() == 1) {
    deleteF(matched, ctx, ref);
    return;
  }
  matched.emplace_back(ref);
}

static void replaceAllUses(SmallVecImpl<OperandRef> &replaced,
                           InstrRef::iterator oldOp, InstrRef::iterator newOp) {
  (*oldOp)->as<WireRef>()->defUse.replaceAllUsesWith(
      (*newOp)->fat(), [&replaced](OperandRef r) { replaced.emplace_back(r); });
}

static void replaceAllUses(SmallVecImpl<OperandRef> &replaced,
                           InstrRef::iterator oldOp, FatDynObjRef<> *newOp) {
  (*oldOp)->as<WireRef>()->defUse.replaceAllUsesWith(
      *newOp, [&replaced](OperandRef r) { replaced.emplace_back(r); });
}

static void replaceAllUses(SmallVecImpl<OperandRef> &replaced,
                           InstrRef::iterator oldOp, ConstantRef newOp) {
  (*oldOp)->as<WireRef>()->defUse.replaceAllUsesWith(
      newOp, [&replaced](OperandRef r) { replaced.emplace_back(r); });
}

bool dyno::generated(HWContext &ctx, SmallVecImpl<InstrRef> &matched,
                     SmallVecImpl<OperandRef> &replaced, HWInstrRef r0) {

  HWInstrBuilder build{ctx};
  build.setInsertPoint(r0.iter(ctx));
  auto cbuild = ctx.constBuild();

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-label"
#pragma clang diagnostic ignored "-Wunused-but-set-variable"
#include "InstCombine.inc"
#pragma clang diagnostic pop

  return false;
}
