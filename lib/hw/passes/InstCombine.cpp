#include "hw/passes/InstCombine.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
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
  return (*use)->as<FatDynObjRef<InstrDefUse>>()->getSingleDef()->instr();
}

static void deleteF(SmallVecImpl<InstrRef> &matched, Context &ctx,
                    InstrRef ref) {
  // for (auto op : ref)
  //   op.replace(FatDynObjRef<>{nullref});
  InstCombinePass::TaggedIRef{ref}.get() = 1;
  matched.emplace_back(ref);
}

static void deleteIfSingleUse(SmallVecImpl<InstrRef> &matched, Context &ctx,
                              InstrRef ref) {

  // this is only valid for direct users replaced values.
  // if (ref.def(0)->as<WireRef>().getNumUses() == 1) {
  //   deleteF(matched, ctx, ref);
  //   return;
  // }
  // matched.emplace_back(ref);

  // for now simply ignore, will be re-inspected.
}

bool dyno::InstCombinePass::generated(HWInstrRef r0) {

  auto &matched = currentMatched;
  auto &replaced = currentReplaced;

  auto replaceAllUses = [this](SmallVecImpl<OperandRef> & /*unused*/,
                               InstrRef::iterator oldOp, auto newOp) {
    replaceUses((*oldOp)->as<WireRef>(),
                (*newOp).template as<FatDynObjRef<>>());
  };

  HWInstrBuilder build{ctx};
  build.setInsertPoint(r0.iter(ctx));
  auto cbuild = ConstantBuilder{ctx.getStore<Constant>()};

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-label"
#pragma clang diagnostic ignored "-Wunused-but-set-variable"
#include "InstCombine.inc"
#pragma clang diagnostic pop

  return false;
}
