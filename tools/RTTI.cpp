#include "support/RTTI.h"
#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/HWAbstraction.h"
#include "hw/IDs.h"

using namespace dyno;

/*
struct Base
{
    int kind;
};

struct DerivedA : Base
{
    static bool is_impl(const Base& Base)
    {
        return Base.kind == 1;
    }
};

struct DerivedB : Base
{
    static bool is_impl(const Base& Base)
    {
        return Base.kind == 2;
    }
};*/

int main() {
  Context ctx;
  FatObjRef<Instr> instr = ctx.getStore<Instr>().create(0, OP_ADD);
  DynObjRef instrDyn = instr;

  ObjRef<Instr>::is_impl(instrDyn);

  FatDynObjRef<Instr> fatDyn{instrDyn, instr.getPtr()};

  // dyn thin to thin
  ObjRef<Instr> dynThinToThin = instrDyn.as<ObjRef<Instr>>();
  ObjRef<Instr> dynFatToThin = fatDyn.as<ObjRef<Instr>>();
  FatObjRef<Instr> dynFatToFat = fatDyn.as<FatObjRef<Instr>>();

  DynObjRef thinToDynThin = dynFatToThin.as<DynObjRef>();
  DynObjRef fatToDynThin = dynFatToFat.as<DynObjRef>();
  FatDynObjRef<Instr> fatToDynFat = dynFatToFat.as<FatDynObjRef<Instr>>();

  if (auto asBlock = dyn_as<ObjRef<Block>>(instrDyn)) {
    assert(0 && "unreachable");
  }
  if (auto asBlock = dyn_as<ObjRef<Instr>>(instrDyn)) {

  } else
    assert(0 && "unreachable");

  if (auto asBlock = instrDyn.dyn_as<ObjRef<Block>>()) {
    assert(0 && "unreachable");
  }
  if (auto asInstr = fatDyn.dyn_as<InstrRef>()) {

  } else
    assert(0 && "unreachable");
}
