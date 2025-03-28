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
  HWContext ctx;
  FatObjRef<Instr> instr =
      ctx.getInstrs().create(0, DialectID{DIALECT_RTL}, OpcodeID{HW_ADD});
  DynObjRef instrDyn = instr;

  ObjRef<Instr>::is_impl(instrDyn);

  FatDynObjRef<Instr> fatDyn{instrDyn, instr.getPtr()};

  // dyn thin to thin
  ObjRef<Instr> dynThinToThin = as<ObjRef<Instr>>(instrDyn);

  // dyn fat to thin
  ObjRef<Instr> dynFatToThin = as<ObjRef<Instr>>(fatDyn);

  // dyn fat to fat
  FatObjRef<Instr> dynFatToFat = as<FatObjRef<Instr>>(fatDyn);

  DynObjRef thinToDynThin = as<DynObjRef>(dynFatToThin);
  DynObjRef fatToDynFat = as<DynObjRef>(dynFatToFat);

  if (auto asBlock = dyn_as<ObjRef<Block>>(instrDyn)) {
    assert(0 && "unreachable");
  }
  if (auto asBlock = dyn_as<ObjRef<Instr>>(instrDyn)) {

  } else assert(0 && "unreachable");
}
