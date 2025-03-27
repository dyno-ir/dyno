#include "dyno/Instr.h"
#include "dyno/Obj.h"
#include "hw/HWAbstraction.h"
#include "hw/IDs.h"
#include "support/RTTI.h"

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

int main()
{
    HWContext ctx;
    FatObjRef<Instr> instr = ctx.getInstrs().create(0, DialectID{DIALECT_RTL}, OpcodeID{HW_ADD});
    DynObjRef instrDyn = instr;

    ObjRef<Instr>::is_impl(instrDyn);

    std::cout << is<ObjRef<Instr>, DynObjRef>(instrDyn) << "\n";
    std::cout << is<DynObjRef, ObjRef<Instr>>(instr) << "\n";


    auto instr2 = static_cast<ObjRef<Instr>>(instrDyn);
    // guess we now need to implement ByValueRTTIMixin to call static_cast like this
    // for (Fat)(Dyn)ObjRefs while keeping normal LLVM-style RTTI for classic sitations like
    // struct Base above.
}
