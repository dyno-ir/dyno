#include "dyno/Obj.h"
#include "hw/HWAbstraction.h"


using namespace dyno;

int main()
{
    HWContext ctx;

    auto mod = ctx.createModule("test");

    auto proc = ctx.createProcess(mod);
    auto block = proc->blocks().begin()->instr().def()->as<BlockRef>();
    HWInstrBuilder build{ctx, block.begin()};
    auto add1 = build.buildAdd(build.buildConst32(20), build.buildConst32(21));
    auto add2 = build.buildAdd(add1.defW(), build.buildConst32(1));
    auto sub = build.buildSub(add2.defW(), add1.defW());


    auto proc2 = ctx.createProcess(mod);
    auto block2 = proc2->blocks().begin()->instr().def()->as<BlockRef>();
    HWInstrBuilder build2{ctx, block2.begin()};
    auto add3 = build2.buildAdd(add1.defW(), add2.defW(), sub.defW());

    HWPrinter print;

    print.printCtx(ctx);

    auto pblock = add3.parentBlock(ctx);
    assert(pblock.as<FatDynObjRef<>>() == block2.as<FatDynObjRef<>>());

    auto pproc = add3.parentProc(ctx);
    assert(pproc.as<FatDynObjRef<>>() == proc2.as<FatDynObjRef<>>());
}
