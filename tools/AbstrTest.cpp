#include "dyno/Obj.h"
#include "hw/HWAbstraction.h"


using namespace dyno;

int main()
{
    HWContext ctx;

    auto proc = ctx.createProcess();
    auto block = proc->blocks().begin()->instr().def()->fat().as<BlockRef>();
    HWInstrBuilder build{ctx, block.begin()};
    auto add1 = build.buildAdd(build.buildConst32(20), build.buildConst32(21));
    auto add2 = build.buildAdd(add1.def()->fat(), build.buildConst32(1));
    auto sub = build.buildSub(add2.def()->fat(), add1.def()->fat());


    auto proc2 = ctx.createProcess();
    auto block2 = proc2->blocks().begin()->instr().def()->fat().as<BlockRef>();
    HWInstrBuilder build2{ctx, block2.begin()};
    auto add3 = build2.buildAdd(add1.def()->fat(), add2.def()->fat(), sub.def()->fat());

    HWPrinter print;

    print.printCtx(ctx);
}
