#include "hw/HWAbstraction.h"


using namespace dyno;

int main()
{
    HWContext ctx;

    auto proc = ctx.createProcess();
    auto block = BlockRef{*proc->blocks().begin()->instr().def()->fat<Block>()};
    HWInstrBuilder build{ctx, block.begin()};
    auto add1 = build.buildAdd(build.buildConst32(20), build.buildConst32(21));
    auto add2 = build.buildAdd(add1.def()->fat<Wire>(), build.buildConst32(1));
    auto sub = build.buildSub(add2.def()->fat<Wire>(), add1.def()->fat<Wire>());


    auto proc2 = ctx.createProcess();
    auto block2 = BlockRef{*proc2->blocks().begin()->instr().def()->fat<Block>()};
    HWInstrBuilder build2{ctx, block2.begin()};
    auto add3 = build2.buildAdd(add1.def()->fat<Wire>(), add2.def()->fat<Wire>(), sub.def()->fat<Wire>());


    HWPrinter print;

    print.printCtx(ctx);
}
