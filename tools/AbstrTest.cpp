#include "dyno/Obj.h"
#include "hw/HWAbstraction.h"


using namespace dyno;

int main()
{
    HWContext ctx;

    auto mod = ctx.createModule("test");

    auto reg = ctx.createRegister(mod);

    auto proc = ctx.createProcess(mod);
    auto block = proc.blocks().begin()->instr().def()->as<BlockRef>();
    HWInstrBuilder build{ctx, block.begin()};
    auto add1 = build.buildAdd(build.buildConst(32, 20), build.buildConst(32, 21));
    auto add2 = build.buildAdd(add1.defW(), build.buildConst(32, 1));
    auto sub = build.buildSub(add2.defW(), add1.defW());
    auto store = build.buildStore(reg, sub.defW());


    auto proc2 = ctx.createProcess(mod);
    auto block2 = proc2.blocks().begin()->instr().def()->as<BlockRef>();

    build.setInsertPoint(block2.begin());
    auto load = build.buildLoad(reg);
    auto add3 = build.buildAdd(load.defW(), build.buildConst(32, 1));
    auto ifelse = build.buildIfElse(add3.defW());

    build.setInsertPoint(ifelse.getTrueBlock().begin());
    ifelse = build.buildSCFYield(ifelse.getSCFConstruct(), build.buildConst(32, 42)).second;

    build.setInsertPoint(ifelse.getFalseBlock().begin());
    ifelse = build.buildSCFYield(ifelse.getSCFConstruct(), build.buildConst(64, 1337)).second;

    //auto endIt = ifelse.getFalseBlock().end();
    //--endIt;
    //ctx.getInstrs().destroy(endIt.instr());
    //endIt.erase();


    build.setInsertPoint(block2.end());
    build.buildStore(reg, ifelse.getYieldValue());

    HWPrinter print;

    print.printCtx(ctx);

    auto pblock = add3.parentBlock(ctx);
    assert(pblock.as<FatDynObjRef<>>() == block2.as<FatDynObjRef<>>());

    auto pproc = add3.parentProc(ctx);
    assert(pproc.as<FatDynObjRef<>>() == proc2.as<FatDynObjRef<>>());
}
