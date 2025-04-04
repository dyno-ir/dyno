#include "dyno/Obj.h"
#include "hw/HWAbstraction.h"


using namespace dyno;

int main()
{
    HWContext ctx;

    auto mod = ctx.createModule("test");
    auto portIn = ctx.createRegister(mod);
    mod.addPort(portIn, Register::PORT_IN);

    auto portOut = ctx.createRegister(mod);
    mod.addPort(portOut, Register::PORT_OUT);

    auto reg = ctx.createRegister(mod);



    auto proc = ctx.createProcess(mod);
    auto block = proc.blocks().begin()->getRef().as<InstrRef>().def()->as<BlockRef>();
    HWInstrBuilder build{ctx, block.begin()};
    auto add1 = build.buildAdd(build.buildConst(32, 20), build.buildConst(32, 21));
    auto add2 = build.buildAdd(add1.defW(), build.buildConst(32, 1));
    auto sub = build.buildSub(add2.defW(), add1.defW());
    auto store = build.buildStore(reg, sub.defW());


    auto proc2 = ctx.createProcess(mod);
    auto block2 = proc2.blocks().begin()->getRef().as<InstrRef>().def()->as<BlockRef>();

    build.setInsertPoint(block2.begin());
    auto load = build.buildLoad(reg);
    auto add3 = build.buildAdd(load.defW(), build.buildConst(32, 1));
    auto ifelse = build.buildIfElse(add3.defW());

    build.setInsertPoint(ifelse.getTrueBlock().begin());
    ifelse = build.buildSCFYield(ifelse.getSCFConstruct(), build.buildConst(32, 42)).second;

    build.setInsertPoint(ifelse.getFalseBlock().begin());
    ifelse = build.buildSCFYield(ifelse.getSCFConstruct(), build.buildConst(64, 1337)).second;

    build.setInsertPoint(block2.end());
    build.buildStore(mod->ports[1], ifelse.getYieldValue());


    //auto endIt = ifelse.getFalseBlock().end();
    //--endIt;
    //ctx.getInstrs().destroy(endIt.instr());
    //endIt.erase();


    auto block3 = ctx.createProcess(mod).blocks().begin()->getRef().as<InstrRef>().def()->as<BlockRef>();
    build.setInsertPoint(block3.begin());
    auto whileInstr = build.buildWhile(build.buildConst(32, 128));
    build.setInsertPoint(whileInstr.getCondBlock().begin());
    auto sub2 = build.buildSub(whileInstr.getYieldValue(0), build.buildConst(32, 1));
    build.buildSCFYield(whileInstr.getSCFConstruct(), sub2.defW(), /*todo: convert to bool*/sub2.defW());
    build.setInsertPoint(whileInstr.getBodyBlock().begin());
    build.buildStore(reg, whileInstr.getYieldValue(0));
    build.buildSCFYield(whileInstr.getSCFConstruct(), whileInstr.getYieldValue(0));


    auto func = build.buildFunc(mod);
    build.setInsertPoint(func.getBlock().begin());
    auto param = build.buildFuncParam(func.func());
    build.buildFuncReturn(func.func(), param.defW(), build.buildConst(32, 1));


    HWPrinter print;

    print.printCtx(ctx);

    auto pblock = add3.parentBlock(ctx);
    assert(pblock.as<FatDynObjRef<>>() == block2.as<FatDynObjRef<>>());

    auto pproc = add3.parentProc(ctx);
    assert(pproc.as<FatDynObjRef<>>() == proc2.as<FatDynObjRef<>>());
}
