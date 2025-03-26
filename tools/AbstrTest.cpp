#include "hw/HWAbstraction.h"


using namespace dyno;

int main()
{
    HWContext ctx;
    HWInstrBuilder build{ctx};

    auto add1 = build.buildAdd(build.buildConst32(20), build.buildConst32(21));
    auto add2 = build.buildAdd(add1.def()->fat<Wire>(), build.buildConst32(1));
    auto sub = build.buildSub(add2.def()->fat<Wire>(), add1.def()->fat<Wire>());

    HWPrinter print;

    print.printCtx(ctx);
}
