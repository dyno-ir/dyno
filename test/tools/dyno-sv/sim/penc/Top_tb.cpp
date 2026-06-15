#include "sim_header.h"
#include "dyno/Context.h"
#include "dyno/FatContext.h"
#include "hw/HWContext.h"
#include "hw/HWTypeIDs.h"
#include "hw/SimHeader.h"
#include "hw/SimTop.h"
#include "hw/passes/ParseDyno.h"
#include "meta/MetaContext.h"
#include "op/OpContext.h"
#include "aig/AIGContext.h"
#include "type/TypeContext.h"
#include <cstdio>

int main(int argc, char** argv) {
    dyno::FatContext ctx;
    ctx.add<dyno::CoreDialectContext>();
    ctx.add<dyno::MetaDialectContext>();
    ctx.add<dyno::OpDialectContext>();
    ctx.add<dyno::HWDialectContext>();
    ctx.add<dyno::AIGDialectContext>();
    ctx.add<dyno::TypeDialectContext>();

    ctx.getCtx<dyno::TypeDialectContext>().baseTypeNames.registerDialect(
        dyno::DIALECT_HW, dyno::hw::hwTypeDialectTypeNames);

    const char* dynoFile = argc > 1 ? argv[1] : "design.dyno";

    dyno::ParseDynoPass parseDyno{ctx};
    parseDyno.config.fileName = dynoFile;
    parseDyno.run();

    auto top = std::make_unique<SimulationTop<ModInst____Top>>(ctx);

    printf("=== PriorityEncoder(BITS=32, N=1) Simulation ===\n");

    struct { uint32_t in; uint32_t expIdx; bool expValid; } tests[] = {
        {0x00000000, 0, false},
        {0x00000001, 0, true},
        {0x00000002, 1, true},
        {0x00000004, 2, true},
        {0x80000000, 31, true},
        {0xAA55AA55, 0, true},
        {0xFFFFFFFF, 0, true},
        {0x00010000, 16, true},
    };

    int passed = 0;
    for (size_t i = 0; i < sizeof(tests) / sizeof(tests[0]); i++) {
        top->top->IN_data = tests[i].in;

        top->eval();

        unsigned idx = (unsigned)top->top->OUT_idx;
        bool valid = (bool)top->top->OUT_idxValid;

        bool ok = (valid == tests[i].expValid) &&
                  (!valid || idx == tests[i].expIdx);
        printf("test %zu: in=0x%08x idx=%u valid=%d %s\n",
               i, tests[i].in, idx, valid, ok ? "OK" : "FAIL");
        if (ok) passed++;
    }

    printf("%d/%d tests passed\n", passed,
           (int)(sizeof(tests) / sizeof(tests[0])));

    printf("=== Done ===\n");
    return passed == (int)(sizeof(tests) / sizeof(tests[0])) ? 0 : 1;
}
