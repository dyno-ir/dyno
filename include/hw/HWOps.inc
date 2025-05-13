// Header guards explicitly unwanted!

#ifndef HEADER
#define HEADER enum HWOps {
#endif

#ifndef FOOTER
#define FOOTER };
#endif

#ifndef ADD_OP
#define ADD_OP(x) x
#endif

HEADER

ADD_OP(MODULE_INSTR),
ADD_OP(PROCESS_INSTR),
ADD_OP(BLOCK_INSTR),
ADD_OP(REGISTER_INSTR),

ADD_OP(LOAD),
ADD_OP(STORE),
ADD_OP(STORE_DEFER),

ADD_OP(INPUT),
ADD_OP(OUTPUT),

ADD_OP(CONCAT),
ADD_OP(SLICE),

FOOTER

#undef HEADER
#undef FOOTER
#undef ADD_OP
