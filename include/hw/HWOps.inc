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

ADD_OP(ADD),
ADD_OP(SUB),
ADD_OP(MUL),
ADD_OP(UDIV),
ADD_OP(SDIV),

ADD_OP(AND),
ADD_OP(OR),
ADD_OP(XOR),

ADD_OP(SLL),
ADD_OP(SRL),
ADD_OP(SRA),

ADD_OP(LOAD),
ADD_OP(STORE)

FOOTER

#undef HEADER
#undef FOOTER
#undef ADD_OP
