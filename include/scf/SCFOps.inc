// Header guards explicitly unwanted!

#ifndef HEADER
#define HEADER enum SCFOps {
#endif

#ifndef FOOTER
#define FOOTER };
#endif

#ifndef ADD_OP
#define ADD_OP(x) x
#endif

HEADER

ADD_OP(IF),
ADD_OP(WHILE),
ADD_OP(YIELD),
ADD_OP(YIELD_COND), // yield with additional bool continue arg, for while loop

ADD_OP(FUNC_INSTR),
ADD_OP(PARAM),
ADD_OP(RETURN),
ADD_OP(CALL),

FOOTER

#undef HEADER
#undef FOOTER
#undef ADD_OP
