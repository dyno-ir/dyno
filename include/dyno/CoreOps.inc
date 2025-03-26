// Header guards explicitly unwanted!

#ifndef HEADER
#define HEADER enum CoreOps {
#endif

#ifndef FOOTER
#define FOOTER };
#endif

#ifndef ADD_OP
#define ADD_OP(x) x
#endif

HEADER

ADD_OP(add),
ADD_OP(sub),
ADD_OP(mul),
ADD_OP(udiv),
ADD_OP(sdiv),

ADD_OP(and),
ADD_OP(or),
ADD_OP(xor),

ADD_OP(sll),
ADD_OP(srl),
ADD_OP(sra),

ADD_OP(load),
ADD_OP(store)

FOOTER

#undef HEADER
#undef FOOTER
#undef ADD_OP
