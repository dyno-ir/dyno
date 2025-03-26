// Header Guards explicitly unwanted!

#ifndef HEADER
#define HEADER enum CoreOps {
#endif

#ifndef FOOTER
#define FOOTER }
#endif

#ifndef ADD_OP
#define ADD_OP(x) (x)
#endif

HEADER

ADD_OP(C_ADD),
ADD_OP(C_SUB),
ADD_OP(C_MUL),
ADD_OP(C_UDIV),
ADD_OP(C_SDIV),

ADD_OP(C_AND),
ADD_OP(C_OR),
ADD_OP(C_XOR),

ADD_OP(C_SLL),
ADD_OP(C_SRL),
ADD_OP(C_SRA),

ADD_OP(C_LOAD),
ADD_OP(C_STORE)

FOOTER
