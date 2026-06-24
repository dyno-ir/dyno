// Header guards explicitly unwanted!

#ifndef HEADER
#define HEADER enum DSLOps {
#endif

#ifndef FOOTER
#define FOOTER };
#endif

#ifndef ADD_OP
#define ADD_OP(x) x
#endif

HEADER

ADD_OP(ASSIGN)
ADD_OP(FUNC)
ADD_OP(APPLY)
ADD_OP(STRUCT)
ADD_OP(TUPLE)
ADD_OP(LET)
ADD_OP(DEF)
ADD_OP(ACCESS)
ADD_OP(STRING_LIT)

FOOTER

#undef HEADER
#undef FOOTER
#undef ADD_OP
