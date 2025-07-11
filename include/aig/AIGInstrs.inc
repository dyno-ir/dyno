// Header guards explicitly unwanted!
#ifndef HEADER
#define HEADER enum AIGOps {
#endif

#ifndef FOOTER
#define FOOTER };
#endif

#ifndef ADD_OP
#define ADD_OP(x) x
#endif


HEADER

ADD_OP(GRAPH)
ADD_OP(INPUT)
ADD_OP(OUTPUT)

FOOTER

#undef HEADER
#undef FOOTER
#undef ADD_OP
