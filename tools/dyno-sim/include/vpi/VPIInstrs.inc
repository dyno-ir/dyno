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

FOOTER

#undef HEADER
#undef FOOTER
#undef ADD_OP
