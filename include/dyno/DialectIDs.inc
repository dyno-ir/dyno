// Header guards explicitly unwanted!

#ifndef HEADER
#define HEADER enum DialectIDs {
#endif

#ifndef FOOTER
#define FOOTER };
#endif

#ifndef ADD_OP
#define ADD_OP(x) x
#endif

#ifndef LAST
#define LAST ADD_OP(NUM_DIALECTS)
#endif

HEADER

ADD_OP(DIALECT_CORE),
ADD_OP(DIALECT_SCF),
ADD_OP(DIALECT_CUSTOM),

LAST
FOOTER

#undef HEADER
#undef FOOTER
#undef ADD_OP
#undef LAST
