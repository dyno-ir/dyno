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

ADD_OP(MODULE_DEF)
ADD_OP(BLACKBOX_MODULE_DEF)
ADD_OP(STDCELL_DEF)

ADD_OP(INIT_PROCESS_DEF)
ADD_OP(COMB_PROCESS_DEF)
ADD_OP(SEQ_PROCESS_DEF)
ADD_OP(LATCH_PROCESS_DEF)
ADD_OP(FINAL_PROCESS_DEF)
ADD_OP(NETLIST_PROCESS_DEF)

ADD_OP(REGISTER_DEF)
ADD_OP(INPUT_REGISTER_DEF)
ADD_OP(OUTPUT_REGISTER_DEF)
ADD_OP(INOUT_REGISTER_DEF)
ADD_OP(REF_REGISTER_DEF)
ADD_OP(TRIGGER_DEF)

ADD_OP(INSTANCE)
ADD_OP(STDCELL_INSTANCE)

ADD_OP(LOAD)

ADD_OP(STORE)
ADD_OP(PRINT)
ADD_OP(STORE_DEFER)
ADD_OP(ASSERT_DEFER)
ADD_OP(PRINT_DEFER)

// out | sel true_val false_val
ADD_OP(MUX)

ADD_OP(CASE_X)
ADD_OP(CASE_Z)

ADD_OP(SPLICE)
ADD_OP(INSERT)
ADD_OP(CONCAT)
ADD_OP(REPEAT)
ADD_OP(RED_XOR)
ADD_OP(ADD_COMPRESS)
ADD_OP(ADD_CARRY)

ADD_OP(UPOW)
ADD_OP(SPOW)

ADD_OP(CLOG2)

// Get Element Pointer. Encodes linear address calculation expression
// `base + (idx * mult)...` with known bounds (max).
// ptr | base (idx mult max)
ADD_OP(GEP)

// q | clk d en (rst rst_val)...
ADD_OP(FLIP_FLOP)
// q | clk d en (rst rst_val)...
ADD_OP(FLIP_FLOP_SRST)
ADD_OP(LATCH)

// out | (select, value)...
ADD_OP(ONEHOT_MUX)

// Forward value if condition met, else 'x.
// out | value condition
ADD_OP(ASSUME)

ADD_OP(MEM_LOAD)
ADD_OP(MEM_STORE)

ADD_OP(MEMORY_DEF)
ADD_OP(READ_PORT_DEF)
ADD_OP(WRITE_PORT_DEF)
ADD_OP(READ_WRITE_PORT_DEF)
ADD_OP(WRITE_FORWARD)

// out:wire | truth_table:constant bool_input:wire...
ADD_OP(LUT)

FOOTER

#undef HEADER
#undef FOOTER
#undef ADD_OP
