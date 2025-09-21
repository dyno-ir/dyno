typedef enum logic[3:0]
{
    // Flags that do not cause a flush or trap
    FLAGS_NONE, FLAGS_BRANCH,

    // Flags for sending direction prediction updates
    FLAGS_PRED_TAKEN, FLAGS_PRED_NTAKEN,

    // Flags that cause a flush
    FLAGS_FENCE, FLAGS_ORDERING,

    // Flags that cause a trap
    FLAGS_ILLEGAL_INSTR, FLAGS_TRAP,

    // Memory Exceptions
    FLAGS_LD_MA, FLAGS_LD_AF, FLAGS_LD_PF,
    FLAGS_ST_MA, FLAGS_ST_AF, FLAGS_ST_PF,

    // Return from exception
    FLAGS_XRET,

    // Invalid (or not-yet-executed) flag
    FLAGS_NX = 4'b1111

} Flags /* public */;
