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


typedef struct packed {
  Flags flags;
  logic valid;
} MyStr;

module Test#(parameter N = 16)
(
  input MyStr IN_valA,
  input logic[N-1:0] IN_valB,
  input logic IN_valC,
  output logic[N-1:0] OUT_valA
);

MyStr mystr;

always_comb begin
  if (IN_valA.flags == FLAGS_PRED_NTAKEN)
    OUT_valA = IN_valC ? FLAGS_PRED_TAKEN : FLAGS_PRED_NTAKEN;
end

endmodule
