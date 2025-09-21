module Test#(parameter N = 64)
(
  input logic clk,
  input logic rst,
  input logic[N-1:0] IN_valA,
  input logic[31:0] IN_valB,

  output logic OUT_valA
);

assign OUT_valA = IN_valA[(IN_valB<<2)|1 +: 1];

endmodule
