module Test#(parameter N = 8)
(
  input logic clk,
  input logic rst,
  input logic[N-1:0] IN_valA,
  input logic[N-1:0] IN_valB,
  input logic[N-1:0] IN_valC,
  input logic IN_ctrl,
  output logic[N-1:0] OUT_valA,
  output logic[N-1:0] OUT_valB
);

assign OUT_valA = IN_valA * IN_valB;

endmodule
