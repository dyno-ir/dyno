module Test#(parameter N = 4)
(
  input logic clk,
  input logic rst,
  input logic[N-1:0] IN_valA,
  output logic[N-1:0] OUT_valA
);

always_ff@(posedge clk, posedge rst)
  OUT_valA <= rst ? 4'bxxx0 : IN_valA;

endmodule
