

module Test#(parameter N = 16)
(
  input logic clk,
  input logic[N-1:0] IN_valA,
  input logic[N-1:0] IN_valB,
  input logic[N-1:0] IN_valC,
  output logic[N-1:0] OUT_valA
);

initial OUT_valA = 42;

always@(posedge clk)
  OUT_valA <= IN_valA + IN_valB;

endmodule
