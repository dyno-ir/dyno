module Test#(parameter N = 4)
(
  input logic clk,
  input logic rst,
  input logic[4*N-1:0] IN_valA,
  output logic[N-1:0] OUT_valA,
  output logic[2*N-1:0] OUT_valB,
  output logic[N-1:0] OUT_valC
);

always_ff@(posedge clk) begin
  OUT_valA <= IN_valA[0+:N];
  OUT_valB <= IN_valA[N-2+:N];
  OUT_valC <= IN_valA[N+:N];
end

endmodule
