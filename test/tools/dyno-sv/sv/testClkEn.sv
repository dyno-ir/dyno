module Test#(parameter N = 16)
(
  input logic clk,
  input logic rst,
  input logic[N-1:0] IN_valA,
  output logic[N-1:0] OUT_valA
);

always_ff@(posedge clk) begin
  //if (IN_valA[0])
    OUT_valA = OUT_valA + 1;
end

endmodule
