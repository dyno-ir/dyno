module Test#(parameter N = 4)
(
  input logic clk,
  input logic rst,
  input logic[N-1:0] IN_valA,
  input logic[N-1:0] IN_valB,
  input logic[N-1:0] IN_valC,
  input logic[N-1:0] IN_valD,
  input logic IN_ctrl,
  output logic[N-1:0] OUT_valA,
  output logic[N:0] OUT_valB
);

always_ff@(posedge clk) begin
  if (!rst) begin
  if (IN_ctrl)
    OUT_valA <= IN_valA + IN_valB;
  else
    OUT_valB <= IN_valC + IN_valD;
  end
end

endmodule
