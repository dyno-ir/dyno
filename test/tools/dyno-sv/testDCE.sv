

module Test (
  input logic clk,
  input logic rst,

  input logic[31:0] srcA,
  input logic[31:0] srcB,
  input logic doSub,
  output logic[31:0] out
);

wire[31:0] sum = srcA + srcB;
wire[31:0] diff = srcA - srcB;

always_ff@(posedge clk) begin
  if (doSub)
    out <= diff;
  else
    out <= sum;

  if (rst)
    out <= '0;
end

endmodule
