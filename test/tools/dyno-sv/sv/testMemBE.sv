

module Test (
  input logic clk,
  input logic[7:0] IN_waddr,
  input logic[31:0] IN_wdata,
  input logic[3:0] IN_wmask,

  input logic[7:0] IN_raddr,
  output logic[31:0] OUT_rdata
);

logic[31:0] mem[3:0];
always_ff@(posedge clk)
  for (int i = 0; i < 4; i++)
    if (IN_wmask[i])
      mem[IN_waddr][i*8+:8] <= IN_wdata[i*8+:8];

always_ff@(posedge clk) begin
  OUT_rdata <= mem[IN_raddr];
end


endmodule
