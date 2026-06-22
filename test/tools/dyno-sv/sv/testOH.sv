

module Test (
  input logic clk,
  input logic[7:0] IN_waddr,
  input logic[31:0] IN_wdata,
  input logic[3:0] IN_wmask,

  input logic[7:0] IN_raddr,
  output logic[3:0] OUT_rdata
);

assign OUT_rdata[3:0] = 4'b1 << IN_waddr[0+:4];

endmodule
