

module Test (
  input logic clk,
  input logic[3:0] IN_waddr_x,
  input logic[3:0] IN_waddr_y,
  input logic[31:0] IN_wdata,
  input logic[3:0] IN_wmask,

  input logic[7:0] IN_raddr,
  output logic[31:0] OUT_rdata
);

// non pow2 size
reg[2:0][2:0][3:0][7:0] mem;


always_ff@(posedge clk)
  for (integer i = 0; i < 4; i++)
    if (IN_wmask[i])
      mem[IN_waddr_x][IN_waddr_y][i] <= IN_wdata[i*8+:8];

always_ff@(posedge clk) begin
  // flat access. this may alias
  OUT_rdata <= mem[IN_waddr_y][IN_waddr_x];//[//mem[IN_raddr*8 +: 8];
end


endmodule
