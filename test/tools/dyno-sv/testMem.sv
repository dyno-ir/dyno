

module Test (
  input logic clk,
  input logic[31:0] IN_waddr,
  input logic[7:0] IN_wdata,

  input logic[31:0] IN_raddr,
  output logic[7:0] OUT_rdata
);

logic[15:0][7:0] mem_c;
logic[15:0][7:0] mem_r;
always_ff@(posedge clk)
  mem_r <= mem_c;

always_comb begin
  mem_c = mem_r;
  // todo: try oob
  mem_c[IN_waddr[3:0]] = IN_wdata;
end

always_ff@(posedge clk) begin
  OUT_rdata <= mem_c[IN_raddr[3:0]];
end


endmodule
