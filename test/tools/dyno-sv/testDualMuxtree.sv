
module Test (
  input logic clk,
  input logic rst,
  input logic rst2,
  input logic[7:0] IN_waddr,
  input logic[7:0][31:0] IN_wdata,

  input logic[2:0] IN_raddr,
  output logic[31:0] OUT_rdata
);

always_comb begin
  OUT_rdata = 42;

  if (IN_waddr[0] || IN_waddr[1]) begin
    case (IN_raddr[1:0])
      0: OUT_rdata = IN_wdata[0];
      1: OUT_rdata = IN_wdata[1];
      2: OUT_rdata = IN_wdata[2];
      3: OUT_rdata = IN_wdata[3];
    endcase
  end
  else begin
    case (IN_raddr[1:0])
      0: OUT_rdata = IN_wdata[4];
      1: OUT_rdata = IN_wdata[5];
      2: OUT_rdata = IN_wdata[6];
      3: OUT_rdata = IN_wdata[7];
    endcase
  end

end


endmodule
