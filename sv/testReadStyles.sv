

module Test (
  input logic clk,
  input logic[2:0][2:0][7:0] IN_data,
  input logic[3:0] IN_addrX,
  input logic[3:0] IN_addrY,
  output logic[7:0] OUT_data
);


assign OUT_data = IN_data[IN_addrX][IN_addrY];


endmodule
