

module Test (
  input logic clk,
  input logic[15:0][7:0] IN_data,
  input logic[3:0] IN_addrX,
  input logic[3:0] IN_addrY,
  output logic[7:0] OUT_dataA,
  output logic[7:0] OUT_dataB
);


//assign OUT_data = IN_data[IN_addrX * 3 + IN_addrY];

// larger not divisible by smaller
assign OUT_dataB = {IN_data}[IN_addrX * 3 + IN_addrY * 16 +: 8];

endmodule
