module Test#(parameter N = 8)
(
  input logic clk,
  input logic rst,
  input logic IN_en,
  output logic[N-1:0] OUT_cnt
);

logic[2*N-1:0] cnt;

always_ff@(posedge clk, negedge rst)
  if (!rst) cnt <= '0;
  //else if (cnt == 42) cnt <= 43;
  else if (IN_en) cnt <= cnt + 1;

assign OUT_cnt = cnt;

endmodule
