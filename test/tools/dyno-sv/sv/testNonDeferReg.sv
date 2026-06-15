module Test#(parameter N = 16)
(
  input logic clk,
  input logic IN_a,
  input logic IN_b,
  input logic IN_c,

  output logic OUT_a,
  output logic OUT_b
);

logic[1:0] tmp;
always_ff@(posedge clk) begin
  tmp[1] = IN_a & IN_b;
  OUT_a <= tmp[1] & IN_c;
  tmp = IN_a + IN_c;
  OUT_b <= tmp ^ IN_b;
end


endmodule
