module Sub (
  input wire[31:0] IN_a,
  input wire[31:0] IN_b,
  output wire[31:0] OUT_c
);

assign OUT_c = IN_a + IN_b;

endmodule


module Top (
  input logic[31:0] IN_a,
  input logic[31:0] IN_b,
  input logic[31:0] IN_c,
  output logic[31:0] OUT_d
);

logic[31:0] temp;
Sub sub(IN_a, IN_b, temp);

assign OUT_d = temp * IN_c;

endmodule
