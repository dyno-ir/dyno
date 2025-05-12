
module Test#(parameter WIDTH)
(
  input logic IN_a,
  input logic IN_b,
  output logic[WIDTH-1:0] OUT_sum
);
always_comb begin
  OUT_sum = IN_a + 5'bxzzzz + 64'hdeadbeefdeadbeef + 2000;
end
endmodule


module Test2(
  input logic IN_a,
  input logic IN_b,
  output logic[1:0] OUT_sum,
  output logic OUT_d,
);

Test#(2) test(IN_a, IN_b, OUT_sum);

always_comb begin
  OUT_d = 1;
end
endmodule


module Test3(
  input logic IN_a,
  input logic IN_b,
  output logic[1:0] OUT_sum,
  output logic OUT_d,
);

Test#(2 + 1) test(IN_a, IN_b, OUT_sum);

always_comb begin
  OUT_d = 1;
end
endmodule
