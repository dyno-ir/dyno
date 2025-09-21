module Test#(parameter N = 32)
(
  input logic clk,
  input logic rst,
  input logic[N-1:0] IN_valA,

  output logic OUT_valA
);

int array [$] = '{3,4,5};
always_comb begin
  OUT_valA = IN_valA inside {1, 2, array};
end
endmodule
