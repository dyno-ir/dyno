module Test(
  input logic[31:0] IN_valA,
  input logic[31:0] IN_valB,
  output logic OUT_res
);

assign OUT_res = IN_valA < IN_valB;

endmodule
