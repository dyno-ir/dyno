module Test#(parameter N = 16)
(
  input logic[N-1:0] IN_valA,
  input logic[N-1:0] IN_valB,
  input logic IN_valC,
  output logic[N-1:0] OUT_valA
);

always_comb begin
  OUT_valA = IN_valA + IN_valB + IN_valC;
end

endmodule
