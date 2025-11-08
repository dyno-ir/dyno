module Test#(parameter N = 4)
(
  input logic[N-1:0] IN_valA,
  input logic[N-1:0] IN_valB,
  input logic[N-1:0] IN_valC,
  input logic IN_ctrl,
  output logic[N-1:0] OUT_valA
);

always_comb begin
  OUT_valA = IN_valA + IN_valB;
end

endmodule
