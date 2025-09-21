

module Test#(parameter N = 8)
(
  input logic clk,
  input logic[N-1:0] IN_dataA,
  input logic[N-1:0] IN_dataB,
  input logic[N-1:0] IN_dataC,
  output logic[N-1:0] OUT_data
);

//wire[N-1:0] temp = ~IN_dataA;

always_comb begin
  OUT_data = IN_dataA | IN_dataB | IN_dataC;
end

endmodule
