module Test#(parameter WORDS = 8, parameter BITS = 7)
(
  input logic clk,
  input logic rst,
  input logic[$clog2(WORDS)-1:0] IN_addr,
  input logic[BITS-1:0] IN_word,
  input logic[WORDS-1:0][BITS-1:0] IN_data,
  output logic[WORDS-1:0][BITS-1:0] OUT_data
);

always_comb begin
  OUT_data = 'x;
  OUT_data[0] = 42;
end

endmodule
