module PrefixSum#(parameter N = 32)
(
    input logic clk,
    input logic IN_cfg,
    input logic[N-1:0] IN_dataA,
    input logic[N-1:0] IN_dataB,
    output logic[N-1:0] OUT_data,

    input logic[N-1:0] IN_dataA2,
    input logic[N-1:0] IN_dataB2,
    output logic[N-1:0] OUT_data2
);

always_ff@(negedge clk) begin
  assert(IN_dataA != 0);
  OUT_data <= IN_dataA + IN_dataB;
end

always_ff@(negedge clk)
  OUT_data2 = IN_dataA + IN_dataB;


endmodule
