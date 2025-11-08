

module Test#(parameter N = 8)
(
  input logic clk,
  input logic[N-1:0] IN_data,
  output logic[$clog2(N)-1:0] OUT_idx0,
  output logic[$clog2(N)-1:0] OUT_idx1
);


always_comb begin
  OUT_idx0 = 'x;
  OUT_idx1 = 'x;

  for (int i = 0; i < N; i++)
    if (IN_data[i]) begin
      OUT_idx1 = OUT_idx0;
      OUT_idx0 = i;
    end
end


endmodule
