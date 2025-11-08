module PrefixSum#(parameter N = 32)
(
    input logic[N-1:0] IN_dataA,
    input logic[N-1:0] IN_dataB,
    output logic[N-1:0] OUT_data
);

function automatic logic[N-1:0] func2(input logic[31:0] a, input logic[31:0] b);
  return func(a, b);
endfunction

function automatic logic[N-1:0] func(input logic[31:0] a, input logic[31:0] b);
  return a + b;// + func(a, b);
endfunction


always_comb begin
  OUT_data = func(0,0);
  OUT_data += func2(IN_dataA + 0, IN_dataB);
end
endmodule
