module Test#(parameter N = 32)
(
  input logic clk,
  input logic rst,

  input logic[N-1:0] IN_valA,
  input logic[N-1:0] IN_valB,
  input logic[N-1:0] IN_valC,
  output logic[N-1:0] OUT_valA,
  output logic[N-1:0] OUT_valB
);


always_comb begin
  casez (IN_valA)
    32'b0x: OUT_valA = IN_valB;
    42: OUT_valA = IN_valC;
    69: OUT_valA = IN_valC & IN_valB;
  endcase
end

//always_comb begin
//  OUT_valB = IN_valB & '1 | '0;
//end

endmodule
