module PrefixSum#(parameter N = 32)
(
  input logic clk,
  input logic rst,

  input logic[N-1:0] IN_valA,
  input logic[N-1:0] IN_valB,
  input logic[N-1:0] IN_valC,
  output logic[N-1:0] OUT_valA,
  output logic[N-1:0] OUT_valB
);

/*
 always_comb begin
   OUT_valA = IN_valA;

   OUT_valA += IN_valB;
   OUT_valA += 20;
   //OUT_valA += IN_valC;
   OUT_valA += 40;
   OUT_valA -= IN_valA;
 end
*/

logic[255:0][31:0] mem;

always_comb begin
  mem |= IN_valA << IN_valB;
end

//always_comb begin
//  OUT_valB = IN_valB & '1 | '0;
//end

always_comb begin
  OUT_valA = mem[IN_valC];
end

endmodule
