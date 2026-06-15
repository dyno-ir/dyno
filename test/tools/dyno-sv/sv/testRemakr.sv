module Test#(parameter N = 16)
(
  input logic[N-1:0] IN_valA,
  input logic[N-1:0] IN_valB,
  input logic[N-1:0] IN_valC,
  output logic[N-1:0] OUT_valA
);

// this is slower...
// always_comb begin
//   logic[7:0] temp = IN_valA + IN_valB;
//   OUT_valA = temp + IN_valC;
// end

// ... than this
always_comb begin
  OUT_valA = IN_valA + IN_valB + IN_valC;
end

// Because carry chains can't be merged, consider IN_valA=128, IN_valB=128, IN_valC=0
// first is 0
// second is 256

/*
COMB_PROCESS_INSTR %6:process, %7:block {
  LOAD %8:wire(8), %2, #32'd0, #32'd8
  LOAD %9:wire(8), %3, #32'd0, #32'd8
  ADD %10:wire(8), %8, %9
  ZEXT %11:wire(16), %10
  LOAD %12:wire(16), %4
  ADD %13:wire(16), %11, %12
  STORE %13, %5
}
*/

/*
COMB_PROCESS_INSTR %6:process, %7:block {
  LOAD %8:wire(8), %2, #32'd0, #32'd8
  ZEXT %9:wire(16), %8
  LOAD %10:wire(8), %3, #32'd0, #32'd8
  ZEXT %11:wire(16), %10
  LOAD %12:wire(16), %4
  ADD_COMPRESS %13:wire(16), %14:wire(16), %12, %11, %9
  ADD %15:wire(16), %14, %13
  STORE %15, %5
}
*/

endmodule
