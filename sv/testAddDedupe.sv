// module Test#(parameter N = 16)
// (
//   input logic[N-1:0] IN_valA,
//   input logic[N-1:0] IN_valB,
//   input logic[2*N-1:0] IN_valC,
//   output logic[N-1:0] OUT_valA,
//   output logic[2*N-1:0] OUT_valB
// );

// assign OUT_valA = IN_valA + IN_valC;
// assign OUT_valB = {IN_valB, IN_valA} + IN_valC;


// endmodule


module Test2#(parameter N = 16)
(
  input logic[2*N-1:0] IN_valA,
  input logic[2*N-1:0] IN_valB,
  output logic[N-1:0] OUT_valA,
  output logic[2*N-1:0] OUT_valB
);

assign OUT_valA = IN_valA + IN_valB;
assign OUT_valB = IN_valA + IN_valB;


endmodule
