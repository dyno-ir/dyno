module Test#(parameter N = 4)
(
  input logic clk,
  input logic rst,
  input logic[N-1:0] IN_valA,
  input logic[N-1:0] IN_valB,
  input logic[N-1:0] IN_valB2,

  input logic[N-1:0] IN_valC[3:0],
  input logic[N-1:0] IN_valD[3:0],
  input logic[N-1:0] IN_valE[3:0],

  output logic[N-1:0] OUT_valA
);

always_comb begin
  // outer level: priority between rst (1), default case (2), and explicit cases (3).
  if (rst)
    OUT_valA = 42;
  else begin
    case(IN_valA)
      // mid level: non-overlapping/one-hot cases
      0: OUT_valA = IN_valB[0] ? (IN_valB2[0] ? IN_valE[0] : IN_valC[0]) : IN_valD[0];
                    // inner level (duplicated 4x) priority logic. Can't be lifted
                    // without increasing critical path because values are all different.
      1: OUT_valA = IN_valB[1] ? (IN_valB2[1] ? IN_valE[1] : IN_valC[1]) : IN_valD[1];
      2: OUT_valA = IN_valB[2] ? (IN_valB2[2] ? IN_valE[2] : IN_valC[2]) : IN_valD[2];
      3: OUT_valA = IN_valB[3] ? (IN_valB2[3] ? IN_valE[3] : IN_valC[3]) : IN_valD[3];
    endcase
  end
end

/*
// naive encoder with priority (not MuxTreeOpt style one hot):
// we still get goofy prefixes because of nested cases.[]

rst:                       42

A == 0 &&  B[0] &&  B2[0]: IN_valE[0]
A == 0 &&  B[0]:           IN_valC[0]
A == 0:                    IN_valD[0]

A == 1 &&  B[1] &&  B2[1]: IN_valE[1]
A == 1 &&  B[1]:           IN_valC[1]
A == 1:                    IN_valD[1]

A == 2 &&  B[2] &&  B2[2]: IN_valE[2]
A == 2 &&  B[2]:           IN_valC[2]
A == 2:                    IN_valD[2]

A == 3 &&  B[3] &&  B2[3]: IN_valE[3]
A == 3 &&  B[3]:           IN_valC[3]
A == 3:                    IN_valD[3]

1'b1:                      OUT_valA
*/

endmodule
