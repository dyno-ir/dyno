module Test(
    input logic[31:0] IN_valA,
    output logic[31:0] OUT_valA
);

always_comb begin
    for (integer i = 0; i < 32; i=i+1)
        OUT_valA[i] = IN_valA[31-i];
end

endmodule
