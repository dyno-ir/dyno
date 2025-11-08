

module Test (
  logic[31:0] srcA,
  logic[31:0] srcB,
  logic[1:0] opcode
);

reg[31:0] resC;
always_comb begin
    for (int i = 0; i < 31; i++) begin
      resC = i;
    end
end

endmodule
