

module Test (
  input logic[31:0] srcA,
  input logic[31:0] srcB,
  input logic[1:0] opcode,
  output logic[31:0] out
);

reg[31:0] resC;
always_comb begin
    for (int i = 0; 10 != i;) begin
      i++;
      resC = opcode ? resC : i;
    end
end

assign out = resC;

endmodule
