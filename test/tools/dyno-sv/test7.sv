module PrefixSum#(parameter N = 16)
(
  input logic clk,
  input logic rst,

  input logic[N-1:0] IN_valA,
  input logic[N-1:0] IN_valB,
  output logic[N-1:0] OUT_valA,
  output logic[N-1:0] OUT_valB
);


// always@(posedge clk) begin
//   // OUT_valB <= IN_valA + IN_valB;

//   if (rst) begin
//     OUT_valB[8+:8] <= 0;
//   end
// end

logic[7:0] memory[2:32];

always@(posedge clk) begin
  if (rst) begin
    for (integer i = 0; i < 32; i++)
      memory[IN_valB] <= 0;
  end
end

// always@(posedge clk) begin
//   memory[IN_valA] <= IN_valB;
// end


// always_comb begin
//   OUT_valA = IN_valA + IN_valB;

//   if (rst) begin
//     OUT_valA = 0;
//   end
//   else
//     ;
// end



endmodule
