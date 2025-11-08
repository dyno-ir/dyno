module PrefixSum#(parameter N = 32)
(
  input logic[N-1:0] IN_val,
  output logic[N-1:0] OUT_val
);


// logic[N-1:0] val;

// logic[15:0] a;
// logic[15:0] b;

logic unk;

logic[4095:0][7:0] memory;

// always_comb begin
//   val = a;
//   val[16+:16] = a;
//   val[0+:16] = b;
//   OUT_val = 1 + val;
//   val[unk*16 +: 16] = 0;
// end


// always_comb begin
//   memory[a][0] = IN_val[0];
//   memory[a][1+:3] = IN_val[1+:3];
//   memory[a][4+:4] = IN_val[4+:4];
// end

// always_comb begin
//   //OUT_val = 0;

//   if (unk) begin
//     OUT_val[0+:16] = a;
//     OUT_val[23] = 1;
//   end
//   else begin
//     OUT_val[0+:16] = b;
//     OUT_val[25] = 1;
//   end
// end

// always_comb begin
//   for (integer i = 0; i < 4096; i++) begin
//     unk = !unk;
//     memory[i] = unk;
//   end

//   assert(memory[a]);
// end

always_comb begin
    for (integer i = 0; i < 4096; i=i+1) begin
        for (integer j = 0; j < 8; j=j+1)
            memory[i][j] = 1; // LSBit represents undefined
        memory[i][0] = !unk;
    end
end


endmodule
