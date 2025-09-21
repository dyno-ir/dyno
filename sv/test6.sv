module PrefixSum#(parameter N = 16)
(
  input logic[N-1:0] IN_valA,
  input logic[N-1:0] IN_valB,
  output logic[N-1:0] OUT_valA,
  output logic[N-1:0] OUT_valB
);


// logic[N-1:0] val;

// logic[15:0] a;
// logic[15:0] b;

// logic[1:0][7:0] field;

// always_comb begin
//   field[1] = IN_val;
// end

// always_comb begin
//   field[0] = field[1];
// end

// always_comb begin
//   OUT_val = field[0];
// end

// function integer passthru(input integer a);
//   return a;
// endfunction

logic[7:0] prop;

generate for(genvar i = 1; i < 8; i++) begin
  always@* begin
    //prop[i] = (i == 0) ? 0 : prop[passthru(i) - 1];
    //prop[i] = (i == 0) ? 1'b0 : prop[i - 1];
    prop[i] = (IN_valA == i) ? 0 : prop[(i-1) & 7];
  end
end endgenerate

always@* begin
    //prop[i] = (i == 0) ? 0 : prop[passthru(i) - 1];
    //prop[i] = (i == 0) ? 1'b0 : prop[i - 1];
    prop[0] = (IN_valA == 0) ? prop[0] : prop[(0-1) & 7];
  end

//assign OUT_valA = {IN_valA[15], IN_valA[0+:8]};
//assign OUT_valB = {IN_valA[15], IN_valA[0+:8]};


//logic[7:0] mem[16][16];

//always_comb mem[4][7][IN_valA] = 0;


endmodule
