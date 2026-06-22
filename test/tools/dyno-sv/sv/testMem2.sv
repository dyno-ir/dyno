module Test(
  input logic[3:0] IN_addr,
  input logic[3:0] IN_subaddr,
  output logic OUT_a,
  output logic[7:0] OUT_data
);

typedef struct packed {
  logic[15:0][7:0] arr;
  logic a;
} Struct;

Struct str[15:0];

always_comb begin
  Struct temp = str[IN_addr];
  OUT_data = temp.arr[IN_subaddr];
end

endmodule
