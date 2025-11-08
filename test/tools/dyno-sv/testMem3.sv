module Test(
  input logic[3:0] IN_addr,
  input logic[3:0] IN_subaddr,
  output logic OUT_a,
  output logic[7:0] IN_data
);

typedef struct packed {
  logic[15:0][7:0] arr;
  logic a;
} Struct;

Struct str[15:0];

always_comb begin
  str[IN_addr].arr[IN_subaddr] = IN_data;
end
assign OUT_a = str[IN_addr].a;
endmodule
