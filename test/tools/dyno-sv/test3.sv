module PrefixSum#(parameter N = 32)
(
    input logic IN_cfg,
    input logic[N-1:0] IN_dataA,
    input logic[N-1:0] IN_dataB,
    output logic[N-1:0] OUT_data,

    input logic[N-1:0] IN_dataA2,
    input logic[N-1:0] IN_dataB2,
    output logic[N-1:0] OUT_data2
);

logic cfg;
always_comb
  cfg = ~IN_cfg;

logic[N-1:0] dataA;
logic[N-1:0] dataB;

always_comb
  dataA = IN_dataA;

always_comb
  dataB = IN_dataB;


always_comb
  OUT_data = cfg ? (dataA + dataB) : (dataA - dataB);

logic other;

always_comb
  other = ^dataA;



logic[N-1:0] dataA2;
logic[N-1:0] dataB2;

always_comb
  dataA2 = IN_dataA2;

always_comb
  dataB2 = IN_dataB2;


always_comb
  OUT_data2 = cfg ? (dataA2 + dataB2) : (dataA2 - dataB2);

logic other2;

always_comb
  other2 = ^dataA2;

endmodule
