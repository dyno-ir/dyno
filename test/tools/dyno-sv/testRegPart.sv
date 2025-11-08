module TestRegPart#(parameter N=4)(
  input wire clk,

  input wire[N-1:0] IN_a,
  input wire[N-1:0] IN_b,
  input wire[N-1:0] IN_c,
  input wire[N-1:0] IN_d,

  input wire IN_sel_a,
  input wire IN_sel_b,
  input wire IN_sel_c,
  input wire IN_sel_d,

  output logic[4*N-1:0] OUT_dat,

  output logic[N-1:0] OUT_a,
  output logic[N-1:0] OUT_b,
  output logic[N-1:0] OUT_c,
  output logic[N-1:0] OUT_d
);

logic[4*N-1:0] dat;

always_ff@(posedge clk) begin
  if (IN_sel_a)
    dat[0+:4] <= IN_a;
  if (IN_sel_b)
    dat[4+:4] <= IN_b;
  if (IN_sel_c)
    dat[8+:4] <= IN_c;
  if (IN_sel_d)
    dat[12+:4] <= IN_d;
end

// assign {OUT_a, OUT_b, OUT_c, OUT_d} = dat;
assign OUT_dat = dat;

endmodule
