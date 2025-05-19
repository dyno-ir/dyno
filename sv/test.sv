/*module Test#(parameter WIDTH)
(
  input logic IN_a,
  input logic IN_b,
  output logic[WIDTH-1:0] OUT_sum
);
always_comb begin
  OUT_sum = IN_a + 5'sbzxxxx + 64'hdeadbeefdeadbeef + 2000;
end
endmodule


module Test2(
  input logic IN_a,
  input logic IN_b,
  output logic[1:0] OUT_sum,
  output logic OUT_d
);

Test#(2) test(.IN_a(IN_a), .IN_b(IN_b), .OUT_sum(OUT_sum));

always_comb begin
  OUT_d = 1;
end
endmodule


module Test3(
  input logic IN_a,
  input logic IN_b,
  output logic[1:0] OUT_sum,
  output logic[2:0] OUT_d
);

Test#(2) test(IN_a, IN_b, OUT_sum);

always_comb OUT_d[1+:2] = (OUT_sum + 1)[1:0];
endmodule*/


module Calc(
  input logic clk,
  input logic rst,
  input logic[127:0] in,
  output logic[31:0] res,
  output logic[127:0] res2
);

logic[99:0] variable;// = 100;

//always_comb res = (128'hdeadbeefdeadbeef * 100) / 128'hc0febabec0febabe == 115;
//always_comb res = -8 % 3;
//always_comb res = 32'hdeadbeef == (32'hc0febabe + 498009137);

//always_comb res = 1'(1'b1 ~^ 1'b1);
//always_comb res = !in;
//always_comb res2 = -2 ** -7;

//always_comb res2 = {in[31:0] + 32'b1, res[31:0]};
//always_comb {res, res2[3*32+:32], res2[0*32+:32], res2[1*32+:32], res2[2*32+:32]} = in;

always_comb begin
  static logic variable = 1;
  res2 = {4{1'b1, 30'h0000_0000, 1'bx}};
end

always_ff@(posedge clk, posedge rst) begin
  if (rst) begin
    variable <= 0;
  end
  else begin
    case (in[3:0])
        4'b0001,
        4'b1001: variable <= 2;
        4'b0110: variable <= 3;
        default: variable <= 4;
    endcase

  end
end

endmodule


module FIFO#(parameter NUM = 128, parameter WIDTH = 32)
(
  input logic clk,
  input logic rst,

  input logic[WIDTH-1:0] IN_data,
  input logic IN_valid,

  input logic IN_ready,
  output logic OUT_valid = 0,
  output logic[WIDTH-1:0] OUT_data = 0
);

logic[$clog2(NUM+1)-1:0] wrPtr = 0;
logic[$clog2(NUM+1)-1:0] rdPtr = 0;


logic[NUM-1:0] memory[WIDTH-1:0];

always_ff@(posedge clk, posedge rst) begin
  if (rst) begin
    wrPtr <= 0;
    rdPtr <= 0;
    OUT_valid <= 0;
  end
  else begin
    case ({IN_valid, OUT_valid && IN_ready})
      default: ;
      2'b01: begin
        rdPtr <= rdPtr + 1'b1;
        OUT_valid <= rdPtr + 1 != wrPtr;
      end
      2'b10: begin
        wrPtr <= wrPtr + 1'b1;
        OUT_valid <= 1;
      end
      2'b11: begin
        wrPtr <= wrPtr + 1'b1;
        rdPtr <= rdPtr + 1'b1;
        // OUT_valid <= 1;
      end
    endcase
  end
end

always_ff@(posedge clk) begin
  if (IN_valid)
    memory[wrPtr] <= IN_data;
end
always_ff@(posedge clk) begin
  if (rdPtr != wrPtr)
    OUT_data <= memory[rdPtr];
end

endmodule
