module Test3#(parameter WIDTH)
(
  input logic IN_a,
  input logic IN_b,
  output logic[WIDTH-1:0] OUT_sum
);
always_comb begin
  OUT_sum = IN_a + 5'sbzxxxx + 64'hdeadbeefdeadbeef + 2000;
end
endmodule


module Test4(
  input logic IN_a,
  input logic IN_b,
  output logic[1:0] OUT_sum,
  output logic OUT_d
);

Test3#(2) test(.IN_a(IN_a), .IN_b(IN_b), .OUT_sum(OUT_sum));

always_comb begin
  OUT_d = 1;
end
endmodule


module Test5(
  input logic IN_a,
  input logic IN_b,
  output logic[1:0] OUT_sum,
  output logic[2:0] OUT_d
);

Test3#(2) test(IN_a, IN_b, OUT_sum);

always_comb OUT_d[1+:2] = {OUT_sum + 1}[1:0];
endmodule


module Calc(
  input logic clk,
  input logic rst,
  input logic[127:0] in,
  output logic[31:0] res,
  output logic[127:0] res2
);

logic[99:0] variable;// = 100;

initial res = (128'hdeadbeefdeadbeef * 100) / 128'hc0febabec0febabe == 115;
initial res = -8 % 3;
initial res = 32'hdeadbeef == (32'hc0febabe + 498009137);
initial res = 1'(1'b1 ~^ 1'b1);
initial res = !in;
initial res2 = -2 ** -7;
initial res2 = {in[31:0] + 32'b1, res[31:0]};
initial {res, res2[3*32+:32], res2[0*32+:32], res2[1*32+:32], res2[2*32+:32]} = in;

initial begin
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

logic[WIDTH-1:0] memory[NUM-1:0];

initial begin
  for (int i = 1; i < NUM; i += 1)
    memory[i] = 'x;
end

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

always@(posedge clk) begin
  if (IN_valid)
    memory[wrPtr] <= IN_data;
end
always_ff@(posedge clk) begin
  if (rdPtr != wrPtr)
    OUT_data <= memory[rdPtr];
end

endmodule


typedef struct packed {
  logic[7:0] a;
  logic[7:0] b;
  logic c;
} Struct;

typedef struct packed {

  Struct str1;
  Struct str2;
  logic[31:0] d;
} Struct2;

module Test(
  input Struct IN_str,
  output logic[7:0] OUT_b,
  output Struct OUT_str
);

always_comb begin
  OUT_b = '1;
  if (IN_str.c) begin
    OUT_b = IN_str.a;
  end
end


Struct2 str2;
always_comb begin
  str2 = '{str1: Struct'{a:1, b:1, c:1}, str2: Struct'{a:1, b:1, c:1}, default: '0};
end

 logic[7:0] arr[3:0];
 always_comb
   arr = '{1: 1, default: 0};

//always_comb d = 2'b10;


typedef struct {
  logic [7:0] a;
  bit b;
  bit signed [31:0] c;
} sa;
sa s2;
initial s2 = '{int:1, default:0};
initial s2 = '{logic:1, default:0};
initial s2 = '{bit:1, default:0};
initial s2 = '{bit signed:1, default:0};

endmodule





module Test2(
  input logic[7:0] IN_b,
  output logic[7:0] OUT_b
);

generate for (genvar i = 0; i < 8; i++) begin
  if (i % 2 == 0) begin
    assign OUT_b[i] = IN_b[i];
  end
  else begin
    assign OUT_b[i] = !IN_b[i];
  end
end
endgenerate

endmodule
