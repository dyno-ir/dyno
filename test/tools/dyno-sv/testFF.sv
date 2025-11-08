module Test#(parameter N = 4)
(
  input logic clk,
  input logic rst,
  input logic[N-1:0] IN_valA,
  input logic[N-1:0] IN_valB,
  input logic[N-1:0] IN_valC,
  input logic IN_ctrl,
  output logic[N-1:0] OUT_valA
);

always_ff@(posedge clk)
  OUT_valA <= IN_valA & IN_valB;//(!rst | IN_ctrl) ? 4'b0 : IN_valA;

endmodule

/*
MODULE_DEF %Test:module("Test"), %0:block {
  INPUT_REGISTER_DEF %clk:register(1)
  INPUT_REGISTER_DEF %rst:register(1)
  INPUT_REGISTER_DEF %IN_valA:register(4)
  INPUT_REGISTER_DEF %IN_valB:register(4)
  INPUT_REGISTER_DEF %IN_valC:register(4)
  INPUT_REGISTER_DEF %IN_ctrl:register(1)
  OUTPUT_REGISTER_DEF %OUT_valA:register(4)
  REGISTER_DEF %1:register(4)
  FLIP_FLOP %clk, #1'd0, %1, %OUT_valA, %rst, #1'd1, #4'd0
  TRIGGER_DEF %2:trigger(neg, pos), %clk, %rst
  COMB_PROCESS_DEF %3:process, %4:block {
    LOAD %5:wire(1), %IN_ctrl  ["sv/testFF.sv:13:16-29"]
    LOAD %6:wire(4), %IN_valA  ["sv/testFF.sv:13:15-47"]
    MUX %7:wire(4), %5, #4'd0, %6
    STORE %7, %1
  }
}
*/
