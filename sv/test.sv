
module Test(
  input logic IN_a,
  input logic IN_b,
  output logic OUT_sum
);

always_comb begin
  OUT_sum = IN_a + 5'bxzzzz + 64'hdeadbeefdeadbeef + 2000;
end

endmodule


/*
raw instr dump:
%0 = rtl.MODULE_INSTR :rtl.module(0)
%1 = rtl.REGISTER_INSTR :rtl.register(0) &rtl.module(0)(1)
%2 = rtl.REGISTER_INSTR | &rtl.register(1) | &rtl.module(0)(2)
%3 = rtl.REGISTER_INSTR | &rtl.register(2) | &rtl.module(0)(3)
%4 = rtl.PROCESS_INSTR | &rtl.process(0) | &rtl.module(0)(4)
%5 = rtl.BLOCK_INSTR | &core.block(0) | &rtl.process(0)(1)
%6 = rtl.LOAD | &rtl.wire(0) | &rtl.register(0)(1)
%7 = rtl.LOAD | &rtl.wire(1) | &rtl.register(1)(1)
%8 = rtl.ADD | &rtl.wire(2) | &rtl.wire(0)(1) &rtl.wire(1)(1)
%9 = rtl.STORE | | &rtl.register(2)(1) &rtl.wire(2)(1)

structured dump:
module(0, Test,
in: %0 = &rtl.register(0)
in: %1 = &rtl.register(1)
out: %2 = &rtl.register(2)
):
proc(0):
block(0):
%3 = rtl.LOAD | &rtl.wire(1) | %1(1)
%4 = rtl.LOAD | &rtl.wire(0) | %0(1)
%5 = rtl.ADD | &rtl.wire(2) | &rtl.wire(0)(1) &rtl.wire(1)(1)
%6 = rtl.STORE | | %2(1) &rtl.wire(2)(1)
*/
