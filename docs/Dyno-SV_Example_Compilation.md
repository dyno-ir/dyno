# Example Compilation
This is an example to demonstrate the Dyno-SV synthesis flow. This is very simplified, a total of around 126 passes are run during synthesis right now (mostly InstCombine & CSE for canonicalization). This is just a bunch of IR snippets from interesting points in the pipeline.

### 1. SystemVerilog input
```systemverilog
module Counter(
  input logic clk,
  input logic rst,

  input logic en,
  output logic cnt
);

// Count up by 10 in a loop to make things interesting
logic next_cnt;
always_comb begin
  next_cnt = cnt;
  for (int i = 0; i < 10; i++)
    next_cnt = next_cnt + 1;
end

always_ff@(posedge clk) begin
  if (rst)
    cnt <= 0;
  else if (en)
    cnt <= next_cnt;
end

endmodule
```

### 2. Unoptimized Dyno-IR after Slang-based parser
Naive translation of Verilog to Dyno-IR. Goal is mapping Verilog concepts to Dyno-IR essentially 1:1 such that no information is lost. Importantly, we don't convert to a netlist yet (and won't for quite a while as you'll see below). This is using a sequential execution model with full control flow.
```
MODULE_DEF %Counter:module("Counter"), %0:block {
  INPUT_REGISTER_DEF %clk:register(1)
  INPUT_REGISTER_DEF %rst:register(1)
  INPUT_REGISTER_DEF %en:register(1)
  OUTPUT_REGISTER_DEF %cnt:register(8)
  REGISTER_DEF %next_cnt:register(8)
  REGISTER_DEF %r5:register(32)
  TRIGGER_DEF %1:trigger(pos), %clk
  COMB_PROCESS_DEF %2:process, %3:block {
    LOAD %w0:wire(8), %cnt, #32'd0
    STORE %w0, %next_cnt, #32'd0
    STORE #32'd0, %r5
    WHILE %4:block, %5:block {
      LOAD %w1:wire(32), %r5, #32'd0
      ICMP_SLT %w2:wire(1), %w1, #32'd10
      YIELD %w2
    }{
      LOAD %w3:wire(8), %next_cnt, #32'd0
      ZEXT %w4:wire(32), %w3
      ADD %w5:wire(32), %w4, #32'd1
      TRUNC %w6:wire(8), %w5
      STORE %w6, %next_cnt, #32'd0
      LOAD %w7:wire(32), %r5, #32'd0
      ADD %w8:wire(32), %w7, #32'd1
      STORE %w8, %r5, #32'd0
    }
  }
  SEQ_PROCESS_DEF %6:process, %7:block, %1 {
    LOAD %w9:wire(1), %rst, #32'd0
    IF %8:block, %9:block, %w9 {
      STORE_DEFER #8'd0, %cnt, #32'd0
    } /*ELSE*/ {
      LOAD %w10:wire(1), %en, #32'd0
      IF %10:block, %w10 {
        LOAD %w11:wire(8), %next_cnt, #32'd0
        STORE_DEFER %w11, %cnt, #32'd0
      }
    }
  }
}
```

### 3. General Optimization
Optimizations that are applicable to both simulation and synthesis, e.g.

- SSA construction, convert load/store to yield values
- Process fusion: Everything is fused into as few processes as possible. Clock sensitivity is moved from processes into unblocking (deferred) assignments.
- Loop simplification: While loop to known-bounds for loop.
```
MODULE_DEF %Counter:module("Counter"), %0:block {
  INPUT_REGISTER_DEF %clk:register(1)
  INPUT_REGISTER_DEF %rst:register(1)
  INPUT_REGISTER_DEF %en:register(1)
  OUTPUT_REGISTER_DEF %cnt:register(8)
  TRIGGER_DEF %1:trigger(pos), %clk
  COMB_PROCESS_DEF %2:process, %3:block {
    LOAD %w11:wire(8), %cnt
    FOR %4:block, %w14:wire(8), #32'd0, #32'd10, #32'd1, %w11 {
      UNYIELD %w17:wire(32), %w16:wire(8)
      ADD %w12:wire(8), %w16, #8'd1
      YIELD %w12
    }
    LOAD %w1:wire(1), %rst
    IF %5:block, %6:block, %w10:wire(8), %w1 {
      YIELD #8'd0
    }{
      LOAD %w15:wire(1), %en
      IF %7:block, %8:block, %w9:wire(8), %w15 {
        YIELD %w14
      }{
        YIELD %w11
      }
      YIELD %w9
    }
    // a "non-blocking" store in Verilog parlance. %1 is the trigger, part of the store here.
    STORE_DEFER %w10, %cnt, %1
  }
}
```

### 4. Unroll Loops
This is code directly after unrolling --- instcombine will merge the 10 adds immediately after.
```
MODULE_DEF %Counter:module("Counter"), %0:block {
  INPUT_REGISTER_DEF %clk:register(1)
  INPUT_REGISTER_DEF %rst:register(1)
  INPUT_REGISTER_DEF %en:register(1)
  OUTPUT_REGISTER_DEF %cnt:register(8)
  TRIGGER_DEF %1:trigger(pos), %clk
  COMB_PROCESS_DEF %2:process, %3:block {
    LOAD %w14:wire(8), %cnt
    ADD %w13:wire(8), %w14, #8'd1
    ADD %w8:wire(8), %w13, #8'd1
    ADD %w6:wire(8), %w8, #8'd1
    ADD %w5:wire(8), %w6, #8'd1
    ADD %w4:wire(8), %w5, #8'd1
    ADD %w0:wire(8), %w4, #8'd1
    ADD %w3:wire(8), %w0, #8'd1
    ADD %w7:wire(8), %w3, #8'd1
    ADD %w19:wire(8), %w7, #8'd1
    ADD %w2:wire(8), %w19, #8'd1
    LOAD %w16:wire(1), %rst
    IF %4:block, %5:block, %w10:wire(8), %w16 {
      YIELD #8'd0
    }{
      LOAD %w17:wire(1), %en
      IF %6:block, %7:block, %w9:wire(8), %w17 {
        YIELD %w2
      }{
        YIELD %w14
      }
      YIELD %w9
    }
    STORE_DEFER %w10, %cnt, %1
  }
}
```

### 5. Mid-level Synthesis Optimizations
This is the level most interesting for novel synthesis optimization. For example, since control flow is still intact we can do logic sharing in a much faster and more intuitive manner than on netlists: If logic is in different branches of an if it may be shared. This covers common sharing cases without any SAT.
```
MODULE_DEF %Counter:module("Counter"), %0:block {
  INPUT_REGISTER_DEF %clk:register(1)
  INPUT_REGISTER_DEF %rst:register(1)
  INPUT_REGISTER_DEF %en:register(1)
  OUTPUT_REGISTER_DEF %cnt:register(8)
  TRIGGER_DEF %1:trigger(pos), %clk
  COMB_PROCESS_DEF %2:process, %3:block {
    LOAD %w14:wire(8), %cnt
    ADD %w2:wire(8), %w14, #8'd10
    LOAD %w16:wire(1), %rst
    IF %4:block, %5:block, %w10:wire(8), %w16 {
      YIELD #8'd0
    }{
      LOAD %w17:wire(1), %en
      IF %6:block, %7:block, %w9:wire(8), %w17 {
        YIELD %w2
      }{
        YIELD %w14
      }
      YIELD %w9
    }
    STORE_DEFER %w10, %cnt, %1
  }
}

```

### 6. Low-level Synthesis Optimizations
Classic synthesis optimizations on a flat netlist representation. Not much to do in this case.
```
MODULE_DEF %Counter:module("Counter"), %0:block {
  INPUT_REGISTER_DEF %clk:register(1)
  INPUT_REGISTER_DEF %rst:register(1)
  INPUT_REGISTER_DEF %en:register(1)
  OUTPUT_REGISTER_DEF %cnt:register(8)
  TRIGGER_DEF %1:trigger(pos), %clk
  COMB_PROCESS_DEF %2:process, %3:block {
    LOAD %w17:wire(8), %cnt
    ADD %w2:wire(8), %w17, #8'd10
    LOAD %w9:wire(1), %rst
    LOAD %w8:wire(1), %en
    MUX %w13:wire(8), %w8, %w2, %w17
    MUX %w10:wire(8), %w9, #8'd0, %w13
    STORE_DEFER %w10, %cnt, %1
  }
}
```

### 7. Flip Flop Inference
```
MODULE_DEF %Counter:module("Counter"), %0:block {
  // To avoid confusion with flip flops: "register" in Dyno-IR is like a "reg" assigned in always_comb in Verilog --- not a flip flop.
  INPUT_REGISTER_DEF %clk:register(1)
  INPUT_REGISTER_DEF %rst:register(1)
  INPUT_REGISTER_DEF %en:register(1)
  OUTPUT_REGISTER_DEF %cnt:register(8)
  REGISTER_DEF %r6:register(1)
  REGISTER_DEF %r7:register(8)
  // A generic 8-bit flop flop with {clock, clock edge, D, Q, clock enable, clock enable polarity}
  FLIP_FLOP %clk, #1'd1, %r7, %cnt, %r6, #1'd0
  TRIGGER_DEF %1:trigger(pos), %clk
  COMB_PROCESS_DEF %2:process, %3:block {
    LOAD %w17:wire(8), %cnt
    ADD %w2:wire(8), %w17, #8'd10
    LOAD %w9:wire(1), %rst
    LOAD %w8:wire(1), %en
    MUX %w13:wire(8), %w8, %w2, %w17
    MUX %w10:wire(8), %w9, #8'd0, %w13
    NOT %w14:wire(1), %w9
    NOT %w16:wire(1), %w8
    AND %w19:wire(1), %w16, %w14
    STORE %w19, %r6
    // Make value of w7 undefined when clock enable is not set by MUXing in 'x.
    MUX %w7:wire(8), %w19, #8'hxx, %w10
    STORE %w7, %r7
  }
}
```

### 8. Flop Flop Mapping
Map FFs to target-specific FFs from liberty file.
```
MODULE_DEF %Counter:module("Counter"), %0:block {
  INPUT_REGISTER_DEF %clk:register(1)
  INPUT_REGISTER_DEF %rst:register(1)
  INPUT_REGISTER_DEF %en:register(1)
  OUTPUT_REGISTER_DEF %cnt:register(8)
  COMB_PROCESS_DEF %1:process, %2:block {
    LOAD %w3843:wire(1), %en
    LOAD %w3813:wire(1), %rst
    OR %w6:wire(1), %w3813, %w3843
    LOAD %w3738:wire(8), %cnt
    ADD %w2:wire(8), %w3738, #8'd10
    MUX %w4:wire(8), %w3843, %w2, %w3738
    MUX %w3:wire(8), %w3813, #8'd0, %w4
    LOAD %w3716:wire(1), %clk
    TRUNC %w3956:wire(1), %w3
    // :? is a forward reference (the sky130_fd_sc_hd__edfxtp_1 is not printed here for brevity)
    STDCELL_INSTANCE %w3928:wire(1), %sky130_fd_sc_hd__edfxtp_1:?module("sky130_fd_sc_hd__edfxtp_1"), %w3716, %w3956, %w6
    SPLICE %w3872:wire(1), %w3, #32'd1
    STDCELL_INSTANCE %w3871:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w3872, %w6
    SPLICE %w3842:wire(1), %w3, #32'd2
    STDCELL_INSTANCE %w3814:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w3842, %w6
    SPLICE %w3756:wire(1), %w3, #32'd3
    STDCELL_INSTANCE %w3739:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w3756, %w6
    SPLICE %w3734:wire(1), %w3, #32'd4
    STDCELL_INSTANCE %w3717:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w3734, %w6
    SPLICE %w3712:wire(1), %w3, #32'd5
    STDCELL_INSTANCE %w3695:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w3712, %w6
    SPLICE %w3690:wire(1), %w3, #32'd6
    STDCELL_INSTANCE %w3673:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w3690, %w6
    SPLICE %w3671:wire(1), %w3, #32'd7
    STDCELL_INSTANCE %w3667:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w3671, %w6
    CONCAT %w3649:wire(8), %w3667, %w3673, %w3695, %w3717, %w3739, %w3814, %w3871, %w3928
    STORE %w3649, %cnt
  }
}
```

### 9. Lower Ops to Gates
Implement complex operations (add in this case) with basic logic gates.
```
MODULE_DEF %Counter:module("Counter"), %0:block {
  INPUT_REGISTER_DEF %clk:register(1)
  INPUT_REGISTER_DEF %rst:register(1)
  INPUT_REGISTER_DEF %en:register(1)
  OUTPUT_REGISTER_DEF %cnt:register(8)
  COMB_PROCESS_DEF %1:process, %2:block {
    LOAD %w3843:wire(1), %en
    LOAD %w3813:wire(1), %rst
    OR %w6:wire(1), %w3813, %w3843
    LOAD %w3738:wire(8), %cnt
    TRUNC %w40:wire(4), %w3738
    AND %w31:wire(4), %w40, #4'd10
    XOR %w1345:wire(8), %w3738, #8'd10
    SPLICE %w3983:wire(1), %w31, #32'd1
    SPLICE %w1338:wire(1), %w1345, #32'd2
    SPLICE %w1336:wire(1), %w1345, #32'd3
    SPLICE %w3981:wire(1), %w31, #32'd3
    SPLICE %w1332:wire(1), %w1345, #32'd4
    SPLICE %w1330:wire(1), %w1345, #32'd5
    SPLICE %w1328:wire(1), %w1345, #32'd6
    SPLICE %w1324:wire(1), %w1345, #32'd7
    AND %w1315:wire(1), %w1336, %w1338
    AND %w1310:wire(1), %w1330, %w1332
    AND %w1306:wire(1), %w1315, %w3983
    OR %w1303:wire(1), %w1306, %w3981
    AND %w1293:wire(1), %w1310, %w1303
    AND %w1290:wire(1), %w1328, %w1293
    AND %w1285:wire(1), %w1332, %w1303
    AND %w1282:wire(1), %w1338, %w3983
    XOR %w1277:wire(1), %w1324, %w1290
    XOR %w1276:wire(1), %w1328, %w1293
    XOR %w1275:wire(1), %w1330, %w1285
    XOR %w1274:wire(1), %w1332, %w1303
    XOR %w1273:wire(1), %w1336, %w1282
    XOR %w1272:wire(1), %w1338, %w3983
    TRUNC %w22:wire(2), %w1345
    CONCAT %w2:wire(8), %w1277, %w1276, %w1275, %w1274, %w1273, %w1272, %w22
    MUX %w4:wire(8), %w3843, %w2, %w3738
    MUX %w3:wire(8), %w3813, #8'd0, %w4
    LOAD %w3716:wire(1), %clk
    TRUNC %w3956:wire(1), %w3
    STDCELL_INSTANCE %w3928:wire(1), %sky130_fd_sc_hd__edfxtp_1:?module("sky130_fd_sc_hd__edfxtp_1"), %w3716, %w3956, %w6
    SPLICE %w3872:wire(1), %w3, #32'd1
    STDCELL_INSTANCE %w3871:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w3872, %w6
    SPLICE %w3842:wire(1), %w3, #32'd2
    STDCELL_INSTANCE %w3814:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w3842, %w6
    SPLICE %w3756:wire(1), %w3, #32'd3
    STDCELL_INSTANCE %w3739:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w3756, %w6
    SPLICE %w3734:wire(1), %w3, #32'd4
    STDCELL_INSTANCE %w3717:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w3734, %w6
    SPLICE %w3712:wire(1), %w3, #32'd5
    STDCELL_INSTANCE %w3695:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w3712, %w6
    SPLICE %w3690:wire(1), %w3, #32'd6
    STDCELL_INSTANCE %w3673:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w3690, %w6
    SPLICE %w3671:wire(1), %w3, #32'd7
    STDCELL_INSTANCE %w3667:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w3671, %w6
    CONCAT %w3649:wire(8), %w3667, %w3673, %w3695, %w3717, %w3739, %w3814, %w3871, %w3928
    STORE %w3649, %cnt
  }
}
```
### 10. Lower Logic to AND inverter graph (AIG)
AIGs are great for generic logic optimization and mapping to gates. We convert all comb logic into one here.
```
MODULE_DEF %Counter:module("Counter"), %0:block {
  INPUT_REGISTER_DEF %clk:register(1)
  INPUT_REGISTER_DEF %rst:register(1)
  INPUT_REGISTER_DEF %en:register(1)
  OUTPUT_REGISTER_DEF %cnt:register(8)
  COMB_PROCESS_DEF %1:process, %2:block {
    // The AIG graph itself, consisting only of ANDs & inverters
    aig.GRAPH %3:aig(
      // fat AIG nodes are used as input or outputs. They are fat as they carry extra info
      // to make them accessible from outside the AIG.
      $1 = node !$fat0, !$fat1
      $2 = node $fat4, !$fat5
      $3 = node $fat6, $fat7
      $4 = node $2, $fat3
      $5 = node !$4, !$fat5
      $6 = node $3, !$5
      $7 = node $6, $fat8
      $8 = node !$5, $fat6
      $9 = node $fat3, $fat4
      $10 = node !$7, $fat9
      $11 = node $7, !$fat9
      $12 = node !$10, !$11
      $13 = node !$6, $fat8
      $14 = node $6, !$fat8
      $15 = node !$13, !$14
      $16 = node !$8, $fat7
      $17 = node $8, !$fat7
      $18 = node !$16, !$17
      $19 = node $5, $fat6
      $20 = node !$5, !$fat6
      $21 = node !$19, !$20
      $22 = node !$9, !$fat5
      $23 = node $9, $fat5
      $24 = node !$22, !$23
      $25 = node !$fat3, $fat4
      $26 = node $fat3, !$fat4
      $27 = node !$25, !$26
      $28 = node $fat0, $fat2
      $29 = node !$fat0, $fat2
      $30 = node !$28, !$29
      $31 = node $fat0, !$fat3
      $32 = node !$fat0, $fat3
      $33 = node !$31, !$32
      $34 = node !$27, $fat0
      $35 = node !$fat0, $fat4
      $36 = node !$34, !$35
      $37 = node !$24, $fat0
      $38 = node !$fat0, $fat5
      $39 = node !$37, !$38
      $40 = node !$21, $fat0
      $41 = node !$fat0, $fat6
      $42 = node !$40, !$41
      $43 = node !$18, $fat0
      $44 = node !$fat0, $fat7
      $45 = node !$43, !$44
      $46 = node !$15, $fat0
      $47 = node !$fat0, $fat8
      $48 = node !$46, !$47
      $49 = node !$12, $fat0
      $50 = node !$fat0, $fat9
      $51 = node !$49, !$50
      $52 = node !$30, !$fat1
      $53 = node !$33, !$fat1
      $54 = node !$36, !$fat1
      $55 = node !$39, !$fat1
      $56 = node !$42, !$fat1
      $57 = node !$45, !$fat1
      $58 = node !$48, !$fat1
      $59 = node !$51, !$fat1
    )
    LOAD %w3843:wire(1), %en
    // this makes $fat0 in the AIG equal to wire %w3843
    aig.INPUT %4:fat_node($fat0), %w3843, %3
    LOAD %w3813:wire(1), %rst
    aig.INPUT %5:fat_node($fat1), %w3813, %3
    LOAD %w3738:wire(8), %cnt
    aig.INPUT %6:fat_node($fat2), %7:fat_node($fat3), %8:fat_node($fat4), %9:fat_node($fat5), %10:fat_node($fat6), %11:fat_node($fat7), %12:fat_node($fat8), %13:fat_node($fat9), %w3738, %3
    LOAD %w3716:wire(1), %clk
    aig.INPUT %14:fat_node($fat10), %w3716, %3
    // this makes %w3956 equal to AIG node $52 (fat node just used as bridge)
    aig.OUTPUT %w3956:wire(1), %15:fat_node($fat11, $52), %3
    aig.OUTPUT %w6:wire(1), %16:fat_node($fat12, !$1), %3
    STDCELL_INSTANCE %w3928:wire(1), %sky130_fd_sc_hd__edfxtp_1:?module("sky130_fd_sc_hd__edfxtp_1"), %w3716, %w3956, %w6
    aig.INPUT %17:fat_node($fat13), %w3928, %3
    aig.OUTPUT %w3872:wire(1), %18:fat_node($fat14, $53), %3
    STDCELL_INSTANCE %w3871:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w3872, %w6
    aig.INPUT %19:fat_node($fat15), %w3871, %3
    aig.OUTPUT %w3842:wire(1), %20:fat_node($fat16, $54), %3
    STDCELL_INSTANCE %w3814:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w3842, %w6
    aig.INPUT %21:fat_node($fat17), %w3814, %3
    aig.OUTPUT %w3756:wire(1), %22:fat_node($fat18, $55), %3
    STDCELL_INSTANCE %w3739:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w3756, %w6
    aig.INPUT %23:fat_node($fat19), %w3739, %3
    aig.OUTPUT %w3734:wire(1), %24:fat_node($fat20, $56), %3
    STDCELL_INSTANCE %w3717:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w3734, %w6
    aig.INPUT %25:fat_node($fat21), %w3717, %3
    aig.OUTPUT %w3712:wire(1), %26:fat_node($fat22, $57), %3
    STDCELL_INSTANCE %w3695:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w3712, %w6
    aig.INPUT %27:fat_node($fat23), %w3695, %3
    aig.OUTPUT %w3690:wire(1), %28:fat_node($fat24, $58), %3
    STDCELL_INSTANCE %w3673:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w3690, %w6
    aig.INPUT %29:fat_node($fat25), %w3673, %3
    aig.OUTPUT %w3671:wire(1), %30:fat_node($fat26, $59), %3
    STDCELL_INSTANCE %w3667:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w3671, %w6
    aig.INPUT %31:fat_node($fat27), %w3667, %3
    // these are identity outputs --- will be optimized out
    aig.OUTPUT %w3649:wire(8), %32:fat_node($fat28, $fat13), %33:fat_node($fat29, $fat15), %34:fat_node($fat30, $fat17), %35:fat_node($fat31, $fat19), %36:fat_node($fat32, $fat21), %37:fat_node($fat33, $fat23), %38:fat_node($fat34, $fat25), %39:fat_node($fat35, $fat27), %3
    STORE %w3649, %cnt
  }
}
```

### 11. AIG Optimization & Techmapping with ABC
Like Yosys, we use currently use ABC for AIG optimization and tech mapping. This is the final netlist after re-importing ABC results and cleaning up.
```
MODULE_DEF %Counter:module("Counter"), %0:block {
  INPUT_REGISTER_DEF %clk:register(1)
  INPUT_REGISTER_DEF %rst:register(1)
  INPUT_REGISTER_DEF %en:register(1)
  OUTPUT_REGISTER_DEF %cnt:register(8)
  // "netlist process" just means you can have cyclic wire dependencies.
  NETLIST_PROCESS_DEF %1:process, %2:block {
    LOAD %w3843:wire(1), %en
    LOAD %w3813:wire(1), %rst
    LOAD %w3716:wire(1), %clk
    STDCELL_INSTANCE %w3928:wire(1), %sky130_fd_sc_hd__edfxtp_1:?module("sky130_fd_sc_hd__edfxtp_1"), %w3716, %w1324:?wire(1), %w1315:?wire(1)
    STDCELL_INSTANCE %w3871:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w1310:?wire(1), %w1315
    STDCELL_INSTANCE %w3814:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w1306:?wire(1), %w1315
    STDCELL_INSTANCE %w3739:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w1303:?wire(1), %w1315
    STDCELL_INSTANCE %w3717:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w1293:?wire(1), %w1315
    STDCELL_INSTANCE %w3695:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w1290:?wire(1), %w1315
    STDCELL_INSTANCE %w3673:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w1285:?wire(1), %w1315
    STDCELL_INSTANCE %w3667:wire(1), %sky130_fd_sc_hd__edfxtp_1, %w3716, %w1282:?wire(1), %w1315
    CONCAT %w22:wire(8), %w3667, %w3673, %w3695, %w3717, %w3739, %w3814, %w3871, %w3928
    STORE %w22, %cnt
    STDCELL_INSTANCE %w4:wire(1), %sky130_fd_sc_hd__inv_1:?module("sky130_fd_sc_hd__inv_1"), %w3717
    STDCELL_INSTANCE %w3:wire(1), %sky130_fd_sc_hd__inv_1, %w3695
    STDCELL_INSTANCE %w1324:wire(1), %sky130_fd_sc_hd__nor2b_1:?module("sky130_fd_sc_hd__nor2b_1"), %w3813, %w3928
    STDCELL_INSTANCE %w1315:wire(1), %sky130_fd_sc_hd__or2_0:?module("sky130_fd_sc_hd__or2_0"), %w3843, %w3813
    STDCELL_INSTANCE %w2:wire(1), %sky130_fd_sc_hd__o21bai_1:?module("sky130_fd_sc_hd__o21bai_1"), %w3843, %w3871, %w3813
    STDCELL_INSTANCE %w1310:wire(1), %sky130_fd_sc_hd__a21oi_1:?module("sky130_fd_sc_hd__a21oi_1"), %w3843, %w3871, %w2
    STDCELL_INSTANCE %w1346:wire(1), %sky130_fd_sc_hd__nand2_1:?module("sky130_fd_sc_hd__nand2_1"), %w3871, %w3814
    STDCELL_INSTANCE %w1344:wire(1), %sky130_fd_sc_hd__nand3_1:?module("sky130_fd_sc_hd__nand3_1"), %w3843, %w3871, %w3814
    STDCELL_INSTANCE %w1343:wire(1), %sky130_fd_sc_hd__a21oi_1, %w3843, %w3871, %w3814
    STDCELL_INSTANCE %w1340:wire(1), %sky130_fd_sc_hd__nor2_1:?module("sky130_fd_sc_hd__nor2_1"), %w3813, %w1343
    STDCELL_INSTANCE %w1306:wire(1), %sky130_fd_sc_hd__and2_0:?module("sky130_fd_sc_hd__and2_0"), %w1344, %w1340
    STDCELL_INSTANCE %w1339:wire(1), %sky130_fd_sc_hd__a21oi_1, %w3843, %w1346, %w3739
    STDCELL_INSTANCE %w1337:wire(1), %sky130_fd_sc_hd__nand2_1, %w3843, %w3739
    STDCELL_INSTANCE %w1335:wire(1), %sky130_fd_sc_hd__a21oi_1, %w3871, %w3814, %w1337
    STDCELL_INSTANCE %w1303:wire(1), %sky130_fd_sc_hd__nor3_1:?module("sky130_fd_sc_hd__nor3_1"), %w3813, %w1339, %w1335
    STDCELL_INSTANCE %w1331:wire(1), %sky130_fd_sc_hd__a21oi_1, %w1344, %w1337, %w4
    STDCELL_INSTANCE %w1329:wire(1), %sky130_fd_sc_hd__and3_1:?module("sky130_fd_sc_hd__and3_1"), %w4, %w1344, %w1337
    STDCELL_INSTANCE %w1293:wire(1), %sky130_fd_sc_hd__nor3_1, %w3813, %w1331, %w1329
    STDCELL_INSTANCE %w1325:wire(1), %sky130_fd_sc_hd__a211oi_1:?module("sky130_fd_sc_hd__a211oi_1"), %w1344, %w1337, %w4, %w3
    STDCELL_INSTANCE %w1323:wire(1), %sky130_fd_sc_hd__nor2_1, %w3695, %w1331
    STDCELL_INSTANCE %w1290:wire(1), %sky130_fd_sc_hd__nor3_1, %w3813, %w1325, %w1323
    STDCELL_INSTANCE %w1322:wire(1), %sky130_fd_sc_hd__o21bai_1, %w3673, %w1325, %w3813
    STDCELL_INSTANCE %w1285:wire(1), %sky130_fd_sc_hd__a21oi_1, %w3673, %w1325, %w1322
    STDCELL_INSTANCE %w1321:wire(1), %sky130_fd_sc_hd__and3_1, %w3673, %w3667, %w1325
    STDCELL_INSTANCE %w1318:wire(1), %sky130_fd_sc_hd__a21oi_1, %w3673, %w1325, %w3667
    STDCELL_INSTANCE %w1282:wire(1), %sky130_fd_sc_hd__nor3_1, %w3813, %w1321, %w1318
  }
}
```

### 12. Final Verilog Netlist Dump
The same netlist back in Verilog.
```verilog
module Counter(
input wire clk,
input wire rst,
input wire en,
output wire cnt
);
wire _w5_;
wire _w27_;
wire _w18_;
wire _w37_;
wire _w34_;
wire _w30_;
wire _w26_;
wire _w21_;
wire _w17_;
wire _w11_;
wire _w6_;
wire _w2_;
wire _w32_;
wire _w33_;
wire _w29_;
wire _w24_;
wire _w36_;
wire _w25_;
wire _w20_;
wire _w19_;
wire _w13_;
wire _w12_;
wire _w16_;
wire _w10_;
wire _w9_;
wire _w8_;
wire _w14_;
wire _w31_;
wire _w28_;
wire _w23_;
wire _w3_;
wire _w22_;
wire _w15_;
wire _w0_;
wire _w7_;
wire _w1_;
wire _w4_;
wire _w35_;
assign _w0_ = rst;
assign _w1_ = en;
sky130_fd_sc_hd__or2_0 _inst0_ (.A(_w1_), .B(_w0_), .X(_w2_));
assign _w3_ = clk;
sky130_fd_sc_hd__o21bai_1 _inst1_ (.A1(_w1_), .A2(_w4_), .B1_N(_w0_), .Y(_w5_));
sky130_fd_sc_hd__a21oi_1 _inst2_ (.A1(_w1_), .A2(_w4_), .B1(_w5_), .Y(_w6_));
sky130_fd_sc_hd__edfxtp_1 _inst3_ (.CLK(_w3_), .D(_w6_), .DE(_w2_), .Q(_w4_));
sky130_fd_sc_hd__nand3_1 _inst4_ (.A(_w1_), .B(_w4_), .C(_w7_), .Y(_w8_));
sky130_fd_sc_hd__a21oi_1 _inst5_ (.A1(_w1_), .A2(_w4_), .B1(_w7_), .Y(_w9_));
sky130_fd_sc_hd__nor2_1 _inst6_ (.A(_w0_), .B(_w9_), .Y(_w10_));
sky130_fd_sc_hd__and2_0 _inst7_ (.A(_w8_), .B(_w10_), .X(_w11_));
sky130_fd_sc_hd__edfxtp_1 _inst8_ (.CLK(_w3_), .D(_w11_), .DE(_w2_), .Q(_w7_));
sky130_fd_sc_hd__a21oi_1 _inst9_ (.A1(_w4_), .A2(_w7_), .B1(_w12_), .Y(_w13_));
sky130_fd_sc_hd__nand2_1 _inst10_ (.A(_w4_), .B(_w7_), .Y(_w14_));
sky130_fd_sc_hd__a21oi_1 _inst11_ (.A1(_w1_), .A2(_w14_), .B1(_w15_), .Y(_w16_));
sky130_fd_sc_hd__nor3_1 _inst12_ (.A(_w0_), .B(_w16_), .C(_w13_), .Y(_w17_));
sky130_fd_sc_hd__edfxtp_1 _inst13_ (.CLK(_w3_), .D(_w17_), .DE(_w2_), .Q(_w15_));
sky130_fd_sc_hd__nand2_1 _inst14_ (.A(_w1_), .B(_w15_), .Y(_w12_));
sky130_fd_sc_hd__a21oi_1 _inst15_ (.A1(_w8_), .A2(_w12_), .B1(_w18_), .Y(_w19_));
sky130_fd_sc_hd__and3_1 _inst16_ (.A(_w18_), .B(_w8_), .C(_w12_), .X(_w20_));
sky130_fd_sc_hd__nor3_1 _inst17_ (.A(_w0_), .B(_w19_), .C(_w20_), .Y(_w21_));
sky130_fd_sc_hd__edfxtp_1 _inst18_ (.CLK(_w3_), .D(_w21_), .DE(_w2_), .Q(_w22_));
sky130_fd_sc_hd__inv_1 _inst19_ (.A(_w22_), .Y(_w18_));
sky130_fd_sc_hd__nor2_1 _inst20_ (.A(_w23_), .B(_w19_), .Y(_w24_));
sky130_fd_sc_hd__nor3_1 _inst21_ (.A(_w0_), .B(_w25_), .C(_w24_), .Y(_w26_));
sky130_fd_sc_hd__edfxtp_1 _inst22_ (.CLK(_w3_), .D(_w26_), .DE(_w2_), .Q(_w23_));
sky130_fd_sc_hd__inv_1 _inst23_ (.A(_w23_), .Y(_w27_));
sky130_fd_sc_hd__a211oi_1 _inst24_ (.A1(_w8_), .A2(_w12_), .B1(_w18_), .C1(_w27_), .Y(_w25_));
sky130_fd_sc_hd__o21bai_1 _inst25_ (.A1(_w28_), .A2(_w25_), .B1_N(_w0_), .Y(_w29_));
sky130_fd_sc_hd__a21oi_1 _inst26_ (.A1(_w28_), .A2(_w25_), .B1(_w29_), .Y(_w30_));
sky130_fd_sc_hd__edfxtp_1 _inst27_ (.CLK(_w3_), .D(_w30_), .DE(_w2_), .Q(_w28_));
sky130_fd_sc_hd__a21oi_1 _inst28_ (.A1(_w28_), .A2(_w25_), .B1(_w31_), .Y(_w32_));
sky130_fd_sc_hd__and3_1 _inst29_ (.A(_w28_), .B(_w31_), .C(_w25_), .X(_w33_));
sky130_fd_sc_hd__nor3_1 _inst30_ (.A(_w0_), .B(_w33_), .C(_w32_), .Y(_w34_));
sky130_fd_sc_hd__edfxtp_1 _inst31_ (.CLK(_w3_), .D(_w34_), .DE(_w2_), .Q(_w31_));
sky130_fd_sc_hd__nor2b_1 _inst32_ (.A(_w0_), .B_N(_w35_), .Y(_w36_));
sky130_fd_sc_hd__edfxtp_1 _inst33_ (.CLK(_w3_), .D(_w36_), .DE(_w2_), .Q(_w35_));
assign _w37_ = {_w31_, _w28_, _w23_, _w22_, _w15_, _w7_, _w4_, _w35_};
assign cnt = _w37_;
endmodule
```
