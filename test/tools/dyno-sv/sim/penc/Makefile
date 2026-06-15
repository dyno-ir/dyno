DYNO_ROOT ?= ../../../../..
BUILD     := $(DYNO_ROOT)/build
DYNO_SV   := $(BUILD)/tools/dyno-sv/dyno-sv
INCLUDE   := $(DYNO_ROOT)/include
LIBHW     := $(BUILD)/lib/libhw.a
LIBCORE   := $(BUILD)/lib/libcore.a

CXX       := clang++
CXXFLAGS  := -std=c++23 -Wall -Wextra -Wno-unused-parameter -Wno-unused-function -fno-rtti -I$(INCLUDE) -O2
LDFLAGS   :=

BIN       := sim

.PHONY: synth sim run clean

synth:
	$(DYNO_SV) synth -l="" -s=../sim_flow.dyno PriorityEncoder.sv

sim: Top_tb.cpp synth
	$(CXX) $(CXXFLAGS) -o $@ $< $(LIBHW) $(LIBCORE) $(LDFLAGS)

run: sim
	./sim

clean:
	rm -f $(BIN) design.dyno sim_header.h
