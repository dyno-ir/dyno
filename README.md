# Dyno-IR
Dyno-IR is a toolkit for building high performance compiler IRs with powerful generic infrastructure, but swappable core data structures. This is a monorepo for all Dyno-IR applications.

## Building
```bash
cmake --preset debug # Debug build
cmake --preset release # Release build
```
We currently only test with `clang` + `libc++`, errors might occur with other toolchains.

# Dyno-SV
Dyno-SV is a Dyno-IR based SystemVerilog synthesis tool.

## Example Usage
```
dyno-sv --liberty=sky130_fd_sc_hd.lib MyDesign.v -o=MyGateLevelNetlist.v
```

