# Dyno-IR
Dyno-IR is a toolkit for building high performance compiler IRs with powerful generic infrastructure, but swappable core data structures. This is a monorepo for all Dyno-IR applications.

## Building
```bash
git clone --recursive git@github.com:dyno-ir/dyno.git
# Debug configuration
cmake --preset debug
cmake --build build/debug
# Release configuration
cmake --preset release
cmake --build build/release
```
We currently only test with `clang` + `libc++`, errors might occur with other toolchains.

# Dyno-SV
Dyno-SV is a Dyno-IR based SystemVerilog synthesis tool.

## Example Usage
```
dyno-sv --liberty=sky130_fd_sc_hd.lib MyDesign.v -o=MyGateLevelNetlist.v
```
## Publications
ORConf25: [Dyno-SV: An IR-Driven Open Source RTL Synthesis Tool](https://www.youtube.com/watch?v=H-sggJKpQWE)
