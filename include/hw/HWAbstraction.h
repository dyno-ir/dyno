#pragma once
#include "dyno/CFG.h"
#include "dyno/Constant.h"
#include "dyno/Instr.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Obj.h"
#include "dyno/ObjInfo.h"
#include "hw/IDs.h"
#include "hw/ObjInfo.h"
#include "hw/Process.h"
#include "hw/Wire.h"
#include <dyno/NewDeleteObjStore.h>
#include <iostream>
#include <optional>

namespace dyno {

class HWContext {

  NewDeleteObjStore<Wire> wires;
  NewDeleteObjStore<Process> procs;
  CFG cfg;
  NewDeleteObjStore<Instr> instrs;
  // todo: processes & modules

public:
  auto &getWires() { return wires; }
  auto &getProcs() { return procs; }
  auto &getInstrs() { return instrs; }
  auto &getCFG() { return cfg; }

  ProcessRef createProcess() {
    auto blockRef = cfg.blocks.create(cfg);
    auto blockInstrRef =
        InstrRef{instrs.create(2, DialectID{DIALECT_RTL}, OpcodeID{HW_BLOCK_INSTR})};
    InstrBuilder blockInstrBuild{blockInstrRef};

    auto procRef = procs.create();

    auto procInstRef =
        InstrRef{instrs.create(2, DialectID{DIALECT_RTL}, OpcodeID{HW_PROCESS_INSTR})};
    InstrBuilder{procInstRef}.addRef(procRef).other().addRef(blockRef);
    blockInstrBuild.addRef(blockRef).other().addRef(procRef);
    return procRef;
  }
};

class HWInstrBuilder {
  HWContext &ctx;
  BlockRef_iterator<true> insert;

public:
  HWInstrBuilder(HWContext &ctx, BlockRef_iterator<true> insert)
      : ctx(ctx), insert(insert) {}

  void insertInstr(InstrRef instr) {
    insert.insertPrev(instr);
  }

  template <typename... Ts> void addRefs(InstrRef instr, Ts... operands) {
    auto defWire = ctx.getWires().create();
    InstrBuilder build{instr};
    build.addRef(defWire).other();
    ([&] { build.addRef(operands); }(), ...);
  }

  template <typename... Ts>
  InstrRef buildInstr(OpcodeID opcode, Ts... operands) {
    auto instr = InstrRef{ctx.getInstrs().create(
        1 + sizeof...(operands), DialectID{DIALECT_RTL}, opcode)};

    insertInstr(instr);
    addRefs(instr, operands...);
    return instr;
  }

  template <typename... Ts> InstrRef buildAdd(Ts... operands) {
    return buildInstr(OpcodeID{HW_ADD}, operands...);
  }

  template <typename LHS, typename RHS> InstrRef buildSub(LHS lhs, RHS rhs) {
    return buildInstr(OpcodeID{HW_SUB}, lhs, rhs);
  }

  // todo: full constant support
  ConstantRef buildConst32(uint32_t value) { return ConstantRef{32, value}; }
};

class HWPrinter {
  // todo: better spot for these
  std::array<const DialectInfo *, 2> dialectIs{&coreDialectInfo,
                                               &rtlDialectInfo};
  std::array<const TyInfo *, 2> tyIs{coreTyInfo, rtlTyInfo};
  std::array<const OpcodeInfo *, 2> opcodeIs{coreOpcodeInfo, rtlOpcodeInfo};

  Interface<DialectInfo> dialectI{dialectIs.data()};
  Interface<TyInfo> tyI{tyIs.data()};
  Interface<OpcodeInfo> opcI{opcodeIs.data()};

  RefPrinter refPrinter{std::cout, dialectI, tyI};
  InstrPrinter instrPrinter{refPrinter, opcI};

public:
  void printCtx(HWContext &ctx) {
    std::cout << "raw instr dump:\n";
    for (auto instr : ctx.getInstrs()) {
        instrPrinter.print(InstrRef{instr});
    }
    std::cout << "\nstructured dump:\n";
    for (auto proc : ctx.getProcs()) {
      std::cout << "proc(" << proc.getObjID() << "):\n";
      for (auto block : proc->blocks()) {
        std::cout << "block(" << block.instr().def()->fat().as<FatObjRef<Block>>().getObjID() << "):\n";
        auto asBlockRef = BlockRef{block.instr().def()->fat().as<FatObjRef<Block>>()};
        for (auto insn : asBlockRef) {
          // todo: better fix for null InstrRef in block
          if (insn.getPtr() == nullptr)
            break;
          instrPrinter.print(insn);
        }
      }
    }
  }
};

}; // namespace dyno
