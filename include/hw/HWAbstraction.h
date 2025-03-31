#pragma once
#include "dyno/CFG.h"
#include "dyno/Constant.h"
#include "dyno/Instr.h"
#include "dyno/InstrPrinter.h"
#include "dyno/Obj.h"
#include "dyno/ObjInfo.h"
#include "hw/IDs.h"
#include "hw/Module.h"
#include "hw/ObjInfo.h"
#include "hw/Process.h"
#include "hw/Register.h"
#include "hw/Wire.h"
#include <dyno/NewDeleteObjStore.h>
#include <iostream>
#include <optional>

namespace dyno {

class HWContext {

  NewDeleteObjStore<Module> modules;
  NewDeleteObjStore<Register> regs;
  NewDeleteObjStore<Wire> wires;
  NewDeleteObjStore<Process> procs;
  CFG cfg;
  NewDeleteObjStore<Instr> instrs;
  // todo: processes & modules

public:
  auto &getModules() { return modules; }
  auto &getRegs() { return regs; }
  auto &getWires() { return wires; }
  auto &getProcs() { return procs; }
  auto &getInstrs() { return instrs; }
  auto &getCFG() { return cfg; }

  ModuleRef createModule(std::string_view name) {
    auto moduleRef = modules.create(std::string(name));
    auto moduleInstr = InstrRef{
        instrs.create(1, DialectID{DIALECT_RTL}, OpcodeID{HW_MODULE_INSTR})};

    InstrBuilder{moduleInstr}.addRef(moduleRef);

    return moduleRef;
  }

  RegisterRef createRegister(ModuleRef parent) {
    auto regRef = RegisterRef{regs.create()};
    // in order for the reg to use anything it must be an instr as well
    auto regInstr = InstrRef{instrs.create(2, DialectID{DIALECT_RTL}, OpcodeID{HW_REGISTER_INSTR})};
    InstrBuilder{regInstr}.addRef(regRef).other().addRef(parent);

    return regRef;
  }

  ProcessRef createProcess(ModuleRef parent) {
    auto blockRef = cfg.blocks.create(cfg);
    auto blockInstrRef = InstrRef{
        instrs.create(2, DialectID{DIALECT_RTL}, OpcodeID{HW_BLOCK_INSTR})};
    InstrBuilder blockInstrBuild{blockInstrRef};

    auto procRef = procs.create();

    auto procInstRef = InstrRef{
        instrs.create(2, DialectID{DIALECT_RTL}, OpcodeID{HW_PROCESS_INSTR})};
    InstrBuilder{procInstRef}.addRef(procRef).other().addRef(parent);
    blockInstrBuild.addRef(blockRef).other().addRef(procRef);
    return procRef;
  }
};

class HWInstrRef : public InstrRef {
public:
  using InstrRef::InstrRef;
  HWInstrRef(const InstrRef &ref) : InstrRef(ref) {}

  WireRef operandW(uint n) { return operand(n)->as<WireRef>(); }
  InstrRef operandI(uint n) { return operandW(n).getDefI(); }
  WireRef defW(uint n = 0) {
    assert(n < getNumDefs());
    return operandW(n);
  }
  InstrRef defI(uint n = 0) {
    assert(n < getNumDefs());
    return operandI(n);
  }
  // todo: get rid of ctx params via global directory.
  auto iter(HWContext &ctx) { return ctx.getCFG()[this->as<ObjRef<Instr>>()]; }
  BlockRef parentBlock(HWContext &ctx) { return iter(ctx).blockRef(); }
  ProcessRef parentProc(HWContext &ctx) {
    auto pBlock = parentBlock(ctx);
    return pBlock.parent().as<ProcessRef>();
  }
};

class HWInstrBuilder {
  HWContext &ctx;
  BlockRef_iterator<true> insert;

public:
  HWInstrBuilder(HWContext &ctx, BlockRef_iterator<true> insert)
      : ctx(ctx), insert(insert) {}

  void insertInstr(InstrRef instr) { insert.insertPrev(instr); }

  template <typename... Ts> void addRefs(InstrRef instr, Ts... operands) {
    auto defWire = ctx.getWires().create();
    InstrBuilder build{instr};
    build.addRef(defWire).other();
    ([&] { build.addRef(operands); }(), ...);
  }

  template <typename... Ts>
  HWInstrRef buildInstr(OpcodeID opcode, Ts... operands) {
    auto instr = InstrRef{ctx.getInstrs().create(
        1 + sizeof...(operands), DialectID{DIALECT_RTL}, opcode)};

    insertInstr(instr);
    addRefs(instr, operands...);
    return HWInstrRef{instr};
  }

  template <typename... Ts> HWInstrRef buildAdd(Ts... operands) {
    return buildInstr(OpcodeID{HW_ADD}, operands...);
  }

  template <typename LHS, typename RHS> HWInstrRef buildSub(LHS lhs, RHS rhs) {
    return buildInstr(OpcodeID{HW_SUB}, lhs, rhs);
  }

  HWInstrRef buildLoad(RegisterRef reg) {
    return buildInstr(OpcodeID{HW_LOAD}, reg);
  }

  HWInstrRef buildStore(RegisterRef reg, FatObjRef<Wire> value) {
    return buildInstr(OpcodeID{HW_STORE}, reg, value);
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

    refPrinter.reset();

    for (auto mod : ctx.getModules()) {
      auto moduleRef = ModuleRef{mod};
      std::cout << "module(" << mod.getObjID() << ", " << mod->name << "):\n";

      for (auto reg : moduleRef.regs())
      {
        refPrinter.introduceRef(reg.instr().def()->as<FatDynObjRef<>>());
      }

      for (auto proc : moduleRef.procs()) {
        auto procRef = proc.instr().def()->as<ProcessRef>();
        std::cout << "proc(" << procRef.getObjID() << "):\n";
        for (auto block : procRef.blocks()) {
          auto blockRef = block.instr().def()->as<BlockRef>();
          std::cout << "block(" << blockRef.getObjID() << "):\n";

          for (auto insn = blockRef.begin(); insn != blockRef.end(); insn++) {
            instrPrinter.print(*insn);
          }
        }
      }
    }
  }
};

}; // namespace dyno
