#pragma once
#include "dyno/CFG.h"
#include "dyno/Constant.h"
#include "dyno/IDs.h"
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
#include "scf/IDs.h"
#include "scf/ObjInfo.h"
#include "scf/SCF.h"
#include <dyno/NewDeleteObjStore.h>
#include <iostream>

namespace dyno {

class HWContext {

  NewDeleteObjStore<Module> modules;
  NewDeleteObjStore<Register> regs;
  NewDeleteObjStore<SCFConstruct> scfConstrs;
  NewDeleteObjStore<Wire> wires;
  NewDeleteObjStore<Process> procs;
  CFG cfg;
  NewDeleteObjStore<Instr> instrs;
  // todo: processes & modules

public:
  auto &getModules() { return modules; }
  auto &getRegs() { return regs; }
  auto &getSCFConstrs() { return scfConstrs; }
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
    auto regInstr = InstrRef{
        instrs.create(2, DialectID{DIALECT_RTL}, OpcodeID{HW_REGISTER_INSTR})};
    InstrBuilder{regInstr}.addRef(regRef).other().addRef(parent);

    return regRef;
  }

  template <typename... Ts> BlockRef createBlock(Ts... parents) {
    auto blockRef = cfg.blocks.create(cfg);
    auto blockInstrRef =
        InstrRef{instrs.create(1 + sizeof...(parents), DialectID{DIALECT_RTL},
                               OpcodeID{HW_BLOCK_INSTR})};
    InstrBuilder build{blockInstrRef};
    build.addRef(blockRef).other();
    (([&]() { build.addRef(parents); })(), ...);
    return blockRef;
  }

  ProcessRef createProcess(ModuleRef parent) {
    auto procRef = procs.create();
    auto procInstRef = InstrRef{
        instrs.create(2, DialectID{DIALECT_RTL}, OpcodeID{HW_PROCESS_INSTR})};
    InstrBuilder{procInstRef}.addRef(procRef).other().addRef(parent);
    createBlock(procRef);
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

  template <typename... Ts>
  void addRefs(InstrRef instr, bool addWireDef, Ts... operands) {
    InstrBuilder build{instr};
    if (addWireDef) {
      auto defWire = ctx.getWires().create();
      build.addRef(defWire);
    }
    build.other();
    ([&] { build.addRef(operands); }(), ...);
  }

  template <typename... Ts>
  HWInstrRef buildInstr(DialectID dialect, OpcodeID opcode, bool addWireDef,
                        Ts... operands) {
    auto instr = InstrRef{ctx.getInstrs().create(
        addWireDef + sizeof...(operands), dialect, opcode)};

    insertInstr(instr);
    addRefs(instr, addWireDef, operands...);
    return HWInstrRef{instr};
  }

  template <typename... Ts> HWInstrRef buildAdd(Ts... operands) {
    return buildInstr(DialectID{DIALECT_RTL}, OpcodeID{HW_ADD}, true,
                      operands...);
  }

  template <typename LHS, typename RHS> HWInstrRef buildSub(LHS lhs, RHS rhs) {
    return buildInstr(DialectID{DIALECT_RTL}, OpcodeID{HW_SUB}, true, lhs, rhs);
  }

  HWInstrRef buildLoad(RegisterRef reg) {
    return buildInstr(DialectID{DIALECT_RTL}, OpcodeID{HW_LOAD}, true, reg);
  }

  HWInstrRef buildStore(RegisterRef reg, FatDynObjRef<> value) {
    return buildInstr(DialectID{DIALECT_RTL}, OpcodeID{HW_STORE}, false, reg,
                      value);
  }

  IfInstrRef buildIfElse(FatDynObjRef<> cond) {
    SCFConstructRef scfConstr{ctx.getSCFConstrs().create()};
    InstrRef instrRef = InstrRef{
        ctx.getInstrs().create(4, DialectID{DIALECT_SCF}, OpcodeID{SCF_IF})};
    insertInstr(instrRef);

    InstrBuilder build{instrRef};
    build.addRef(scfConstr).other().addRef(cond);

    auto trueBl = ctx.createBlock(insert.blockRef().parent(), scfConstr);
    auto falseBl = ctx.createBlock(insert.blockRef().parent(), scfConstr);

    build.addRef(trueBl).addRef(falseBl);
    return IfInstrRef{instrRef};
  }

  template <typename... Ts>
  auto buildSCFYield(SCFConstructRef scfConstr, Ts... value) {
    // todo: ideally this would dynamically add Wires to the associated
    // IfInstrRef. Alternative is a GET_YIELD instr or something, but then we
    // end up with 3 categories in the SCFConstruct vreg use list (YIELD,
    // GET_YIELD and block). For now naive implementation as reference, just
    // delete old instr and rebuild
    auto ifInstr = scfConstr.getDef().instr();

    if (sizeof...(value) >= ifInstr.getNumDefs()) {
      auto newInstr = InstrRef{
          ctx.getInstrs().create(sizeof...(value) + 4,
                                 DialectID{DIALECT_SCF}, OpcodeID{SCF_IF})};

      InstrBuilder build{newInstr};
      for (uint i = 1; i < ifInstr.getNumDefs(); i++)
        build.addRef(ifInstr.operand(i)->as<FatDynObjRef<>>());

      for (uint i = 0; i < sizeof...(value) - ifInstr.getNumDefs() + 1; i++)
        build.addRef(scfConstr).addRef(ctx.getWires().create());

      build.other();
      for (uint i = ifInstr.getNumDefs(); i < ifInstr.getNumOperands(); i++)
        build.addRef(ifInstr.operand(i)->as<FatDynObjRef<>>());

      HWInstrRef{ifInstr}.iter(ctx).replace(newInstr);
      ctx.getInstrs().destroy(ifInstr);
      ifInstr = newInstr;
    }

    auto yieldInstr = buildInstr(DialectID{DIALECT_SCF}, OpcodeID{SCF_YIELD},
                                 false, scfConstr, value...);
    return std::make_pair(yieldInstr, ifInstr);
  }

  // todo: full constant support
  ConstantRef buildConst32(uint32_t value) { return ConstantRef{32, value}; }

  void setInsertPoint(BlockRef_iterator<true> it) { insert = it; }
};

class HWPrinter {
  // todo: better spot for these
  std::array<const DialectInfo *, 3> dialectIs{
      &coreDialectInfo, &scfDialectInfo, &rtlDialectInfo};
  std::array<const TyInfo *, 3> tyIs{coreTyInfo, scfTyInfo, rtlTyInfo};
  std::array<const OpcodeInfo *, 3> opcodeIs{coreOpcodeInfo, scfOpcodeInfo,
                                             rtlOpcodeInfo};

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

      for (auto reg : moduleRef.regs()) {
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
