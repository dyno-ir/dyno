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
#include "support/Utility.h"
#include <dyno/NewDeleteObjStore.h>
#include <iostream>

namespace dyno {

class HWContext {

  ConstantStore constants;
  NewDeleteObjStore<Module> modules;
  NewDeleteObjStore<Register> regs;
  NewDeleteObjStore<SCFConstruct> scfConstrs;
  NewDeleteObjStore<Wire> wires;
  NewDeleteObjStore<Process> procs;
  CFG cfg;
  NewDeleteObjStore<Instr> instrs;
  // todo: processes & modules

public:
  auto &getConstants() { return constants; }
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

    OpcodeID opcode = OpcodeID{SCF_YIELD};

    auto instr = scfConstr.getDef().instr();
    switch (instr.getOpcode()) {
    case SCF_IF: {
      if (sizeof...(value) >= instr.getNumDefs()) {
        auto newInstr = InstrRef{ctx.getInstrs().create(
            sizeof...(value) + 4, DialectID{DIALECT_SCF}, OpcodeID{SCF_IF})};

        InstrBuilder build{newInstr};
        for (uint i = 1; i < instr.getNumDefs(); i++)
          build.addRef(instr.operand(i)->as<FatDynObjRef<>>());

        for (uint i = 0; i < sizeof...(value) - instr.getNumDefs() + 1; i++)
          build.addRef(scfConstr).addRef(ctx.getWires().create());

        build.other();
        for (uint i = instr.getNumDefs(); i < instr.getNumOperands(); i++)
          build.addRef(instr.operand(i)->as<FatDynObjRef<>>());

        HWInstrRef{instr}.iter(ctx).replace(newInstr);
        ctx.getInstrs().destroy(instr);
        instr = newInstr;
      }
      break;
    }
    case SCF_WHILE: {
      WhileInstrRef asWhile{instr};
      // conditional yield is just implicit for now, one more arg
      if (sizeof...(Ts) == asWhile.getNumYieldValues() + 1)
        opcode = OpcodeID{SCF_YIELD_COND};
      else {
        assert(sizeof...(Ts) == asWhile.getNumYieldValues());
      }
      break;
    }
    default:
      dyno_unreachable("undefined");
    }

    auto yieldInstr = buildInstr(DialectID{DIALECT_SCF}, opcode,
                                 false, scfConstr, value...);
    return std::make_pair(yieldInstr, instr);
  }

  template <typename... Ts> auto buildWhile(Ts... inputs) {
    SCFConstructRef scfConstr{ctx.getSCFConstrs().create()};
    InstrRef instrRef = InstrRef{
        ctx.getInstrs().create(3 + 2 * sizeof...(inputs),
                               DialectID{DIALECT_SCF}, OpcodeID{SCF_WHILE})};
    insertInstr(instrRef);

    InstrBuilder build{instrRef};
    build.addRef(scfConstr);
    for (uint i = 0; i < sizeof...(inputs); i++)
      build.addRef(ctx.getWires().create());
    // todo: copy over all parents for nested scf constructs?
    build.other();
    build.addRef(ctx.createBlock(insert.blockRef().parent(), scfConstr));
    build.addRef(ctx.createBlock(insert.blockRef().parent(), scfConstr));
    ([&]() { build.addRef(inputs); }(), ...);

    return WhileInstrRef{instrRef};
  }

  // todo: full constant support
  ConstantRef buildConst(uint bits, uint64_t value) {
    // still quite problematic, this constant is never deleted.
    // Could do unique_ptr or shared_ptr style implementation for safety.
    return ConstantBuilder{ctx.getConstants()}.build(bits, value);
  }

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
      std::cout << "module(" << mod.getObjID() << ", " << mod->name << ",\n";
      for (auto port : moduleRef->ports) {
        switch (RegisterRef{port}.getPortType()) {
        case Register::PORT_IN:
          std::cout << "in: ";
          break;
        case Register::PORT_OUT:
          std::cout << "out: ";
          break;
        case Register::PORT_INOUT:
          std::cout << "io: ";
          break;
        case Register::PORT_PARAM_IN:
          std::cout << "param: ";
          break;
        case Register::PORT_NONE:
          unreachable();
        }

        refPrinter.introduceRef(port);
      }
      std::cout << "):\n";

      for (auto reg : moduleRef.regs()) {
        auto asRegRef = reg.instr().def()->as<RegisterRef>();
        if (!asRegRef.isPort())
          refPrinter.introduceRef(asRegRef);
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
